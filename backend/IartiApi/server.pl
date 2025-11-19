% Bibliotecas 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/http_cors)).

% Carregar US 3.4.2
:- consult('PrologCode/US 3.4.2.pl').
% Carregar US 3.4.4
:- consult('PrologCode/US 3.4.4.pl').
% Carregar US 3.4.5
:- consult('PrologCode/US 3.4.5.pl').

:- http_handler('/lapr5', responde_ola, []).
:- http_handler('/register_user', register_user, []).
:- http_handler('/send_file_post', send_file_post, []).
:- http_handler('/shortest_delay', handle_shortest_delay, []).
:- http_handler('/heuristic_schedule', handle_heuristic_schedule, []).
:- http_handler('/multi_crane_schedule', handle_multi_crane_schedule, []).

% Enable CORS for all routes
:- set_setting(http:cors, [*]).

server(Port) :-						
        http_server(http_dispatch, [port(Port)]).

% Tratamento de 'http://localhost:5000/lapr5'
responde_ola(_Request) :-					
        format('Content-type: text/plain~n~n'),
        format('Olá LAPR5!~n').

% M�TODO POST
% http_client:http_post('http://localhost:5000/register_user', form_data([name='Jos�', sex=male, birth_year=1975]), Reply, []).

register_user(Request) :-
    http_parameters(Request,
                    [ name(Name, []),
                      sex(Sex, [oneof([male,female])]),
                      birth_year(BY, [between(1850,10000)])
                    ]),
    format('Content-type: text/plain~n~n'),
    format('User registered!~n'),
	format('Name: ~w~nSex: ~w~nBirth Year: ~w~n',[Name,Sex,BY]).

% M�TODO POST enviando um ficheiro de texto
% http_client:http_post('http://localhost:5000/send_file_post', form_data([file=file('./teste.txt')]), Reply, []).

send_file_post(Request) :-
	http_parameters(Request,[ file(X,[])]),
    format('Content-type: text/plain~n~n'),
	format('Received: ~w~n',[X]).

% ============================================
% US 3.4.2 - Shortest Delay Endpoint
% ============================================
% GET http://localhost:5003/shortest_delay?date=2025-11-09&format=json
% GET http://localhost:5003/shortest_delay?date=2025-11-09&format=text (default)

% Main handler that dispatches based on HTTP method
handle_shortest_delay(Request) :-
    memberchk(method(Method), Request),
    !,
    (   Method = options
    ->  handle_cors_preflight(Request)
    ;   Method = get
    ->  get_shortest_delay(Request)
    ;   % Unsupported method
        format('Status: 405~n'),
        format('Content-type: text/plain~n~n'),
        format('Method not allowed~n')
    ).

% Handle CORS preflight OPTIONS request
handle_cors_preflight(_Request) :-
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type, Authorization~n'),
    format('Access-Control-Max-Age: 86400~n'),
    format('Status: 204~n'),
    format('~n').

get_shortest_delay(Request) :-
    % Send CORS headers first - ALWAYS send these regardless of errors
    catch(
        get_shortest_delay_impl(Request),
        Error,
        (format(user_error, '[FATAL ERROR] Unhandled exception in get_shortest_delay: ~w~n', [Error]),
         format('Content-type: application/json~n~n'),
         format('{"error": "Internal server error", "details": "~w"}~n', [Error]))
    ).

get_shortest_delay_impl(Request) :-
    % Send CORS headers
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type, Authorization~n'),
    
    % Read query parameters
    http_parameters(Request,
                    [ date(Date, [optional(true), default('2025-11-09')]),
                      format(Format, [optional(true), default('text'), oneof([json, text])])
                    ]),

    % Validate that the date is not in the past
    get_time(Now),
    stamp_date_time(Now, date(YearNow, MonthNow, DayNow, _, _, _, _, _, _), local),
    (   atom(Date) -> atom_string(Date, DateStr) ; DateStr = Date ),
    % Parse the requested date (format: YYYY-MM-DD)
    split_string(DateStr, "-", "", [YearStr, MonthStr, DayStr]),
    atom_number(YearStr, Year),
    atom_number(MonthStr, Month),
    atom_number(DayStr, Day),
    % Compare dates
    (   (Year < YearNow ; (Year =:= YearNow, Month < MonthNow) ; (Year =:= YearNow, Month =:= MonthNow, Day < DayNow))
    ->  % Date is in the past
        format(user_error, '[ERROR] Cannot schedule for past date: ~w (today is ~w-~w-~w)~n', [DateStr, YearNow, MonthNow, DayNow]),
        (   Format = json
        ->  reply_json(json([error='Cannot schedule for a date in the past', requestedDate=Date]))
        ;   format('Content-type: text/plain~n~n'),
            format('ERROR: Cannot schedule for a date in the past.~n'),
            format('Requested date: ~w~n', [Date]),
            format('Current date: ~w-~w-~w~n', [YearNow, MonthNow, DayNow])
        )
    ;   % Date is valid, proceed with scheduling
        schedule_for_date(Request, Date, DateStr, Format)
    ).

% Helper predicate to handle the actual scheduling logic
schedule_for_date(_Request, Date, DateStr, Format) :-
    % Fetch approved vessel visit notifications from MainApi
    % NOTE: MainApi runs on port 5000 (HTTP) or 5001 (HTTPS)
    % Using HTTPS and disabling certificate verification for local development
    MainApiUrl = 'https://localhost:5001/api/VesselVisitNotifications/approved',
    format(user_error, '[INFO] Fetching approved notifications from: ~w~n', [MainApiUrl]),
    catch(
        (http_get(MainApiUrl, JsonData, [json_object(dict), cert_verify_hook(cert_accept_any)]),
         format(user_error, '[DEBUG] HTTP GET successful~n', [])),
        E,
        (format(user_error, '[ERROR] Failed to fetch notifications: ~w~n', [E]), 
         format(user_error, '[ERROR] Make sure MainApi is running on port 5001 (HTTPS)~n', []),
         format(user_error, '[ERROR] If authentication is required, the endpoint must allow anonymous access~n', []),
         JsonData = [])
    ),

    % JsonData is already a list of dicts
    (   is_list(JsonData) -> JsonList = JsonData ; JsonList = [] ),
    length(JsonList, TotalCount),
    format(user_error, '[INFO] Received ~w notifications from API~n', [TotalCount]),

    % Remove any existing vessel facts and ignored notifications
    retractall(vessel(_,_,_,_,_,_)),
    retractall(ignored_notification(_,_)),
    format(user_error, '[INFO] Cleared existing vessel facts and ignored notifications~n', []),

    % Iterate notifications and assert vessel facts only for the requested date
    catch(
        (assert_notifications_for_date(JsonList, Date, 1, CountAsserted),
         format(user_error, '[INFO] Asserted ~w vessel facts for date ~w~n', [CountAsserted, Date])),
        Error,
        (format(user_error, '[ERROR] Failed during notification processing: ~w~n', [Error]),
         CountAsserted = 0)
    ),

    % If no vessels were asserted, return error message
    (   findall(V, vessel(V,_,_,_,_,_), Vs), Vs = []
    ->  (format(user_error, '[ERROR] No approved vessels found for date ~w~n', [Date]),
         (   Format = json
         ->  reply_json(json([error='No approved vessel visit notifications found for the selected date', requestedDate=Date]))
         ;   format('Content-type: text/plain~n~n'),
             format('ERROR: No approved vessel visit notifications found for the selected date.~n'),
             format('Requested date: ~w~n', [Date])
         ))
    ;   (format(user_error, '[INFO] Running scheduling algorithm...~n', []),
         obtain_seq_shortest_delay(SeqBetterTriplets, SShortestDelay),
         format(user_error, '[INFO] Scheduling complete. Total delay: ~w~n', [SShortestDelay]),
         % Return as previously (JSON or text)
         (   Format = json
         ->  build_json_response(Date, SShortestDelay, SeqBetterTriplets, JsonResponse),
             reply_json(JsonResponse)
         ;   format('Content-type: text/plain~n~n'),
             format('========================================~n'),
             format('  VESSEL SCHEDULING - SHORTEST DELAY~n'),
             format('========================================~n~n'),
             format('Target Date: ~w~n', [Date]),
             format('Total Delay: ~w time units~n~n', [SShortestDelay]),
             print_summary_table(SeqBetterTriplets),
             format('~n'),
             print_timeline(SeqBetterTriplets)
         ))
    ).

% assert_notifications_for_date(+JsonList, +DateStr, +IndexIncr, -Count)
assert_notifications_for_date([], _Date, Count, Count) :-
    format(user_error, '[INFO] Finished processing notifications~n', []).
assert_notifications_for_date([D|Rest], Date, N, Count) :-
    format(user_error, '[DEBUG] ========== Notification #~w ==========~n', [N]),
    format(user_error, '[DEBUG] Dict keys: ~w~n', [D]),
    catch(
        (process_notification(D, Date, N, N1),
         format(user_error, '[DEBUG] Processed, continuing to next...~n', [])),
        Error,
        (format(user_error, '[ERROR] Caught error in notification #~w: ~w~n', [N, Error]),
         N1 = N)
    ),
    !,  % Cut to prevent backtracking
    assert_notifications_for_date(Rest, Date, N1, Count).

process_notification(D, Date, N, N1) :-
    get_dict(arrivalTime, D, ArrivalAtom),
    format(user_error, '[DEBUG] arrivalTime raw: ~w~n', [ArrivalAtom]),
    
    % Convert to string safely
    (atom(ArrivalAtom) -> atom_string(ArrivalAtom, ArrivalStr) ; ArrivalStr = ArrivalAtom),
    format(user_error, '[DEBUG] arrivalTime string: ~w~n', [ArrivalStr]),
    
    % Extract date
    sub_string(ArrivalStr, 0, 10, _, ArrivalDateStr),
    format(user_error, '[DEBUG] Extracted date: ~w~n', [ArrivalDateStr]),
    
    % Convert requested date to string
    (atom(Date) -> atom_string(Date, DateStr) ; DateStr = Date),
    format(user_error, '[DEBUG] Requested date: ~w~n', [DateStr]),
    
    % Compare dates
    (ArrivalDateStr == DateStr ->
        (format(user_error, '[DEBUG] ✓ MATCH! Validating resources...~n', []),
         % Validate physical resources and staff members
         (validate_notification_resources(D) ->
             (format(user_error, '[DEBUG] ✓ Resources valid! Extracting vessel info...~n', []),
              extract_vessel_info(D, ArrivalStr, N),
              N1 is N + 1)
         ;
             (format(user_error, '[WARN] ✗ Resources invalid! Ignoring notification...~n', []),
              record_ignored_notification(D),
              N1 = N)
         ))
    ;
        (format(user_error, '[DEBUG] ✗ NO MATCH, skipping~n', []),
         N1 = N)
    ).

extract_vessel_info(D, ArrivalStr, N) :-
    format(user_error, '[DEBUG] Extracting arrival hour...~n', []),
    % Extract arrival hour safely
    (sub_string(ArrivalStr, 11, 2, _, HourStr) ->
        (catch(atom_string(HourAtom, HourStr), _, HourAtom = '00'),
         catch(atom_number(HourAtom, ArrivalHour), _, ArrivalHour = 0))
    ;   ArrivalHour = 0
    ),
    format(user_error, '[DEBUG] Arrival hour: ~w~n', [ArrivalHour]),
    
    format(user_error, '[DEBUG] Extracting departure time...~n', []),
    % Get departure hour
    (get_dict(departureTime, D, DepAtom) ->
        ((atom(DepAtom) -> atom_string(DepAtom, DepStr) ; DepStr = DepAtom),
         (sub_string(DepStr, 11, 2, _, DepHourStr) ->
             (catch(atom_string(DepHourAtom, DepHourStr), _, DepHourAtom = '00'),
              catch(atom_number(DepHourAtom, DepartureHour), _, (DepartureHour is ArrivalHour + 6)))
         ;   DepartureHour is ArrivalHour + 6))
    ;   DepartureHour is ArrivalHour + 6
    ),
    format(user_error, '[DEBUG] Departure hour: ~w~n', [DepartureHour]),
    
    format(user_error, '[DEBUG] Extracting load/unload times...~n', []),
    % Get unload/load times
    (get_dict(unloadTime, D, UnloadHours), number(UnloadHours) ->
        UnloadTime = UnloadHours
    ;   UnloadTime = 1
    ),
    (get_dict(loadTime, D, LoadHours), number(LoadHours) ->
        LoadTime = LoadHours
    ;   LoadTime = 1
    ),
    format(user_error, '[DEBUG] Unload: ~w, Load: ~w~n', [UnloadTime, LoadTime]),
    
    format(user_error, '[DEBUG] Building vessel ID...~n', []),
    % Build vessel ID
    atom_concat(v, N, IdAtom),
    format(user_error, '[DEBUG] Vessel ID: ~w~n', [IdAtom]),
    
    format(user_error, '[DEBUG] Getting vessel name...~n', []),
    % Get vessel name
    (get_dict(vesselName, D, VesselName) -> true ; VesselName = 'Unknown'),
    format(user_error, '[DEBUG] Vessel name: ~w~n', [VesselName]),
    
    format(user_error, '[DEBUG] Getting dock ID...~n', []),
    % Get dock ID
    (get_dict(assignedDock, D, DockId) ->
        format(user_error, '[DEBUG] Found assignedDock: ~w~n', [DockId])
    ;   (get_dict(dockId, D, DockId) ->
            format(user_error, '[DEBUG] Found dockId: ~w~n', [DockId])
        ;   (DockId = 'unknown_dock',
             format(user_error, '[DEBUG] No dock ID, using default~n', []))
        )
    ),
    
    format(user_error, '[INFO] ✓ ASSERTING vessel ~w: ~w (Dock=~w, Arr=~w, Dep=~w, Unl=~w, Ld=~w)~n',
           [IdAtom, VesselName, DockId, ArrivalHour, DepartureHour, UnloadTime, LoadTime]),
    
    % Assert the vessel
    assertz(vessel(IdAtom, DockId, ArrivalHour, DepartureHour, UnloadTime, LoadTime)),
    
    format(user_error, '[INFO] ✓✓ VESSEL ASSERTED SUCCESSFULLY!~n', []).

% ============================================
% VALIDATION PREDICATES
% ============================================

% validate_notification_resources(+Dict)
% Validates that the notification has both physical resources and staff members
validate_notification_resources(D) :-
    format(user_error, '[DEBUG] Validating physical resources...~n', []),
    % Check physical resources
    (get_dict(physicalResourceIds, D, PhysicalResources) ->
        (is_list(PhysicalResources) ->
            (PhysicalResources = [] ->
                (format(user_error, '[WARN] No physical resources assigned~n', []),
                 fail)
            ;
                (length(PhysicalResources, PhysCount),
                 format(user_error, '[DEBUG] Found ~w physical resource(s)~n', [PhysCount])))
        ;
            (format(user_error, '[DEBUG] Single physical resource found~n', []))
        )
    ;
        (format(user_error, '[WARN] physicalResourceIds field missing~n', []),
         fail)
    ),
    
    format(user_error, '[DEBUG] Validating staff members...~n', []),
    % Check staff members
    (get_dict(staffMemberIds, D, StaffMembers) ->
        (is_list(StaffMembers) ->
            (StaffMembers = [] ->
                (format(user_error, '[WARN] No staff members assigned~n', []),
                 fail)
            ;
                (length(StaffMembers, StaffCount),
                 format(user_error, '[DEBUG] Found ~w staff member(s)~n', [StaffCount])))
        ;
            (format(user_error, '[DEBUG] Single staff member found~n', []))
        )
    ;
        (format(user_error, '[WARN] staffMemberIds field missing~n', []),
         fail)
    ),
    
    format(user_error, '[INFO] ✓ Notification has valid resources~n', []).

% record_ignored_notification(+Dict)
% Records a notification that was ignored due to missing resources
record_ignored_notification(D) :-
    % Get vessel name
    (get_dict(vesselName, D, VesselName) -> true ; VesselName = 'Unknown'),
    
    % Determine reason
    (get_dict(physicalResourceIds, D, PhysicalResources) ->
        (is_list(PhysicalResources), PhysicalResources = [] ->
            MissingPhys = true
        ;
            MissingPhys = false)
    ;
        MissingPhys = true
    ),
    
    (get_dict(staffMemberIds, D, StaffMembers) ->
        (is_list(StaffMembers), StaffMembers = [] ->
            MissingStaff = true
        ;
            MissingStaff = false)
    ;
        MissingStaff = true
    ),
    
    % Build reason message
    (MissingPhys, MissingStaff ->
        Reason = 'Missing physical resources and staff members'
    ; MissingPhys ->
        Reason = 'Missing physical resources'
    ; MissingStaff ->
        Reason = 'Missing staff members'
    ;
        Reason = 'Invalid resources configuration'
    ),
    
    format(user_error, '[INFO] Recording ignored notification: ~w - ~w~n', [VesselName, Reason]),
    assertz(ignored_notification(VesselName, Reason)).

% ============================================
% JSON RESPONSE BUILDERS
% ============================================

% Build JSON response from schedule data with ignored notifications
build_json_response(Date, TotalDelay, SeqTriplets, JsonResponse) :-
    maplist(triplet_to_json, SeqTriplets, ScheduleList),
    % Get list of ignored notifications
    findall(json([vessel=VName, reason=Reason]), ignored_notification(VName, Reason), IgnoredList),
    JsonResponse = json([
        date=Date,
        totalDelay=TotalDelay,
        schedule=ScheduleList,
        ignoredNotifications=IgnoredList
    ]).

% Convert a triplet (V, TInUnload, TEndLoad) to JSON object
triplet_to_json((V, TInUnload, TEndLoad), Json) :-
    vessel(V, DockId, Arrival, Departure, Unload, Load),
    Duration is Unload + Load,
    TPossibleDep is TEndLoad + 1,
    (TPossibleDep > Departure -> Delay is TPossibleDep - Departure ; Delay is 0),
    Json = json([
        vessel=V,
        dockId=DockId,
        arrival=Arrival,
        departure=Departure,
        startTime=TInUnload,
        endTime=TEndLoad,
        duration=Duration,
        delay=Delay
    ]).

% Print summary table with vessel details
print_summary_table(SeqTriplets) :-
    format('SUMMARY TABLE:~n'),
    format('+----------+-------------+----------+----------+---------+~n'),
    format('| Vessel   | Start Time  | End Time | Duration | Delay   |~n'),
    format('+----------+-------------+----------+----------+---------+~n'),
    print_table_rows(SeqTriplets),
    format('+----------+-------------+----------+----------+---------+~n').

% Print each row of the table
print_table_rows([]).
print_table_rows([(V, TInUnload, TEndLoad)|Rest]) :-
    vessel(V, _DockId, Arrival, Departure, Unload, Load),
    Duration is Unload + Load,
    TPossibleDep is TEndLoad + 1,
    (TPossibleDep > Departure -> Delay is TPossibleDep - Departure ; Delay is 0),
    format('| ~|~t~w~8+ | ~|~t~w~11+ | ~|~t~w~8+ | ~|~t~w~8+ | ~|~t~w~7+ |~n', 
           [V, TInUnload, TEndLoad, Duration, Delay]),
    print_table_rows(Rest).

% Print timeline visualization
print_timeline(SeqTriplets) :-
    format('TIMELINE VISUALIZATION:~n'),
    format('~n'),
    find_max_time(SeqTriplets, MaxTime),
    print_time_ruler(MaxTime),
    format('~n'),
    print_vessel_timelines(SeqTriplets, MaxTime).

% Find maximum time for timeline scale
find_max_time([], 0).
find_max_time([(_, _, TEndLoad)|Rest], MaxTime) :-
    find_max_time(Rest, RestMax),
    MaxTime is max(TEndLoad + 2, RestMax).

% Print time ruler
print_time_ruler(MaxTime) :-
    format('Time:  '),
    print_time_marks(0, MaxTime),
    format('~n       '),
    print_time_line(0, MaxTime).

print_time_marks(T, MaxTime) :-
    T > MaxTime, !.
print_time_marks(T, MaxTime) :-
    format('~|~t~d~2+', [T]),
    T1 is T + 1,
    print_time_marks(T1, MaxTime).

print_time_line(T, MaxTime) :-
    T > MaxTime, !,
    format('~n').
print_time_line(T, MaxTime) :-
    format('--'),
    T1 is T + 1,
    print_time_line(T1, MaxTime).

% Print timeline for each vessel
print_vessel_timelines([], _).
print_vessel_timelines([(V, TInUnload, TEndLoad)|Rest], MaxTime) :-
    vessel(V, _DockId, Arrival, _, _, _),
    format('~w:     ', [V]),
    print_vessel_line(0, Arrival, TInUnload, TEndLoad, MaxTime),
    format('~n'),
    print_vessel_timelines(Rest, MaxTime).

% Print individual vessel timeline
print_vessel_line(Current, Arrival, TInUnload, TEndLoad, MaxTime) :-
    Current > MaxTime, !.
print_vessel_line(Current, Arrival, TInUnload, TEndLoad, MaxTime) :-
    (   Current =:= Arrival ->
        format('A ')  % Arrival
    ;   Current >= TInUnload, Current =< TEndLoad ->
        format('██')  % Working period
    ;   Current < Arrival ->
        format('  ')  % Before arrival
    ;   Current > TEndLoad ->
        format('  ')  % After completion
    ;
        format('··')  % Waiting
    ),
    Next is Current + 1,
    print_vessel_line(Next, Arrival, TInUnload, TEndLoad, MaxTime).

% ============================================
% US 3.4.4 - Heuristic Schedule Endpoint (EAT)
% ============================================
% GET http://localhost:5003/heuristic_schedule?date=2025-11-09&format=json
% GET http://localhost:5003/heuristic_schedule?date=2025-11-09&format=text (default)

% Main handler that dispatches based on HTTP method
handle_heuristic_schedule(Request) :-
    memberchk(method(Method), Request),
    !,
    (   Method = options
    ->  handle_cors_preflight(Request)
    ;   Method = get
    ->  get_heuristic_schedule(Request)
    ;   % Unsupported method
        format('Status: 405~n'),
        format('Content-type: text/plain~n~n'),
        format('Method not allowed~n')
    ).

get_heuristic_schedule(Request) :-
    % Send CORS headers first - ALWAYS send these regardless of errors
    catch(
        get_heuristic_schedule_impl(Request),
        Error,
        (format(user_error, '[FATAL ERROR] Unhandled exception in get_heuristic_schedule: ~w~n', [Error]),
         format('Content-type: application/json~n~n'),
         format('{"error": "Internal server error", "details": "~w"}~n', [Error]))
    ).

get_heuristic_schedule_impl(Request) :-
    % Send CORS headers
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type, Authorization~n'),
    
    % Read query parameters
    http_parameters(Request,
                    [ date(Date, [optional(true), default('2025-11-09')]),
                      format(Format, [optional(true), default('text'), oneof([json, text])])
                    ]),

    % Validate that the date is not in the past
    get_time(Now),
    stamp_date_time(Now, date(YearNow, MonthNow, DayNow, _, _, _, _, _, _), local),
    (   atom(Date) -> atom_string(Date, DateStr) ; DateStr = Date ),
    % Parse the requested date (format: YYYY-MM-DD)
    split_string(DateStr, "-", "", [YearStr, MonthStr, DayStr]),
    atom_number(YearStr, Year),
    atom_number(MonthStr, Month),
    atom_number(DayStr, Day),
    % Compare dates
    (   (Year < YearNow ; (Year =:= YearNow, Month < MonthNow) ; (Year =:= YearNow, Month =:= MonthNow, Day < DayNow))
    ->  % Date is in the past
        format(user_error, '[ERROR] Cannot schedule for past date: ~w (today is ~w-~w-~w)~n', [DateStr, YearNow, MonthNow, DayNow]),
        (   Format = json
        ->  reply_json(json([error='Cannot schedule for a date in the past', requestedDate=Date]))
        ;   format('Content-type: text/plain~n~n'),
            format('ERROR: Cannot schedule for a date in the past.~n'),
            format('Requested date: ~w~n', [Date]),
            format('Current date: ~w-~w-~w~n', [YearNow, MonthNow, DayNow])
        )
    ;   % Date is valid, proceed with heuristic scheduling
        heuristic_schedule_for_date(Request, Date, DateStr, Format)
    ).

% Helper predicate to handle the actual heuristic scheduling logic
heuristic_schedule_for_date(_Request, Date, DateStr, Format) :-
    % Fetch approved vessel visit notifications from MainApi
    MainApiUrl = 'https://localhost:5001/api/VesselVisitNotifications/approved',
    format(user_error, '[INFO] Heuristic - Fetching approved notifications from: ~w~n', [MainApiUrl]),
    catch(
        (http_get(MainApiUrl, JsonData, [json_object(dict), cert_verify_hook(cert_accept_any)]),
         format(user_error, '[DEBUG] Heuristic - HTTP GET successful~n', [])),
        E,
        (format(user_error, '[ERROR] Heuristic - Failed to fetch notifications: ~w~n', [E]), 
         format(user_error, '[ERROR] Make sure MainApi is running on port 5001 (HTTPS)~n', []),
         format(user_error, '[ERROR] If authentication is required, the endpoint must allow anonymous access~n', []),
         JsonData = [])
    ),

    % JsonData is already a list of dicts
    (   is_list(JsonData) -> JsonList = JsonData ; JsonList = [] ),
    length(JsonList, TotalCount),
    format(user_error, '[INFO] Heuristic - Received ~w notifications from API~n', [TotalCount]),

    % Remove any existing vessel facts and ignored notifications
    retractall(vessel(_,_,_,_,_,_)),
    retractall(ignored_notification(_,_)),
    format(user_error, '[INFO] Heuristic - Cleared existing vessel facts and ignored notifications~n', []),

    % Iterate notifications and assert vessel facts only for the requested date (reuse same logic)
    catch(
        (assert_notifications_for_date(JsonList, Date, 1, CountAsserted),
         format(user_error, '[INFO] Heuristic - Asserted ~w vessel facts for date ~w~n', [CountAsserted, Date])),
        Error,
        (format(user_error, '[ERROR] Heuristic - Failed during notification processing: ~w~n', [Error]),
         CountAsserted = 0)
    ),

    % If no vessels were asserted, return error message
    (   findall(V, vessel(V,_,_,_,_,_), Vs), Vs = []
    ->  (format(user_error, '[ERROR] Heuristic - No approved vessels found for date ~w~n', [Date]),
         (   Format = json
         ->  reply_json(json([error='No approved vessel visit notifications found for the selected date', requestedDate=Date]))
         ;   format('Content-type: text/plain~n~n'),
             format('ERROR: No approved vessel visit notifications found for the selected date.~n'),
             format('Requested date: ~w~n', [Date])
         ))
    ;   (format(user_error, '[INFO] Heuristic - Running EAT scheduling algorithm...~n', []),
         % Record start time for performance metrics
         get_time(StartTime),
         heuristic_early_arrival_time(SeqTripletsH, SDelaysH),
         get_time(EndTime),
         ComputationTime is EndTime - StartTime,
         format(user_error, '[INFO] Heuristic - Scheduling complete. Total delay: ~w, Time: ~3f seconds~n', [SDelaysH, ComputationTime]),
         % Return results
         (   Format = json
         ->  build_heuristic_json_response(Date, SDelaysH, ComputationTime, SeqTripletsH, JsonResponse),
             reply_json(JsonResponse)
         ;   format('Content-type: text/plain~n~n'),
             format('========================================~n'),
             format('  HEURISTIC SCHEDULING (EAT)~n'),
             format('========================================~n~n'),
             format('Algorithm: Early Arrival Time (EAT)~n'),
             format('Target Date: ~w~n', [Date]),
             format('Total Delay: ~w time units~n', [SDelaysH]),
             format('Computation Time: ~3f seconds~n~n', [ComputationTime]),
             print_summary_table(SeqTripletsH),
             format('~n'),
             print_timeline(SeqTripletsH)
         ))
    ).

% Build JSON response for heuristic algorithm with performance metrics
build_heuristic_json_response(Date, TotalDelay, ComputationTime, SeqTriplets, JsonResponse) :-
    maplist(triplet_to_json, SeqTriplets, ScheduleList),
    % Get list of ignored notifications
    findall(json([vessel=VName, reason=Reason]), ignored_notification(VName, Reason), IgnoredList),
    JsonResponse = json([
        algorithm='Heuristic (Early Arrival Time)',
        date=Date,
        totalDelay=TotalDelay,
        computationTime=ComputationTime,
        schedule=ScheduleList,
        ignoredNotifications=IgnoredList
    ]).

% ============================================
% US 3.4.5 - Multi-Crane Schedule Endpoint
% ============================================
% GET http://localhost:5003/multi_crane_schedule?date=2025-11-09&format=json
% GET http://localhost:5003/multi_crane_schedule?date=2025-11-09&format=text (default)

% Main handler that dispatches based on HTTP method
handle_multi_crane_schedule(Request) :-
    memberchk(method(Method), Request),
    !,
    (   Method = options
    ->  handle_cors_preflight(Request)
    ;   Method = get
    ->  get_multi_crane_schedule(Request)
    ;   % Unsupported method
        format('Status: 405~n'),
        format('Content-type: text/plain~n~n'),
        format('Method not allowed~n')
    ).

get_multi_crane_schedule(Request) :-
    % Send CORS headers first
    catch(
        get_multi_crane_schedule_impl(Request),
        E,
        (format('Access-Control-Allow-Origin: *~n'),
         format('Access-Control-Allow-Methods: GET, OPTIONS~n'),
         format('Access-Control-Allow-Headers: Content-Type, Authorization~n'),
         format('Content-type: application/json~n~n'),
         format(user_error, '[ERROR] Multi-crane - Exception caught: ~w~n', [E]),
         reply_json(json([error='Internal server error', details=E])))
    ).

get_multi_crane_schedule_impl(Request) :-
    % Send CORS headers
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type, Authorization~n'),
    
    % Read query parameters
    http_parameters(Request,
                    [date(Date, [optional(false)]),
                     format(Format, [optional(true), default(text)])
                    ]),

    % Validate that the date is not in the past
    get_time(Now),
    stamp_date_time(Now, date(YearNow, MonthNow, DayNow, _, _, _, _, _, _), local),
    (   atom(Date) -> atom_string(Date, DateStr) ; DateStr = Date ),
    % Parse the requested date (format: YYYY-MM-DD)
    split_string(DateStr, "-", "", [YearStr, MonthStr, DayStr]),
    atom_number(YearStr, Year),
    atom_number(MonthStr, Month),
    atom_number(DayStr, Day),
    % Compare dates
    (   (Year < YearNow ; (Year =:= YearNow, Month < MonthNow) ; (Year =:= YearNow, Month =:= MonthNow, Day < DayNow))
    ->  % Date is in the past
        (   Format = json
        ->  reply_json(json([error='Cannot schedule for past dates', requestedDate=Date]))
        ;   format('Content-type: text/plain~n~n'),
            format('ERROR: Cannot schedule for past dates.~n'),
            format('Requested date: ~w~n', [Date])
        )
    ;   % Date is valid, proceed with multi-crane scheduling
        multi_crane_schedule_for_date(Request, Date, DateStr, Format)
    ).

% Helper predicate to handle the actual multi-crane scheduling logic
multi_crane_schedule_for_date(_Request, Date, DateStr, Format) :-
    % Fetch max cranes from PhysicalResources API
    PhysicalResourcesUrl = 'https://localhost:5001/api/PhysicalResources',
    format(user_error, '[INFO] Multi-crane - Fetching physical resources from: ~w~n', [PhysicalResourcesUrl]),
    catch(
        (http_get(PhysicalResourcesUrl, ResourcesData, [json_object(dict), cert_verify_hook(cert_accept_any)]),
         format(user_error, '[DEBUG] Multi-crane - Physical resources GET successful~n', [])),
        E,
        (format(user_error, '[WARN] Multi-crane - Failed to fetch physical resources: ~w~n', [E]),
         ResourcesData = [])
    ),
    
    format(user_error, '[DEBUG] Multi-crane - ResourcesData type: ~w~n', [ResourcesData]),
    
    % Count cranes in the physical resources
    catch(
        (   is_list(ResourcesData)
        ->  (format(user_error, '[DEBUG] Multi-crane - ResourcesData is a list~n', []),
             findall(1, (member(R, ResourcesData), get_dict(type, R, Type), Type = 'Crane'), CraneList),
             length(CraneList, MaxCranes),
             format(user_error, '[DEBUG] Multi-crane - CraneList length: ~w~n', [MaxCranes]))
        ;   (format(user_error, '[DEBUG] Multi-crane - ResourcesData is NOT a list, using default~n', []),
             MaxCranes = 2)  % fallback
        ),
        CraneCountError,
        (format(user_error, '[ERROR] Multi-crane - Error counting cranes: ~w~n', [CraneCountError]),
         MaxCranes = 2)
    ),
    
    (   MaxCranes > 0
    ->  format(user_error, '[INFO] Multi-crane - Found ~w cranes in database~n', [MaxCranes])
    ;   (format(user_error, '[WARN] Multi-crane - No cranes found, using default max=2~n', []),
         MaxCranesFixed = 2)
    ),
    
    % Use the correct variable
    (   var(MaxCranesFixed)
    ->  MaxCranesFinal = MaxCranes
    ;   MaxCranesFinal = MaxCranesFixed
    ),
    
    % Set as global variable for use in generate_crane_allocation
    nb_setval(max_cranes, MaxCranesFinal),
    format(user_error, '[INFO] Multi-crane - Set max_cranes global variable to: ~w~n', [MaxCranesFinal]),
    
    % Fetch approved vessel visit notifications from MainApi
    MainApiUrl = 'https://localhost:5001/api/VesselVisitNotifications/approved',
    format(user_error, '[INFO] Multi-crane - Fetching approved notifications from: ~w~n', [MainApiUrl]),
    catch(
        (http_get(MainApiUrl, JsonData, [json_object(dict), cert_verify_hook(cert_accept_any)]),
         format(user_error, '[DEBUG] Multi-crane - HTTP GET successful~n', [])),
        E,
        (format(user_error, '[ERROR] Multi-crane - Failed to fetch notifications: ~w~n', [E]), 
         format(user_error, '[ERROR] Make sure MainApi is running on port 5001 (HTTPS)~n', []),
         format(user_error, '[ERROR] If authentication is required, the endpoint must allow anonymous access~n', []),
         JsonData = [])
    ),

    % JsonData is already a list of dicts
    (   is_list(JsonData) -> JsonList = JsonData ; JsonList = [] ),
    length(JsonList, TotalCount),
    format(user_error, '[INFO] Multi-crane - Received ~w notifications from API~n', [TotalCount]),

    % Remove any existing vessel facts and ignored notifications
    retractall(vessel(_,_,_,_,_,_)),
    retractall(ignored_notification(_,_)),
    format(user_error, '[INFO] Multi-crane - Cleared existing vessel facts and ignored notifications~n', []),

    % Iterate notifications and assert vessel facts only for the requested date
    catch(
        (assert_notifications_for_date(JsonList, Date, 1, CountAsserted),
         format(user_error, '[INFO] Multi-crane - Asserted ~w vessel facts for date ~w~n', [CountAsserted, Date])),
        Error,
        (format(user_error, '[ERROR] Multi-crane - Failed during notification processing: ~w~n', [Error]),
         CountAsserted = 0)
    ),

    % If no vessels were asserted, return error message
    (   findall(V, vessel(V,_,_,_,_,_), Vs), Vs = []
    ->  (format(user_error, '[ERROR] Multi-crane - No approved vessels found for date ~w~n', [Date]),
         (   Format = json
         ->  reply_json(json([error='No approved vessel visit notifications found for the selected date', requestedDate=Date]))
         ;   format('Content-type: text/plain~n~n'),
             format('ERROR: No approved vessel visit notifications found for the selected date.~n'),
             format('Requested date: ~w~n', [Date])
         ))
    ;   (format(user_error, '[INFO] Multi-crane - Running multi-crane scheduling algorithm...~n', []),
         % Record start time for performance metrics
         get_time(StartTime),
         obtain_seq_multi_crane(SeqTriplets, TotalDelay, CraneAllocation),
         get_time(EndTime),
         ComputationTime is EndTime - StartTime,
         format(user_error, '[INFO] Multi-crane - Scheduling complete. Total delay: ~w, Time: ~3f seconds~n', [TotalDelay, ComputationTime]),
         % Return results
         (   Format = json
         ->  build_multi_crane_json_response(Date, TotalDelay, ComputationTime, SeqTriplets, CraneAllocation, JsonResponse),
             reply_json(JsonResponse)
         ;   format('Content-type: text/plain~n~n'),
             format('========================================~n'),
             format('  MULTI-CRANE SCHEDULING~n'),
             format('========================================~n~n'),
             format('Algorithm: Multi-Crane Optimization~n'),
             format('Target Date: ~w~n', [Date]),
             format('Total Delay: ~w time units~n', [TotalDelay]),
             format('Computation Time: ~3f seconds~n~n', [ComputationTime]),
             print_multi_crane_summary(SeqTriplets, CraneAllocation),
             format('~n'),
             print_timeline(SeqTriplets)
         ))
    ).

% Build JSON response for multi-crane algorithm
build_multi_crane_json_response(Date, TotalDelay, ComputationTime, SeqTriplets, CraneAllocation, JsonResponse) :-
    maplist(triplet_crane_to_json(CraneAllocation), SeqTriplets, ScheduleList),
    % Convert CraneAllocation to JSON-serializable format
    maplist(crane_alloc_to_json, CraneAllocation, CraneAllocJson),
    % Get list of ignored notifications
    findall(json([vessel=VName, reason=Reason]), ignored_notification(VName, Reason), IgnoredList),
    JsonResponse = json([
        algorithm='Multi-Crane Optimization',
        date=Date,
        totalDelay=TotalDelay,
        computationTime=ComputationTime,
        schedule=ScheduleList,
        craneAllocation=CraneAllocJson,
        ignoredNotifications=IgnoredList
    ]).

% Convert crane allocation tuple to JSON object
crane_alloc_to_json((Vessel, NumCranes), json([vessel=Vessel, cranes=NumCranes])).

% Convert a triplet with crane info to JSON
triplet_crane_to_json(CraneAllocation, (V, TInUnload, TEndLoad), Json) :-
    vessel(V, DockId, Arrival, Departure, Unload, Load),
    Duration is (TEndLoad - TInUnload + 1),
    TPossibleDep is TEndLoad + 1,
    (   TPossibleDep > Departure
    ->  Delay is TPossibleDep - Departure
    ;   Delay is 0
    ),
    % Find number of cranes for this vessel
    (   member((V, NumCranes), CraneAllocation)
    ->  true
    ;   NumCranes = 1
    ),
    Json = json([
        vessel=V,
        dockId=DockId,
        arrival=Arrival,
        departure=Departure,
        startTime=TInUnload,
        endTime=TEndLoad,
        duration=Duration,
        delay=Delay,
        cranes=NumCranes
    ]).

% Print summary table for multi-crane schedule
print_multi_crane_summary(SeqTriplets, CraneAllocation) :-
    format('~w~t~w~30|~t~w~40|~t~w~50|~t~w~60|~t~w~70|~n', 
           ['Vessel', 'Start', 'End', 'Duration', 'Delay', 'Cranes']),
    format('~`-t~70|~n'),
    print_multi_crane_rows(SeqTriplets, CraneAllocation).

print_multi_crane_rows([], _).
print_multi_crane_rows([(V, TInUnload, TEndLoad)|Rest], CraneAllocation) :-
    vessel(V, _, _, Departure, _, _),
    Duration is (TEndLoad - TInUnload + 1),
    TPossibleDep is TEndLoad + 1,
    (   TPossibleDep > Departure
    ->  Delay is TPossibleDep - Departure
    ;   Delay is 0
    ),
    (   member((V, NumCranes), CraneAllocation)
    ->  true
    ;   NumCranes = 1
    ),
    format('~w~t~w~30|~t~w~40|~t~w~50|~t~w~60|~t~w~70|~n', 
           [V, TInUnload, TEndLoad, Duration, Delay, NumCranes]),
    print_multi_crane_rows(Rest, CraneAllocation).