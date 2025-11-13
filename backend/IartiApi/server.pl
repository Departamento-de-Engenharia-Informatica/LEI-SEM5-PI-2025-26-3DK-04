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

:- http_handler('/lapr5', responde_ola, []).
:- http_handler('/register_user', register_user, []).
:- http_handler('/send_file_post', send_file_post, []).
:- http_handler('/shortest_delay', get_shortest_delay, [method(get)]).

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
% GET http://localhost:5000/shortest_delay?date=2025-11-09&format=json
% GET http://localhost:5000/shortest_delay?date=2025-11-09&format=text (default)
get_shortest_delay(Request) :-
    % Enable CORS
    cors_enable(Request, [methods([get])]),
    
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
    MainApiUrl = 'http://localhost:5000/api/VesselVisitNotifications/approved',
    format(user_error, '[INFO] Fetching approved notifications from: ~w~n', [MainApiUrl]),
    catch(
        (http_get(MainApiUrl, JsonData, [json_object(dict)]),
         format(user_error, '[DEBUG] HTTP GET successful~n', [])),
        E,
        (format(user_error, '[ERROR] Failed to fetch notifications: ~w~n', [E]), 
         format(user_error, '[ERROR] Make sure MainApi is running on port 5000~n', []),
         JsonData = [])
    ),

    % JsonData is already a list of dicts
    (   is_list(JsonData) -> JsonList = JsonData ; JsonList = [] ),
    length(JsonList, TotalCount),
    format(user_error, '[INFO] Received ~w notifications from API~n', [TotalCount]),

    % Remove any existing vessel facts (we will assert the ones from the DB)
    retractall(vessel(_,_,_,_,_)),
    format(user_error, '[INFO] Cleared existing vessel facts~n', []),

    % Iterate notifications and assert vessel facts only for the requested date
    assert_notifications_for_date(JsonList, Date, 1, CountAsserted),
    format(user_error, '[INFO] Asserted ~w vessel facts for date ~w~n', [CountAsserted, Date]),

    % If no vessels were asserted, return error message
    (   findall(V, vessel(V,_,_,_,_), Vs), Vs = []
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
    (   get_dict(arrivalTime, D, ArrivalAtom)
    ->  (   atom(ArrivalAtom) -> atom_string(ArrivalAtom, ArrivalStr)
        ;   string(ArrivalAtom) -> ArrivalStr = ArrivalAtom
        ;   format(user_error, '[WARNING] arrivalTime is not atom/string: ~w~n', [ArrivalAtom]), fail
        ),
        % Extract date (first 10 chars) from ISO8601 like 2025-11-13T05:00:00Z
        sub_string(ArrivalStr, 0, 10, _, ArrivalDateStr),
        % Convert Date parameter to string for comparison
        (   atom(Date) -> atom_string(Date, DateStr) ; DateStr = Date ),
        format(user_error, '[DEBUG] Processing notification: ArrivalDate=~w, RequestedDate=~w (types: ~w vs ~w)~n', 
               [ArrivalDateStr, DateStr, ArrivalDateStr, DateStr]),
        (   ArrivalDateStr = DateStr
        ->  % extract hour from ISO8601 (position 11-12)
            (   sub_string(ArrivalStr, 11, 2, _, Hs) -> atom_number(Hs, ArrivalHour) ; ArrivalHour = 0 ),
            % Get departure time
            (   get_dict(departureTime, D, DepAtom) 
            ->  (atom(DepAtom) -> atom_string(DepAtom, DepStr) ; DepStr = DepAtom),
                (sub_string(DepStr, 11, 2, _, Ds) -> atom_number(Ds, DepartureHour) ; DepartureHour = ArrivalHour + 6)
            ;   DepartureHour is ArrivalHour + 6
            ),
            % UnloadTime and LoadTime are in HOURS from backend DTO
            % Use them directly as time units in the Prolog scheduling logic
            (   get_dict(unloadTime, D, UnloadHours), number(UnloadHours) 
            ->  UnloadTime = UnloadHours
            ;   UnloadTime = 1
            ),
            (   get_dict(loadTime, D, LoadHours), number(LoadHours) 
            ->  LoadTime = LoadHours
            ;   LoadTime = 1
            ),
            % Build an id atom like v1, v2, ...
            atomic_list_concat([v, N], IdAtom),
            % Get vessel name if available
            (   get_dict(vesselName, D, VesselName) -> true ; VesselName = 'Unknown' ),
            % Log the vessel being added
            format(user_error, '[INFO] Adding vessel ~w: ~w (Arrival=~w, Departure=~w, Unload=~w hours, Load=~w hours)~n', 
                   [IdAtom, VesselName, ArrivalHour, DepartureHour, UnloadTime, LoadTime]),
            % Assert vessel(Id, ArrivalHour, DepartureHour, UnloadTime, LoadTime)
            assertz(vessel(IdAtom, ArrivalHour, DepartureHour, UnloadTime, LoadTime)),
            N1 is N + 1,
            assert_notifications_for_date(Rest, Date, N1, Count)
        ;   % not match date -> skip
            format(user_error, '[DEBUG] Skipping notification with arrival date ~w (requested: ~w)~n', [ArrivalDateStr, DateStr]),
            assert_notifications_for_date(Rest, Date, N, Count)
        )
    ;   % no arrivalTime field -> skip
        format(user_error, '[WARNING] Notification without arrivalTime field, skipping~n', []),
        assert_notifications_for_date(Rest, Date, N, Count)
    ).

% Build JSON response from schedule data
build_json_response(Date, TotalDelay, SeqTriplets, JsonResponse) :-
    maplist(triplet_to_json, SeqTriplets, ScheduleList),
    JsonResponse = json([
        date=Date,
        totalDelay=TotalDelay,
        schedule=ScheduleList
    ]).

% Convert a triplet (V, TInUnload, TEndLoad) to JSON object
triplet_to_json((V, TInUnload, TEndLoad), Json) :-
    vessel(V, Arrival, Departure, Unload, Load),
    Duration is Unload + Load,
    TPossibleDep is TEndLoad + 1,
    (TPossibleDep > Departure -> Delay is TPossibleDep - Departure ; Delay is 0),
    Json = json([
        vessel=V,
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
    vessel(V, Arrival, Departure, Unload, Load),
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
    vessel(V, Arrival, _, _, _),
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