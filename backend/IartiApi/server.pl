% Bibliotecas 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

% Carregar US 3.4.2
:- consult('PrologCode/US 3.4.2.pl').

:- http_handler('/lapr5', responde_ola, []).
:- http_handler('/register_user', register_user, []).
:- http_handler('/send_file_post', send_file_post, []).
:- http_handler('/shortest_delay', get_shortest_delay, [method(get)]).

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
    http_parameters(Request,
                    [ date(Date, [optional(true), default('2025-11-09')]),
                      format(Format, [optional(true), default('text'), oneof([json, text])])
                    ]),
    obtain_seq_shortest_delay(SeqBetterTriplets, SShortestDelay),
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