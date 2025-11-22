% ============================================
% US 3.4.4 - ALTERNATIVE SCHEDULING ALGORITHM
% Early Departure Time (EDT) Heuristic
% ============================================
% This algorithm prioritizes vessels that depart early,
% using a greedy approach for computational efficiency.
% Declaração dinâmica para permitir assertz/retract em runtime
:- dynamic vessel/6.
:- dynamic ignored_notification/2.

% ============================================
% HEURISTIC ALGORITHM - EARLY DEPARTURE TIME
% ============================================

% heuristic_early_departure_time(SeqTripletsH, SDelaysH).
% Returns the sequence of vessels with delays calculated using EDT heuristic
% This algorithm processes each dock independently and combines results
heuristic_early_departure_time(SeqTripletsH, SDelaysH) :-
    % Group vessels by dock
    findall(DockId, vessel(_, DockId, _, _, _, _), AllDocks),
    sort(AllDocks, UniqueDocks), % remove duplicates
    format(user_error, '[INFO] EDT Heuristic - Found ~w unique docks: ~w~n', [length(UniqueDocks), UniqueDocks]),
    % For each dock, calculate the best schedule using EDT heuristic
    obtain_edt_schedule_per_dock(UniqueDocks, AllSchedules, TotalDelay),
    % Combine all schedules
    flatten(AllSchedules, SeqTripletsH),
    SDelaysH = TotalDelay.

% obtain_edt_schedule_per_dock(+Docks, -AllSchedules, -TotalDelay)
% For each dock, finds the schedule using EDT heuristic and sums delays
obtain_edt_schedule_per_dock([], [], 0).
obtain_edt_schedule_per_dock([DockId|RestDocks], [Schedule|RestSchedules], TotalDelay) :-
    format(user_error, '[INFO] EDT Heuristic - Processing dock: ~w~n', [DockId]),
    findall((Departure, V), vessel(V, DockId, _, Departure, _, _), LDV),
    sort(LDV, LDVSorted),
    obtain_vessels(LDVSorted, SeqV),
    sequence_temporization(SeqV, Schedule),
    sum_delays(Schedule, DockDelay),
    format(user_error, '[INFO] EDT Heuristic - Dock ~w has ~w vessels~n', [DockId, length(SeqV)]),
    format(user_error, '[INFO] EDT Heuristic - Dock ~w: delay = ~w~n', [DockId, DockDelay]),
    obtain_edt_schedule_per_dock(RestDocks, RestSchedules, RestDelay),
    TotalDelay is DockDelay + RestDelay.

% obtain_vessels(+LDVSorted, -SeqV)
% Extracts vessel IDs from sorted list of (Departure, Vessel) pairs
obtain_vessels([], []).
obtain_vessels([(_, V)|LDV], [V|LV]) :-
    obtain_vessels(LDV, LV).

% ============================================
% TEMPORIZATION LOGIC (same as US 3.4.2)
% ============================================

% sequence_temporization([a,b,c], SeqTriplets).
% Returns all vessel sequences with their associated timing
sequence_temporization(LV, SeqTriplets) :-
    sequence_temporization1(0, LV, SeqTriplets).

% Recursive case: calculate times for each vessel
sequence_temporization1(EndPrev, [V|LV], [(V, TInUnload, TEndLoad)|SeqTriplets]) :-
    vessel(V, _DockId, Arrival, _, Unload, Load),
    ( (Arrival > EndPrev, !, TInUnload is Arrival) ; TInUnload is EndPrev + 1 ),
    TEndLoad is TInUnload + Unload + Load - 1,
    sequence_temporization1(TEndLoad, LV, SeqTriplets).

% Base case: empty list
sequence_temporization1(_, [], []).

% sum_delays([a,b,c], S).
% Returns the sum of delays for a sequence of vessels
sum_delays([], 0).

% For each vessel, calculate individual delay and sum recursively
sum_delays([(V, _, TEndLoad)|LV], S) :-
    vessel(V, _DockId, _, Departure, _, _),
    TPossibleDep is TEndLoad + 1,
    ( (TPossibleDep > Departure, !, SV is TPossibleDep - Departure) ; SV is 0 ),
    sum_delays(LV, SLV),
    S is SV + SLV.

vessel(v1, dock1, 6, 63, 10, 16).
vessel(v2, dock1, 23, 50, 9, 7).
vessel(v3, dock1, 8, 40, 5, 12).
vessel(v4, dock1, 27, 40, 0, 8).
vessel(v5, dock1, 36, 70, 12, 0).
vessel(v6, dock1, 40, 60, 8, 6).
vessel(v7, dock1, 52, 80, 9, 10).
vessel(v8, dock1, 61, 90, 13, 8).
vessel(v9, dock1, 74, 100, 7, 7).
vessel(v10, dock1, 81, 110, 6, 8).
vessel(v11, dock1, 90, 140, 22, 18).
vessel(v12, dock1, 112, 140, 8, 7).
vessel(v13, dock1, 82, 135, 13, 12).
vessel(v14, dock1, 95, 150, 10, 10).
vessel(v15, dock1, 100, 160, 12, 8).
vessel(v16, dock1, 105, 170, 7, 9).
vessel(v17, dock1, 110, 180, 15, 5).
vessel(v18, dock1, 115, 190, 9, 11).
vessel(v19, dock1, 120, 200, 8, 12).
vessel(v20, dock1, 125, 210, 14, 6).
vessel(v21, dock1, 130, 220, 11, 9).
vessel(v22, dock1, 135, 230, 13, 7).
vessel(v23, dock1, 140, 240, 10, 10).
vessel(v24, dock1, 145, 250, 12, 8).
vessel(v25, dock1, 150, 260, 7, 13).
vessel(v26, dock1, 155, 270, 9, 11).
vessel(v27, dock1, 160, 280, 8, 12).
vessel(v28, dock1, 165, 290, 14, 6).
vessel(v29, dock1, 170, 300, 11, 9).
vessel(v30, dock1, 175, 310, 13, 7).
