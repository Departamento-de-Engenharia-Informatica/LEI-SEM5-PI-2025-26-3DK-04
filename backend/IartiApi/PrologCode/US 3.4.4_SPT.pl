% ============================================
% US 3.4.4 - ALTERNATIVE SCHEDULING ALGORITHM
% Shortest Processing Time (SPT) Heuristic
% ============================================
% This algorithm prioritizes vessels with the shortest processing time (unload + load),
% using a greedy approach for computational efficiency.
% Declaração dinâmica para permitir assertz/retract em runtime
:- dynamic vessel/6.
:- dynamic ignored_notification/2.

% ============================================
% HEURISTIC ALGORITHM - SHORTEST PROCESSING TIME
% ============================================

% heuristic_shortest_processing_time(SeqTripletsH, SDelaysH).
% Returns the sequence of vessels with delays calculated using SPT heuristic
% This algorithm processes each dock independently and combines results
heuristic_shortest_processing_time(SeqTripletsH, SDelaysH) :-
    % Group vessels by dock
    findall(DockId, vessel(_, DockId, _, _, _, _), AllDocks),
    sort(AllDocks, UniqueDocks), % remove duplicates
    format(user_error, '[INFO] SPT Heuristic - Found ~w unique docks: ~w~n', [length(UniqueDocks), UniqueDocks]),
    % For each dock, calculate the best schedule using SPT heuristic
    obtain_spt_schedule_per_dock(UniqueDocks, AllSchedules, TotalDelay),
    % Combine all schedules
    flatten(AllSchedules, SeqTripletsH),
    SDelaysH = TotalDelay.

% obtain_spt_schedule_per_dock(+Docks, -AllSchedules, -TotalDelay)
% For each dock, finds the schedule using SPT heuristic and sums delays
obtain_spt_schedule_per_dock([], [], 0).
obtain_spt_schedule_per_dock([DockId|RestDocks], [Schedule|RestSchedules], TotalDelay) :-
    format(user_error, '[INFO] SPT Heuristic - Processing dock: ~w~n', [DockId]),
    findall((ProcTime, V), (vessel(V, DockId, _, _, Unload, Load), ProcTime is Unload + Load), LPTV),
    sort(LPTV, LPTVSorted),
    obtain_vessels(LPTVSorted, SeqV),
    sequence_temporization(SeqV, Schedule),
    sum_delays(Schedule, DockDelay),
    format(user_error, '[INFO] SPT Heuristic - Dock ~w has ~w vessels~n', [DockId, length(SeqV)]),
    format(user_error, '[INFO] SPT Heuristic - Dock ~w: delay = ~w~n', [DockId, DockDelay]),
    obtain_spt_schedule_per_dock(RestDocks, RestSchedules, RestDelay),
    TotalDelay is DockDelay + RestDelay.

% obtain_vessels(+LPTVSorted, -SeqV)
% Extracts vessel IDs from sorted list of (ProcTime, Vessel) pairs
obtain_vessels([], []).
obtain_vessels([(_, V)|LPTV], [V|LV]) :-
    obtain_vessels(LPTV, LV).

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

vessel(va, dock1, 6, 63, 10, 16).
vessel(vb, dock1, 23, 50, 9, 7).
vessel(vc, dock1, 8, 40, 5, 12).
vessel(vd, dock1, 27, 40, 0, 8).
vessel(ve, dock1, 36, 70, 12, 0).
vessel(vf, dock1, 40, 60, 8, 6).
vessel(vg, dock1, 52, 80, 9, 10).
vessel(vi, dock1, 61, 90, 13, 8).
vessel(vj, dock1, 74, 100, 7, 7).
vessel(vk, dock1, 81, 110, 6, 8).
vessel(vl, dock1, 90, 140, 22, 18).
%vessel(vm, dock1, 112, 140, 8, 7).
%vessel(vn, dock1, 82, 135, 13, 12).
