% ============================================
% US 3.4.4 - ALTERNATIVE SCHEDULING ALGORITHM
% Combined Heuristic: Weighted EAT + SPT
% ============================================
% This algorithm combines Early Arrival Time (EAT) and Shortest Processing Time (SPT)
% by assigning a weighted score to each vessel: Score = w1*Arrival + w2*(Unload+Load)
% Lower score means higher priority. You can adjust weights w1 and w2 as needed.
% Declaração dinâmica para permitir assertz/retract em runtime
:- dynamic vessel/6.
:- dynamic ignored_notification/2.

% ============================================
% HEURISTIC ALGORITHM - COMBINED EAT + SPT
% ============================================

% heuristic_combined_eat_spt(SeqTripletsH, SDelaysH).
% Returns the sequence of vessels with delays calculated using the combined heuristic
% This algorithm processes each dock independently and combines results
heuristic_combined_eat_spt(SeqTripletsH, SDelaysH) :-
    % Weights for arrival and processing time
    WArrival = 1, WProc = 1, % You can tune these weights
    % Group vessels by dock
    findall(DockId, vessel(_, DockId, _, _, _, _), AllDocks),
    sort(AllDocks, UniqueDocks), % remove duplicates
    format(user_error, '[INFO] Combined Heuristic - Found ~w unique docks: ~w~n', [length(UniqueDocks), UniqueDocks]),
    % For each dock, calculate the best schedule using combined heuristic
    obtain_combined_schedule_per_dock(UniqueDocks, WArrival, WProc, AllSchedules, TotalDelay),
    % Combine all schedules
    flatten(AllSchedules, SeqTripletsH),
    SDelaysH = TotalDelay.

% obtain_combined_schedule_per_dock(+Docks, +WArrival, +WProc, -AllSchedules, -TotalDelay)
% For each dock, finds the schedule using combined heuristic and sums delays
obtain_combined_schedule_per_dock([], _, _, [], 0).
obtain_combined_schedule_per_dock([DockId|RestDocks], WArrival, WProc, [Schedule|RestSchedules], TotalDelay) :-
    format(user_error, '[INFO] Combined Heuristic - Processing dock: ~w~n', [DockId]),
    findall((Score, V), (vessel(V, DockId, Arrival, _, Unload, Load), ProcTime is Unload + Load, Score is WArrival*Arrival + WProc*ProcTime), LSV),
    sort(LSV, LSVSorted),
    obtain_vessels(LSVSorted, SeqV),
    sequence_temporization(SeqV, Schedule),
    sum_delays(Schedule, DockDelay),
    format(user_error, '[INFO] Combined Heuristic - Dock ~w has ~w vessels~n', [DockId, length(SeqV)]),
    format(user_error, '[INFO] Combined Heuristic - Dock ~w: delay = ~w~n', [DockId, DockDelay]),
    obtain_combined_schedule_per_dock(RestDocks, WArrival, WProc, RestSchedules, RestDelay),
    TotalDelay is DockDelay + RestDelay.

% obtain_vessels(+LSVSorted, -SeqV)
% Extracts vessel IDs from sorted list of (Score, Vessel) pairs
obtain_vessels([], []).
obtain_vessels([(_, V)|LSV], [V|LV]) :-
    obtain_vessels(LSV, LV).

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
