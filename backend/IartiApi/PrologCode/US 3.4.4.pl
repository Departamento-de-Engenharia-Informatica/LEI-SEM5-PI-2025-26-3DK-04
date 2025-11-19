% ============================================
% US 3.4.4 - ALTERNATIVE SCHEDULING ALGORITHM
% Early Arrival Time (EAT) Heuristic
% ============================================
% This algorithm prioritizes vessels that arrive early,
% using a greedy approach for computational efficiency.
% Declaração dinâmica para permitir assertz/retract em runtime
:- dynamic vessel/6.
:- dynamic ignored_notification/2.

% ============================================
% HEURISTIC ALGORITHM - EARLY ARRIVAL TIME
% ============================================

% heuristic_early_arrival_time(SeqTripletsH, SDelaysH).
% Returns the sequence of vessels with delays calculated using EAT heuristic
% This algorithm processes each dock independently and combines results
heuristic_early_arrival_time(SeqTripletsH, SDelaysH) :-
    % Group vessels by dock
    findall(DockId, vessel(_, DockId, _, _, _, _), AllDocks),
    sort(AllDocks, UniqueDocks), % remove duplicates
    format(user_error, '[INFO] EAT Heuristic - Found ~w unique docks: ~w~n', [length(UniqueDocks), UniqueDocks]),
    % For each dock, calculate the best schedule using EAT heuristic
    obtain_eat_schedule_per_dock(UniqueDocks, AllSchedules, TotalDelay),
    % Combine all schedules
    flatten(AllSchedules, SeqTripletsH),
    SDelaysH = TotalDelay.

% obtain_eat_schedule_per_dock(+Docks, -AllSchedules, -TotalDelay)
% For each dock, finds the schedule using EAT heuristic and sums delays
obtain_eat_schedule_per_dock([], [], 0).
obtain_eat_schedule_per_dock([DockId|RestDocks], [Schedule|RestSchedules], TotalDelay) :-
    format(user_error, '[INFO] EAT Heuristic - Processing dock: ~w~n', [DockId]),
    % Find all vessels for this dock
    findall((Arrival, V), vessel(V, DockId, Arrival, _, _, _), LAV),
    length(LAV, VesselCount),
    format(user_error, '[INFO] EAT Heuristic - Dock ~w has ~w vessels~n', [DockId, VesselCount]),
    (   LAV = []
    ->  % No vessels in this dock
        Schedule = [],
        DockDelay = 0
    ;   % Calculate schedule for this dock using EAT heuristic
        sort(LAV, LAVSorted), % Sort by arrival time (earliest first)
        obtain_vessels(LAVSorted, SeqV),
        sequence_temporization(SeqV, Schedule),
        sum_delays(Schedule, DockDelay),
        format(user_error, '[INFO] EAT Heuristic - Dock ~w: delay = ~w~n', [DockId, DockDelay])
    ),
    % Recursion for remaining docks
    obtain_eat_schedule_per_dock(RestDocks, RestSchedules, RestDelay),
    TotalDelay is DockDelay + RestDelay.

% obtain_vessels(+LAVSorted, -SeqV)
% Extracts vessel IDs from sorted list of (Arrival, Vessel) pairs
obtain_vessels([], []).
obtain_vessels([(_, V)|LAV], [V|LV]) :-
    obtain_vessels(LAV, LV).

% ============================================
% TEMPORIZATION LOGIC (same as US 3.4.2)
% ============================================

% sequence_temporization([a,b,c], SeqTriplets).
% Returns all vessel sequences with their associated timing
sequence_temporization(LV, SeqTriplets) :-
    sequence_temporization1(0, LV, SeqTriplets).

% Recursive case: calculate times for each vessel
sequence_temporization1(EndPrev, [V|LV], [(V, TInUnload, TEndLoad)|SeqTriplets]) :-
    vessel(V, _DockId, Arrival, _, Unload, Load), % Get vessel data (now with DockId)
    % Start time is the maximum between arrival and end of previous vessel
    ((Arrival > EndPrev, !, TInUnload is Arrival); TInUnload is EndPrev + 1),
    % End time is start + unload + load times - 1 (because it starts at the beginning of the hour)
    TEndLoad is TInUnload + Unload + Load - 1,
    % Recursive call for next vessel
    sequence_temporization1(TEndLoad, LV, SeqTriplets).

% Base case: empty list
sequence_temporization1(_, [], []).

% sum_delays([a,b,c], S).
% Returns the sum of delays for a sequence of vessels
sum_delays([], 0).

% For each vessel, calculate individual delay and sum recursively
sum_delays([(V, _, TEndLoad)|LV], S) :-
    vessel(V, _DockId, _, Departure, _, _), % Get departure time (now with DockId)
    TPossibleDep is TEndLoad + 1, % Possible departure
    ((TPossibleDep > Departure, !, SV is TPossibleDep - Departure); SV is 0), % Calculate delay
    sum_delays(LV, SLV), % Sum delays of remaining vessels
    S is SV + SLV.
