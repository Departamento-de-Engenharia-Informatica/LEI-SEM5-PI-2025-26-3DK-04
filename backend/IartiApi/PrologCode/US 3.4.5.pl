% ============================================
% US 3.4.5 - MULTI-CRANE SCHEDULING
% ============================================
% This module extends the single-crane scheduling to support multiple cranes
% when a single crane cannot eliminate all vessel departure delays.
% For each additional crane, unload/load times are divided by 2.

:- dynamic vessel/6.
:- dynamic ignored_notification/2.
:- dynamic shortest_delay_multi/3.

% ============================================
% MAIN ENTRY POINT
% ============================================
% obtain_seq_multi_crane(SeqBetterTriplets, SShortestDelay, CraneAllocation)
% Retorna a sequência com menor atraso usando multiplos guindastes quando necessário
obtain_seq_multi_crane(SeqBetterTriplets, SShortestDelay, CraneAllocation) :-
    % Agrupar navios por dock
    findall(DockId, vessel(_, DockId, _, _, _, _), AllDocks),
    sort(AllDocks, UniqueDocks),
    length(UniqueDocks, NumDocks),
    format(user_error, '[INFO] Multi-crane: Found ~w unique docks~n', [NumDocks]),
    
    % Para cada dock, calcular o melhor agendamento com multiplos guindastes
    obtain_best_multi_crane_schedule_per_dock(UniqueDocks, AllSchedules, TotalDelay, AllCraneAllocations),
    
    % Combinar todos os schedules e alocações
    flatten(AllSchedules, SeqBetterTriplets),
    flatten(AllCraneAllocations, CraneAllocation),
    SShortestDelay = TotalDelay.

% ============================================
% SCHEDULING PER DOCK WITH MULTIPLE CRANES
% ============================================
obtain_best_multi_crane_schedule_per_dock([], [], 0, []).
obtain_best_multi_crane_schedule_per_dock([DockId|RestDocks], [BestSchedule|RestSchedules], TotalDelay, [CraneAlloc|RestAllocations]) :-
    format(user_error, '[INFO] Multi-crane: Processing dock: ~w~n', [DockId]),
    
    % Encontrar todos os navios deste dock
    findall(V, vessel(V, DockId, _, _, _, _), VesselsInDock),
    length(VesselsInDock, VesselCount),
    format(user_error, '[INFO] Multi-crane: Dock ~w has ~w vessels~n', [DockId, VesselCount]),
    
    (   VesselsInDock = []
    ->  BestSchedule = [],
        DockDelay = 0,
        CraneAlloc = []
    ;   % Calcular melhor agendamento com multiplos guindastes
        obtain_seq_multi_crane_for_dock(DockId, VesselsInDock, BestSchedule, DockDelay, CraneAlloc)
    ),
    
    format(user_error, '[INFO] Multi-crane: Dock ~w: delay = ~w~n', [DockId, DockDelay]),
    
    % Recursão para os restantes docks
    obtain_best_multi_crane_schedule_per_dock(RestDocks, RestSchedules, RestDelay, RestAllocations),
    TotalDelay is DockDelay + RestDelay.

% ============================================
% MULTI-CRANE OPTIMIZATION FOR A SINGLE DOCK
% ============================================
% Tenta todas as combinações de guindastes para minimizar atraso
obtain_seq_multi_crane_for_dock(DockId, Vessels, BestSchedule, MinDelay, BestCraneAlloc) :-
    retractall(shortest_delay_multi(_, _, _)),
    asserta(shortest_delay_multi(_, 100000, _)),
    
    % Gerar permutações de navios
    permutation(Vessels, SeqV),
    
    % Tentar diferentes combinações de guindastes
    % Começar com 1 guindaste para todos e incrementar quando necessário
    try_multi_crane_combinations(SeqV, SeqTriplets, CraneAllocation, Delay),
    
    % Comparar e guardar a melhor solução
    compare_shortest_delay_multi(SeqTriplets, Delay, CraneAllocation),
    fail.
obtain_seq_multi_crane_for_dock(_DockId, _Vessels, BestSchedule, MinDelay, BestCraneAlloc) :-
    retract(shortest_delay_multi(BestSchedule, MinDelay, BestCraneAlloc)), !.

% ============================================
% TRY MULTI-CRANE COMBINATIONS
% ============================================
% Para uma sequência de navios, tenta diferentes alocações de guindastes
% Usa backtracking do Prolog para testar todas as combinações
try_multi_crane_combinations(SeqV, SeqTriplets, CraneAllocation, TotalDelay) :-
    length(SeqV, N),
    % Gerar uma combinação de guindastes (1 ou 2 por navio)
    generate_crane_allocation(N, Cranes),
    % Calcular temporização com esta alocação
    sequence_temporization_multi(0, SeqV, Cranes, SeqTriplets, CraneAllocation),
    % Calcular atraso total
    sum_delays(SeqTriplets, TotalDelay).

% Gerar combinações de guindastes usando backtracking
% Começar com todos 1, depois tentar com 2 guindastes em diferentes posições
generate_crane_allocation(0, []) :- !.
generate_crane_allocation(N, [C|Rest]) :-
    N > 0,
    % Buscar número maximo de guindastes da database
    (   nb_getval(max_cranes, MaxCranes)
    ->  true
    ;   MaxCranes = 2  % fallback se não estiver definido
    ),
    % Gerar valores de 1 até MaxCranes
    between(1, MaxCranes, C),
    N1 is N - 1,
    generate_crane_allocation(N1, Rest).

% ============================================
% TEMPORIZATION WITH MULTIPLE CRANES
% ============================================
% sequence_temporization_multi(EndPrev, Vessels, Cranes, SeqTriplets, CraneAllocation)
% Calcula temporização considerando número de guindastes por navio
sequence_temporization_multi(_, [], [], [], []).
sequence_temporization_multi(EndPrev, [V|RestV], [NumCranes|RestCranes], 
                             [(V, TInUnload, TEndLoad)|RestTriplets],
                             [(V, NumCranes)|RestAlloc]) :-
    vessel(V, _, Arrival, _, Unload, Load),
    
    % Ajustar tempos baseado no número de guindastes
    % Regra: cada guindaste adicional divide o tempo por 2
    adjusted_time(Unload, NumCranes, AdjustedUnload),
    adjusted_time(Load, NumCranes, AdjustedLoad),
    
    % Calcular início (maior entre chegada e fim do anterior)
    (   Arrival > EndPrev
    ->  TInUnload is Arrival
    ;   TInUnload is EndPrev + 1
    ),
    
    % Calcular fim
    TEndLoad is TInUnload + AdjustedUnload + AdjustedLoad - 1,
    
    % Recursão
    sequence_temporization_multi(TEndLoad, RestV, RestCranes, RestTriplets, RestAlloc).

% Ajustar tempo baseado no número de guindastes
% Cada guindaste adicional divide o tempo por 2
adjusted_time(OriginalTime, 1, OriginalTime) :- !.
adjusted_time(OriginalTime, NumCranes, AdjustedTime) :-
    NumCranes > 1,
    % Calcular divisor: 2^(NumCranes-1)
    Divisor is 2 ** (NumCranes - 1),
    AdjustedTime is max(1, ceiling(OriginalTime / Divisor)).

% ============================================
% COMPARISON AND HELPER PREDICATES
% ============================================
compare_shortest_delay_multi(SeqTriplets, Delay, CraneAlloc) :-
    shortest_delay_multi(_, CurrentBest, _),
    (   Delay < CurrentBest
    ->  retract(shortest_delay_multi(_, _, _)),
        asserta(shortest_delay_multi(SeqTriplets, Delay, CraneAlloc))
    ;   true
    ).

% Reutilizar sum_delays do US 3.4.2
sum_delays([], 0).
sum_delays([(V, _, TEndLoad)|LV], S) :-
    vessel(V, _, _, Departure, _, _),
    TPossibleDep is TEndLoad + 1,
    (   TPossibleDep > Departure
    ->  SV is TPossibleDep - Departure
    ;   SV is 0
    ),
    sum_delays(LV, SLV),
    S is SV + SLV.
