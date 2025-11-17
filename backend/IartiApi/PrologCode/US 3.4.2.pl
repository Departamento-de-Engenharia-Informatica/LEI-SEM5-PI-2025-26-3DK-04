% ============================================
% DADOS DOS NAVIOS
% vessel(ID, DockId, Arrival, Departure, Unload, Load)
% ============================================
% Os dados dos navios são agora carregados dinamicamente
% a partir da base de dados através do servidor (server.pl)
% Declaração dinâmica para permitir assertz/retract em runtime
:- dynamic vessel/6.

% ============================================
% LÓGICA DE AGENDAMENTO POR DOCK
% ============================================

% obtain_seq_shortest_delay(SeqBetterTriplets, SLowestDelay).
% Retorna a sequência de navios com o menor atraso GLOBAL (somando todos os docks)
:- dynamic shortest_delay/2.

obtain_seq_shortest_delay(SeqBetterTriplets, SShortestDelay) :-
    % Agrupar navios por dock
    findall(DockId, vessel(_, DockId, _, _, _, _), AllDocks),
    sort(AllDocks, UniqueDocks), % remove duplicados
    format(user_error, '[INFO] Found ~w unique docks: ~w~n', [length(UniqueDocks), UniqueDocks]),
    % Para cada dock, calcular o melhor agendamento
    obtain_best_schedule_per_dock(UniqueDocks, AllSchedules, TotalDelay),
    % Combinar todos os schedules
    flatten(AllSchedules, SeqBetterTriplets),
    SShortestDelay = TotalDelay.

% obtain_best_schedule_per_dock(+Docks, -AllSchedules, -TotalDelay)
% Para cada dock, encontra o melhor agendamento e soma os delays
obtain_best_schedule_per_dock([], [], 0).
obtain_best_schedule_per_dock([DockId|RestDocks], [BestSchedule|RestSchedules], TotalDelay) :-
    format(user_error, '[INFO] Processing dock: ~w~n', [DockId]),
    % Encontrar todos os navios deste dock
    findall(V, vessel(V, DockId, _, _, _, _), VesselsInDock),
    length(VesselsInDock, VesselCount),
    format(user_error, '[INFO] Dock ~w has ~w vessels~n', [DockId, VesselCount]),
    (   VesselsInDock = []
    ->  % Sem navios neste dock
        BestSchedule = [],
        DockDelay = 0
    ;   % Calcular melhor agendamento para este dock
        obtain_seq_shortest_delay_for_dock(DockId, VesselsInDock, BestSchedule, DockDelay)
    ),
    format(user_error, '[INFO] Dock ~w: delay = ~w~n', [DockId, DockDelay]),
    % Recursão para os restantes docks
    obtain_best_schedule_per_dock(RestDocks, RestSchedules, RestDelay),
    TotalDelay is DockDelay + RestDelay.

% obtain_seq_shortest_delay_for_dock(+DockId, +Vessels, -BestSchedule, -MinDelay)
% Calcula o melhor agendamento para um dock específico
obtain_seq_shortest_delay_for_dock(DockId, Vessels, BestSchedule, MinDelay) :-
    retractall(shortest_delay(_, _)),
    asserta(shortest_delay(_, 100000)),
    permutation(Vessels, SeqV),
    sequence_temporization(SeqV, SeqTriplets),
    sum_delays(SeqTriplets, S),
    compare_shortest_delay(SeqTriplets, S),
    fail.
obtain_seq_shortest_delay_for_dock(_DockId, _Vessels, BestSchedule, MinDelay) :-
    retract(shortest_delay(BestSchedule, MinDelay)), !.

% Atualiza o menor atraso encontrado
compare_shortest_delay(SeqTriplets, S) :-
    shortest_delay(_, SLower),
    (S < SLower ->
        retract(shortest_delay(_, _)),
        asserta(shortest_delay(SeqTriplets, S))        
    ; true).

% ============================================
% LÓGICA DE TEMPORIZAÇÃO (mantida igual)
% ============================================

% sequence_temporization([a,b,c],SeqTriplets).
% Retorna todas as sequências de navios com a sua temporização associada
sequence_temporization(LV,SeqTriplets) :-
    sequence_temporization1(0,LV,SeqTriplets).

% Caso recursivo: calcula tempos para cada navio
sequence_temporization1(EndPrev,[V|LV],[(V,TInUnload,TEndLoad)|SeqTriplets]):-
    vessel(V,_DockId,Arrival,_,Unload,Load), % Obtém dados do navio (agora com DockId)
    % O início é o maior entre a chegada e o fim do anterior
    ( (Arrival > EndPrev,!, TInUnload is Arrival); TInUnload is EndPrev + 1),
    % O fim é o início + tempos de carga/descarga - 1 (porque começa no início da hora)
    TEndLoad is TInUnload + Unload + Load - 1,
    % Chamada recursiva para o próximo navio
    sequence_temporization1(TEndLoad,LV,SeqTriplets).

% Caso base: lista vazia
sequence_temporization1(_,[],[]).

% sum_delays([a,b,c], S).
% Retorna a soma dos atrasos de uma sequência de navios
sum_delays([],0).

% Para cada navio, calcula atraso individual e soma recursivamente
sum_delays([(V,_,TEndLoad)|LV],S):-
    vessel(V,_DockId,_,Departure,_,_), % Obtém hora de partida (agora com DockId)
    TPossibleDep is TEndLoad + 1, % Possível partida
    ((TPossibleDep > Departure,!,SV is TPossibleDep-Departure); SV is 0), % Calcula atraso
    sum_delays(LV,SLV), % Soma atrasos dos restantes
    S is SV + SLV.