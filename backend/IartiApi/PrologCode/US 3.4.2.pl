% sequence_temporization([a,b,c],SeqTriplets).
% Retorna todas as sequências de navios com a sua temporização associada
sequence_temporization(LV,SeqTriplets) :-
    sequence_temporization1(0,LV,SeqTriplets).

% Caso recursivo: calcula tempos para cada navio
sequence_temporization1(EndPrev,[V|LV],[(V,TInUnload,TEndLoad)|SeqTriplets]):-
    vessel(V,Arrival,_,Unload,Load), % Obtém dados do navio
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
    vessel(V,_,Departure,_,_), % Obtém hora de partida
    TPossibleDep is TEndLoad + 1, % Possível partida
    ((TPossibleDep > Departure,!,SV is TPossibleDep-Departure); SV is 0), % Calcula atraso
    sum_delays(LV,SLV), % Soma atrasos dos restantes
    S is SV + SLV.

% obtain_seq_shortest_delay(SeqBetterTriplets, SLowestDelay).
% Retorna a sequência de navios com o menor atraso
:- dynamic shortest_delay/2.

obtain_seq_shortest_delay(SeqBetterTriplets, SShortestDelay) :-
    (obtain_seq_shortest_delay1; true), % Executa busca por todas as permutações
    retract(shortest_delay(SeqBetterTriplets, SShortestDelay)), !.

% Gera todas as permutações de navios e calcula atrasos
obtain_seq_shortest_delay1 :-
    asserta(shortest_delay(_, 100000)),  % valor inicial muito alto
    findall(V, vessel(V, _, _, _, _), LV), % Lista todos os navios
    permutation(LV, SeqV), % Gera permutação
    sequence_temporization(SeqV, SeqTriplets), % Calcula tempos
    sum_delays(SeqTriplets, S), % Soma atrasos
    compare_shortest_delay(SeqTriplets, S), % Compara com o menor até agora
    fail. % força backtracking para testar todas as permutações

% Atualiza o menor atraso encontrado
compare_shortest_delay(SeqTriplets, S) :-
    shortest_delay(_, SLower),
    (S < SLower ->
        retract(shortest_delay(_, _)),
        asserta(shortest_delay(SeqTriplets, S))        
    ; true).