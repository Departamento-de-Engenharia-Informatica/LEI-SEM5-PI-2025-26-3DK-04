% us_3_4_5.pl
% Implementação da US 3.4.5 - Multi-crane fallback scheduling
% SWI-Prolog

:- module(us_3_4_5, [
    schedule_vessels/7,
    best_order_single_crane/2,
    single_crane_schedule/3,
    multi_crane_schedule/5
]).

:- dynamic vessel/5.
% vessel(ID, Arrival, Departure, Unload, Load).
% Arrival/Departure are integer time units (e.g., hours).
% Unload, Load are durations in hours (integers).

% ---------- UTILITÁRIOS ----------
max_of(A,B,Max) :- (A >= B -> Max = A ; Max = B).

% integer ceiling for expressions - uses Prolog's ceiling/1 which accepts float
int_ceiling(X, C) :- C is ceiling(X).

sum_list_int([],0).
sum_list_int([H|T], S) :- sum_list_int(T, ST), S is H + ST.

% ---------- SEQUENCE TEMPORIZATION (single crane model) ----------
% sequence_temporization_for_order(OrderList, SeqTriplets).
% SeqTriplets = [(V,Start,End), ...]
sequence_temporization_for_order(Order, SeqTriplets) :-
    sequence_temporization_for_order1(0, Order, SeqTriplets).

sequence_temporization_for_order1(_, [], []).
sequence_temporization_for_order1(EndPrev, [V|LV], [(V,TInUnload,TEndLoad)|SeqTriplets]) :-
    vessel(V, Arrival, _, Unload, Load),
    ( (Arrival > EndPrev, !, TInUnload is Arrival) ; TInUnload is EndPrev + 1 ),
    TEndLoad is TInUnload + Unload + Load - 1,
    sequence_temporization_for_order1(TEndLoad, LV, SeqTriplets).

% ---------- CALCULO DE ATRASOS ----------
% sum_delays_from_seq(SeqTriplets, TotalDelay)
sum_delays_from_seq([], 0).
sum_delays_from_seq([(V,_,TEndLoad)|T], S) :-
    vessel(V, _, Departure, _, _),
    TPossibleDep is TEndLoad + 1,
    ( TPossibleDep > Departure -> SV is TPossibleDep - Departure ; SV is 0 ),
    sum_delays_from_seq(T, SL),
    S is SV + SL.

% ---------- ENCONTRAR MELHOR ORDEM (single-crane, força-bruta) ----------
% best_order_single_crane(Order, Delay)
% Gera todas as permutações e devolve a ordem com menor atraso (single-crane model).
best_order_single_crane(BestOrder, BestDelay) :-
    findall(V, vessel(V, _, _, _, _), LV),
    ( LV = [] -> BestOrder = [], BestDelay = 0
    ; best_order_search(LV, BestOrder, BestDelay)
    ).

best_order_search(LV, BestOrder, BestDelay) :-
    % inicialização com valor grande
    BestDelayInit = 1000000000,
    best_order_search1(LV, _, BestOrder, BestDelayInit, BestDelay).

best_order_search1(LV, _, BestOrderAcc, BestDelayAcc, BestDelayAcc) :-
    % predicate terminator used via backtracking in generator below
    !, fail.
best_order_search1(LV, _, _, _, _) :-
    % we will use permutation/2 to generate possibilities in the next clause
    permutation(LV, Perm),
    sequence_temporization_for_order(Perm, Seq),
    sum_delays_from_seq(Seq, Delay),
    % store the best using assert/retract would be possible, but here we do local accumulator:
    % We'll collect all (Delay,Perm) and then pick minimum afterwards - easier approach used below.
    fail.

% The above pattern with fail is cumbersome; simpler approach: collect all pairs then pick min
best_order_search(LV, BestOrder, BestDelay) :-
    findall(Delay-Order,
            ( permutation(LV, Order),
              sequence_temporization_for_order(Order, Seq),
              sum_delays_from_seq(Seq, Delay)
            ),
            Pairs),
    % select min
    sort(Pairs, Sorted), % sorts by Delay then Order
    Sorted = [BestDelay-BestOrder|_].

% ---------- SINGLE-CRANE SCHEDULE (interface) ----------
% single_crane_schedule(OrderOrSeq, SeqTriplets, TotalDelay)
% If OrderOrSeq is 'auto' then compute best order; otherwise assume it's a list with vessel ids.
single_crane_schedule(auto, SeqTriplets, TotalDelay) :-
    best_order_single_crane(Order, TotalDelay),
    sequence_temporization_for_order(Order, SeqTriplets).
single_crane_schedule(Order, SeqTriplets, TotalDelay) :-
    is_list(Order),
    sequence_temporization_for_order(Order, SeqTriplets),
    sum_delays_from_seq(SeqTriplets, TotalDelay).

% ---------- MULTI-CRANE SCHEDULING ----------
% multi_crane_schedule(OrderOrAuto, ResultSchedule, TotalDelay, ExtraCraneHours, MaxCranes)
% ResultSchedule = [(V,Start,End,CranesUsed), ...]
% MaxCranes default 5 if not provided.
multi_crane_schedule(auto, Result, TotalDelay, ExtraHours, MaxCranes) :-
    best_order_single_crane(Order, _),
    multi_crane_schedule(Order, Result, TotalDelay, ExtraHours, MaxCranes).
multi_crane_schedule(auto, Result, TotalDelay, ExtraHours) :-
    multi_crane_schedule(auto, Result, TotalDelay, ExtraHours, 5).

multi_crane_schedule(Order, Result, TotalDelay, ExtraHours) :-
    multi_crane_schedule(Order, Result, TotalDelay, ExtraHours, 5).

multi_crane_schedule(Order, Result, TotalDelay, ExtraHours, MaxCranes) :-
    multi_crane_schedule1(Order, 0, [], ResultRev, TotalDelay, 0, ExtraHours, MaxCranes),
    reverse(ResultRev, Result).

% multi_crane_schedule1(RestOrder, PrevEnd, Acc, ResultAcc, TotalDelayAcc, ExtraHoursAcc, ExtraHoursOut, MaxCranes)
multi_crane_schedule1([], _, Acc, Acc, 0, ExtraHoursAcc, ExtraHoursAcc, _).
multi_crane_schedule1([V|Rest], PrevEnd, Acc, Result, TotalDelayOut, ExtraHoursAcc, ExtraHoursOut, MaxCranes) :-
    vessel(V, Arrival, Departure, Unload, Load),
    NormalTime is Unload + Load,
    ( (Arrival > PrevEnd, !, Start is Arrival) ; Start is PrevEnd + 1 ),
    % Try single-crane first
    End1 is Start + NormalTime - 1,
    PossibleDep1 is End1 + 1,
    ( PossibleDep1 =< Departure ->
        % single crane sufficient
        CranesUsed = 1,
        EndFinal = End1,
        DelayHere = 0,
        ExtraHoursHere = 0
    ;
        % need additional cranes: try k = 2..MaxCranes, choose minimal k that eliminates delay, otherwise choose k that minimizes resulting delay
        findall( K-ResultInfo,
                 ( between(2, MaxCranes, K),
                   TimeKFloat is NormalTime / K,
                   int_ceiling(TimeKFloat, TimeK),
                   EndK is Start + TimeK - 1,
                   PossibleDepK is EndK + 1,
                   ( PossibleDepK =< Departure -> DelayK = 0 ; DelayK is PossibleDepK - Departure ),
                   % Extra crane-hours contributed by using K cranes (additional above 1)
                   DurationK is EndK - Start + 1,
                   ExtraH is (K - 1) * DurationK,
                   ResultInfo = info(K, TimeK, EndK, PossibleDepK, DelayK, ExtraH)
                 ),
                 Results),
        % select K with minimal DelayK; if multiple, pick smallest K (minimize crane usage)
        sort(2, @=<, Results, SortedByDelay), % sort by DelayK (positioned in compound), but safer to transform
        % We will select best by minimal DelayK then minimal K
        select_best_k(Results, BestK, BestEnd, BestDelayK, BestExtraH, Start),
        CranesUsed = BestK,
        EndFinal = BestEnd,
        DelayHere = BestDelayK,
        ExtraHoursHere = BestExtraH
    ),
    % accumulate and recurse
    % compute next PrevEnd as EndFinal
    TotalDelayNext is DelayHere, % we'll sum via recursion
    NewAcc = [(V, Start, EndFinal, CranesUsed) | Acc],
    ExtraHoursAcc2 is ExtraHoursAcc + ExtraHoursHere,
    multi_crane_schedule1(Rest, EndFinal, NewAcc, Result, TotalDelayRest, ExtraHoursAcc2, ExtraHoursOut, MaxCranes),
    TotalDelayOut is TotalDelayRest + TotalDelayNext.

% select_best_k(Results, BestK, BestEnd, BestDelayK, BestExtraH, Start)
% Results is list of ResultInfo = info(K, TimeK, EndK, PossibleDepK, DelayK, ExtraH)
select_best_k(Results, BestK, BestEnd, BestDelayK, BestExtraH, _Start) :-
    % Convert to Delay-K pairs for easy min selection
    findall(Delay-K-End-Extra,
            ( member(info(K, _TimeK, End, _PossibleDep, Delay, Extra), Results) ),
            Flat),
    % find minimal Delay
    sort(Flat, Sorted), % sorts by Delay then K then End etc.
    Sorted = [BestDelayK-BestK-BestEnd-BestExtraH | _].

% ---------- SUM DELAYS / EXTRA CRANE HOURS FROM SCHEDULE ----------
% sum_delays_from_multischedule(ResultSchedule, TotalDelay)
sum_delays_from_multischedule([], 0).
sum_delays_from_multischedule([(V,_,End,Cranes)|T], S) :-
    vessel(V, _, Departure, _, _),
    TPossibleDep is End + 1,
    ( TPossibleDep > Departure -> SV is TPossibleDep - Departure ; SV is 0 ),
    sum_delays_from_multischedule(T, SL),
    S is SV + SL.

% extra_crane_hours_from_schedule(ResultSchedule, ExtraHours)
extra_crane_hours_from_schedule([], 0).
extra_crane_hours_from_schedule([(V,Start,End,Cranes)|T], TotalExtra) :-
    Duration is End - Start + 1,
    ( Cranes > 1 -> ExtraH is (Cranes - 1) * Duration ; ExtraH is 0 ),
    extra_crane_hours_from_schedule(T, RestExtra),
    TotalExtra is ExtraH + RestExtra.

% ---------- TOP-LEVEL SCHEDULER ----------
% schedule_vessels(Day, SinglePlan, SingleDelay, MultiPlan, MultiDelay, ExtraCraneHours, Chosen)
% Day parameter currently unused (keeps interface compatible); scheduling uses dynamic vessel/5 facts.
schedule_vessels(_Day, SinglePlan, SingleDelay, MultiPlan, MultiDelay, ExtraCraneHours, Chosen) :-
    % Single-crane plan: auto find best order
    single_crane_schedule(auto, SinglePlan, SingleDelay),
    ( SingleDelay =:= 0 ->
        % no need for multi-crane
        MultiPlan = none,
        MultiDelay = SingleDelay,
        ExtraCraneHours = 0,
        Chosen = single
    ;
        % compute multi-crane schedule (auto order via best single-crane order)
        multi_crane_schedule(auto, MultiPlan, MultiDelay, ExtraCraneHours),
        % choose best: prefer lower delay; if equal prefer single to minimize crane usage
        ( MultiDelay < SingleDelay -> Chosen = multi
        ; Chosen = single
        )
    ).

% ---------- EXAMPLES / TESTS (comentados) ----------
/*
% Exemplos de dados (descomentar para testes)
:- retractall(vessel(_,_,_,_,_)).
:- assertz(vessel(v1, 1, 10, 4, 2)). % total 6
:- assertz(vessel(v2, 2, 8, 3, 1)).  % total 4
:- assertz(vessel(v3, 5, 14, 6, 3)). % total 9

% Teste:
% ?- schedule_vessels('2025-11-16', SinglePlan, SingleDelay, MultiPlan, MultiDelay, EH, Chosen).
*/

% FIM do ficheiro
