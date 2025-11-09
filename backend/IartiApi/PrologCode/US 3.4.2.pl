# ?- permutation([a,b,c],LP). para encontrar todas as possiveis sequencias de vessels

# ?- findall(LP,permutation([a,b,c],LP),LLP). para armazenar todas as possiveis sequencias de vessels numa lista

# ?- sequence_temporization([a,b,c],SeqTriplets). retorna todas a sequencia de vessels com a sua temporizacao associada

sequence_temporization(LV,SeqTriplets) :-
    sequence_temporization1(0,LV,SeqTriplets).

sequence_temporization1(EndPrevSeq,[V|LV],[(V,TInUnload,TEndLoad)|SeqTriplets]):-
    vessel(V,TIn,_,TUnload,TLoad),
    # O início é o maior entre a chegada e o fim do anterior
    ( (TIn> EndPrevSeq,!, TInUnload is TIn); TInUnload is EndPrevSeq+1),
    # O fim é o início + tempos de carga/descarga - 1 (porque começa no início da hora)
    TEndLoad is TInUnload + TUnload+TLoad - 1,
    # Chamada recursiva para o próximo navio
    sequence_temporization1(TEndLoad,LV,SeqTriplets).

sequence_temporization1(_,[],[]).

# ?- sum_delays([a,b,c], S). retorna a soma dos atrasos de uma sequencia de vessels

sum_delays([],0).

sum_delays([(V,_,TEndLoad)|LV],S):-
    vessel(V,_,TDep,_,_),TPossibleDep is TEndLoad+1,
    ((TPossibleDep>TDep,!,SV is TPossibleDep-TDep);SV is 0),
    sum_delays(LV,SLV),
    S is SV+SLV.