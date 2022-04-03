my_graph(G) :- G =
    [a -> [b, c], b -> [a, c, e, f], c -> [a, b, d, e, f],
     d -> [c, f], e -> [b, c, f], f -> [b, c, d, e]].

subseq(_,[]).
subseq([X|L],[X|M]) :- subseq(L,M).
subseq([_|L],M) :- subseq(L,M).

clique(_,0,[]).
clique([V -> L|G], N, [V|C]) :-  subseq(L, C),  N1 + 1 #= N, clique(G,N1,C).
clique([_ -> _|G], N, C) :- clique(G,N,C).
