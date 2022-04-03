to_ints([],[]).
to_ints([o|L],[0|M]) :- to_ints(L,M).
to_ints([x|L],[1|M]) :- to_ints(L,M).

mines_help([0,0],[]).
mines_help([L|V], [o|M]) :- L #= 0, mines_help(V,M).
mines_help([L,C,R| V], [x|M]) :- L - 1 #= 0, C1 #= C - 1, R1 #= R-1, mines_help([C1,R1| V], M). 
mines(V, M) :- length(V,VL), length(M, ML), ML #= VL,N0=[-1|V], append(N0, [-1], N1),mines_help(N1, M1).