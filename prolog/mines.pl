to_xo([], []).
to_xo([0|L], [o|M]) :- to_xo(L, M).
to_xo([1|L], [x|M]) :- to_xo(L, M).


mines_help([], [_,0]).
mines_help([X|V], [L,C,R|M]) :- 
	L in 0..1,
	C in 0..1,
	R in 0..1,
	X in 0..3,
	L+C+R #= X,
	mines_help(V, [C, R | M]).


mines(V, M) :- same_length(V, M), mines_help(V, [0|M1]), append(M0, [0], M1), to_xo(M0, M).

