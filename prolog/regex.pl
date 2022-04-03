match([], []).

match([one(L) | E], S) :- append(L, S0, S), match(E, S0).

match([plus(L) | E], S) :-
	(append(L, S0, S), match([star(L) | E], S0))
	;
	(append(L, S0, S), match(E, S0)).

match([opt(L) | E], S) :-
	(match(E, S))
	;
	(append(L, S0, S), match(E, S0)).

match([star(L) | E], S) :-
	(match(E, S))
	;
	(append(L, S0, S), match([star(L) | E], S0))
	;
	(append(L, S0, S), match(E, S0)).
