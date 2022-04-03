% a) numerals(L, M) - true if L is a list of integers in the range 1 ≤ i ≤ 5 and M is a corresponding list of atoms 
% from the set [ one, two, three, four, five ].

eq(1,one).
eq(2, two).
eq(3,three).
eq(4,four).
eq(5,five).

numerals([],[]).
numerals([X|L],[Y|M]) :- eq(X,Y), numerals(L,M).

% b) pref(L, M) - true if L is a prefix of M.

pref([],_).
pref([X|L],[Y|M]) :- X=Y, pref(L,M).

% c) nth(N, L, X) - true if the nth element of list L is X, where the first element has index 0.

nth(0,[X|_],X).
nth(N,[_|L], X) :- N #= N0 + 1, nth(N0,L,X).

% d) concat(L, M, N) - true if the concatenation of the lists L and M is N.

concat([],[],[]).
concat([],[X|M],[Y|N]) :- X=Y, concat([],M,N).
concat([X|L],M,[Y|N]) :- X=Y, concat(L,M,N).

% e) sel(X, L, M) - true if X can be removed from L to make M. Equivalently, 
% sel(X, L, M) is true if X can be inserted anywhere in M to make L.

sel(X,[X|L],L).
sel(X,[Y|L],[Y|M]) :- sel(X,L,M).

% f) sel4(X, L, Y, M) - true if L and M are identical except for a single element, 
% which is X in L and is Y in M.

sel4(X,[X|L],Y,[Y|L]).
sel4(X,[Z|L],Y,[Z|M]) :- sel4(X,L,Y,M).

% g) dup(L, M) - true if M is obtained by repeating each element in L.

dup([],[]).
dup([X|L],[X,X|M]) :- dup(L,M).

% h) nums(I, J, L) - true if L is the list containing integers I through J, inclusive. 
% If J < I, then L should be the empty list.

nums(I,I,[I]).
nums(I,J,[I|L]) :- I#=< J, I + 1 #= I0, nums(I0,J,L).