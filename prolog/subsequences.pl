% a) Write a predicate sublist(L, M) that is true if M is a sublist of L, i.e. 
% the elements of M appear contiguously somewhere inside L. A sublist must always contain at least one element

sublist(L,M) :- 
    length(L, N0),
    between(1, N0, N1),
    length(M, N1),
    append([_,M,_], L).

% b) Write a predicate subseq(L, M) that is true if M is a subsequence of L, i.e. 
% the elements of M appear in the same order inside L, but not necessarily contiguously. 
% A subsequence may be empty.

subseq(_,[]).
subseq([X|L],[X|M]) :- subseq(L,M).
subseq([_|L],M) :- subseq(L,M).

% c) Write a predicate disjoint(L, M, N) that is true if M and N are disjoint subsequences of L. 
% This means that M and N must be subsequences (as defined in part (b) above) 
% and that M and N together must contain all the elements of L. 
% For simplicity, you may assume that all elements of L are distinct. 

compose([],[],[]).
compose([X|L],[X|M],N) :- compose(L,M,N).
compose([X|L],M,[X|N]) :- compose(L,M,N).
disjoint(L,M,N) :- compose(L,M,N), subseq(L,M), subseq(L,N).