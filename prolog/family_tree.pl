great_grandmother(G,X) :-
        female(G),
        parent(G, C),parent(C,P), parent(P,X).

sibling(X, Y) :- dif(X,Y), parent(P,X),parent(P,Y).

full_sibling(X, Y) :- 
        dif(X,Y),
        dif(P,W),
        parent(P,X), parent(W,X),
        parent(P,Y), parent(W,Y).

first_cousin(X, Y) :- full_sibling(P1,P2),parent(P1,X),parent(P2, Y).

second_cousin(X, Y) :- first_cousin(P1,P2), parent(P1,X),parent(P2, Y).

nth_cousin(X, Y) :- first_cousin(X,Y).
nth_cousin(X, Y) :- parent(P1,X),parent(P2, Y), nth_cousin(P1,P2).
