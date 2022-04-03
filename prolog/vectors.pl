% a) vec_sum(V, W, Z) - true if the sum of the vectors V and W is Z.

vec_sum([],[],[]).
vec_sum([A|V],[B|W],[R|Z]) :- {A+B=R}, vec_sum(V,W,Z).

% b) vec_len(V, X) - true if the length of vector V is X.

vec_len_h([],X,S) :- {X = S^0.5}.
vec_len_h([E|V], X, S) :- {SN = S + E^2}, vec_len_h(V,X,SN). % S is the sum calculated by us
vec_len(V,X) :-  vec_len_h(V, X, 0).

% c) mul(X, V, W) - true if the scalar X multiplied by the vector V is the vector W.

mul(_,[],[]).
mul(X,[Y|V],[Z|W]) :- {Y*X=Z}, mul(X,V,W).

% d) dot(V, W, D) - true if the dot product of vectors V and W is the scalar D.

dot([],[],0).
dot([X|V],[Y|W],D) :- {D0 = D - X*Y}, dot(V,W,D0).

% e) angle(V, W, A) - true if the angle between vectors V and W is A (in radians), where 0 <= A <= π.

angle(V,W,A) :- dot(V,W,D),vec_len(V,LV),vec_len(W,LW),{D=LV*LW*cos(A)}.