% carry_in, X, Y, result, carry_out
two_add(zero,zero,zero,zero,zero).
two_add(one,zero,zero,one,zero).
two_add(one,one,zero,zero,one).
two_add(one,zero,one,zero,one).
two_add(zero,one,zero,one,zero).
two_add(zero,zero,one,one,zero).
two_add(zero,one,one,zero,one).
two_add(one,one,one,one,one).

add(X3, X2, X1, X0, Y3, Y2, Y1, Y0, Z4, Z3, Z2, Z1, Z0) :-
    two_add(zero,X0,Y0,Z0,C0),
    two_add(C0,X1,Y1,Z1,C1),
    two_add(C1,X2,Y2,Z2,C2),
    two_add(C2,X3,Y3,Z3,Z4).