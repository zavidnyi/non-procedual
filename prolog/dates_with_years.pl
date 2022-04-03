all_months(Months, 0) :-
    Months = [jan: 31, feb: 28, mar: 31, apr: 30,
              may: 31, jun: 30, jul: 31, aug: 31,
              sep: 30, oct: 31, nov: 30, dec: 31].
all_months(Months, 1) :-
    Months = [jan: 31, feb: 29, mar: 31, apr: 30,
              may: 31, jun: 30, jul: 31, aug: 31,
              sep: 30, oct: 31, nov: 30, dec: 31].

% N is 1 if Year is a leap year, otherwise 0.
leap(Year, N) :- N #<==> Year mod 400 #= 0 #\/ Year mod 4 #= 0 #/\ Year mod 100 #\= 0.

since_start_of_the_year(date(D, jan), D, _).
since_start_of_the_year(date(D,M), N, L) :- 
    N #= NN + D, 
    all_months(Months, L), 
    nextto(MN: DN, M: _, Months), 
    since_start_of_the_year(date(DN,MN), NN, L).


days_through(1, 365).
days_through(Y, N) :- Y in 2..3000, Y0 + 1 #= Y, leap(Y0, L),days_through(Y0, NN),  N #= NN + 365 + L. 

add_date(date(D0, M0, Y0), N, date(D1, M1, Y1)) :- 
    leap(Y1, L),
    all_months(Months, L), member(M0: L0, Months), member(M1: L1, Months),
    D0 in 1..L0, D1 in 1..L1, Y0 in 1..3000, Y1 in 1..3000,
    N #= YN1 + N1 - YN0 - N0,
    days_through(Y0, YN0),
    since_start_of_the_year(date(D0,M0), N0, L), 
    days_through(Y1, YN1),
    since_start_of_the_year(date(D1,M1), N1, L).
