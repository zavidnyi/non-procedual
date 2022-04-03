all_months(Months) :-
    Months = [jan: 31, feb: 28, mar: 31, apr: 30,
              may: 31, jun: 30, jul: 31, aug: 31,
              sep: 30, oct: 31, nov: 30, dec: 31].

since_start_of_the_year(date(D, jan), D).
since_start_of_the_year(date(D,M), N) :- 
    N #= NN + D, 
    all_months(Months), 
    nextto(MN: DN, M: _, Months), 
    since_start_of_the_year(date(DN,MN), NN).

add_date(date(D0, M0), N, date(D1, M1)) :- 
    all_months(Months), member(M0: L0, Months), member(M1: L1, Months),
    D0 in 1..L0, D1 in 1..L1,
    N #= N1 - N0,
    since_start_of_the_year(date(D0,M0), N0), 
    since_start_of_the_year(date(D1,M1), N1).
