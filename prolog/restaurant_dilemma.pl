% David, Emma, Stella, and Thomas were at a restaurant in Prague. 
% They sat at a square table, with one person on each side, 
% and the men sat across the table from each other, as did the women. 
%Each of them ordered a different food along with a different beverage. 
% The person with cider sat across from the person with trout.
% The dumplings came with beer.
% The mushroom soup came with cider.
% The person with pasta sat across from the person with beer.
% David never drinks iced tea.
% Emma only drinks wine.
% Stella does not like dumplings.

all_dif(A,B,C,D) :- 
    dif(A,B),dif(A,C),dif(A,D),
    dif(B,C),dif(B,D),
    dif(C,D).

sits_across(david,thomas).
sits_across(thomas,david).
sits_across(stella, emma).
sits_across(emma, stella).

solve(Dumplings, Pasta, Soup, Trout) :-
    Pasta=Tea,
    all_dif(Dumplings, Pasta, Soup, Trout),
    all_dif(Cider,Beer,Wine,Tea),
    sits_across(Cider,Trout),
    Dumplings = Beer,
    Soup = Cider,
    sits_across(Pasta, Beer),
    dif(Tea, david),
    Wine=emma,
    dif(Dumplings, stella).

