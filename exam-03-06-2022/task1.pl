intsum(0, []).
intsum(N, [H|T]) :- 
    N #> 0, 
    between(1, N, H),
    N1 #= N - H,
    intsum(N1, T).

tree(N, T) :- treeHelp(N, N, T).

treeHelp(NN, N, t(V, C)) :- between(1, N, V), NN #> 0, NN0 #= NN-1, treeHelpList(NN0, N, C).

treeHelpList(0, _, []).
treeHelpList(NN, N, [ T | R]) :- 
    NN > 0, 
    between(1, NN, NT), treeHelp(NT, N, T), 
    NR #= NN-NT, treeHelpList(NR, N, R).

