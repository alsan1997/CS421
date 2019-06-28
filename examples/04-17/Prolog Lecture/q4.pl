pathFrom(X, X, [X]).
pathFrom(X, Y, [X,Z|LS]) :- connected(X, Z), pathFrom(Z, Y, [Z|LS]).



connected(a, b).
connected(b, c).
connected(b, e).
connected(e, d).
connected(c, d).
connected(a, x).
