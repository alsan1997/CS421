isprefix([], [_|_]).
isprefix([X|XS], [X|ZS]) :- isprefix(XS, ZS).
