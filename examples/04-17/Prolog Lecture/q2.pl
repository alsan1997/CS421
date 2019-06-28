myappend([], YS, YS).
myappend([X|XS], YS, [X|ZS]) :- myappend(XS, YS, ZS).
