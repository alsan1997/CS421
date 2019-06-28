sum([], 0).
sum([X|XS], S) :- sum(XS, Y), S is X + Y.
