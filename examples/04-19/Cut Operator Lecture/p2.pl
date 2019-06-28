friends(a, b).
friends(a, d).
friends(b, c).
friends(b, d).
friends(c, x).
friends(c, y).
friends(d, x).
friends(d, y).

friends(A, B) :- friends(B, A).
