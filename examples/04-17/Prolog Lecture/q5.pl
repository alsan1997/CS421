pathCost(X, X, 0).
pathCost(X, Y, C) :- connected(X, Z, K), pathCost(Z, Y, CS), C is K + CS.



connected(a, b, 10).
connected(b, c, 20).
connected(c, d, 1).
connected(a, x, 4).
