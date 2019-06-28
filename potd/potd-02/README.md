POTD 2 --- Fun with Conditionals
==============================

Your work is to implement the `signum` and `if'` functions, which will give you practice with conditionals.

Given a number, the `signum` function will return -1 for a negative number, 1 for a positive number, and 0 otherwise.
```
signum (-5) == -1
signum 5 == 1
signum 0 == 0
```

The `if'` function is a replacement for the "if-then-else" syntactic sugar in Haskell. It takes in 3 parameters: a boolean value, the return value of the `then` statement, and the return value of the `else` statement.
```
if' (3 > 0) 1 2 == 1
if' (3 < 0) 1 2 == 2
```

Put the code for these functions in `src/Lib.hs`.
