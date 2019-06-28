Today you get to make a calculator!

You are given the following datatype:

```
data Tree =
     TInt Integer
   | TOp String Tree Tree
```

The `TInt` holds an integer value, and `TOp` holds an operator (encoded as a string) along with
two child trees, for the left and right arguments to the operator.  The operators will be from
the set `+`, `-`, and `*`.  We don't feel like dealing with division just now.

Your work is to write two functions:

```
swap :: Tree -> Tree  // reverses the order of children in a tree, recursively
calc :: Tree -> Integer  // Perfoms the calculations represented by the tree
```

Suppose we have this example tree:

```
t1 = TOp "*" (TOp "+" (TInt 20) (TInt 1))
             (TOp "-" (TInt 10) (TInt 8))
```

To print it out as text, there is a function called `show` for you. (Prelude is the prompt here;
yours may be different.)

```
Prelude> show t1
(* (+ 20 1) (- 10 8))
```

Calling `swap t1` will yield `(* (- 8 10) (+ 1 20))`, and calling `calc t1` will result in `42`.

Put the code for these functions in `src/Lib.hs`.
You can use `stack ghci src/Lib.hs` to load you code and test it, and `stack test` to run the given test
cases.
