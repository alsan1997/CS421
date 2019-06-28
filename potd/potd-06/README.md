POTD 6 --- Custom Infix Operators
==============================

Your work is to implement three custom infix operators, which are all related to set theory. Infix operators are really just normal functions, and we can implement them as such!

## Crash-course on infix operators
------

To get you started, we'll give you a quick crash-course on infix operators.

Here is a definition of a custom infix operator that multiplies two numbers together and increments the result:

```
infixl 5 *+
(*+) :: Integer -> Integer -> Integer
x *+ y = x*y + 1
```

In the first line, we are defining the associativity of the operator, along with its precedence. Infix operators can be left-associative (using the `infixl` keyword), right-associative (`infixr`), or have no associativity (`infix`). The number determines the precedence in the order of operations, which can range from `0-9`, where `0` is lowest precedence and `9` is highest precedence.

In the second and third lines, we are defining the type signature of the operator, along with the function definition itself. Note how the function definition already uses the operator in an infix form. 

Alternatively, you could define this operator in a prefix form too.

```
infixl 5 *+
(*+) :: Integer -> Integer -> Integer
(*+) x y = x*y + 1
```

Use the `:info` command in GHCi to get information about a function or operator, including its associativity and precedence.
```
Prelude> :info *
class Num a where
  ...
  (*) :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Num’
infixl 7 *
```

Since the `*` and `+` operators have precedences of `7` and `6`, respectively, then this means that `*` takes higher precedence over `+`, which takes higher precedence over `*+`.

Here are examples of how `*+` can be used:
```
Prelude> 2 *+ 3
7
Prelude> 2 *+ 3 * 2
13
Prelude> 2 *+ 3 * 2 + 1
15
```

## Set Theory
------

For this POTD, you will be implementing infix operators for union, intersection, and cartesian product for sets. We will be using Haskell lists to represent sets. Our sets will be sorted and have no duplicates.

Make union and intersection have left-associativity, and cartesian product have right-associativity. `x` should have higher precedence over `/\`, which should have higher precedence over `\/`, which should have higher precedence over `:` (the Cons operator).

### Union: `\/`

This operator unions the two input sets.
```
Prelude> [] \/ []
[]
Prelude> [1,2,3,4] \/ []
[1,2,3,4]
Prelude> [1,5,9,13] \/ [1,5,9,13]
[1,5,9,13]
Prelude> [2,4,9,15] \/ [1,3,8,14,17]
[1,2,3,4,8,9,14,15,17]
```

### Intersection: `/\`

This operator intersects the two input sets.
```
Prelude> [] /\ []
[]
Prelude> [] /\ [1,2,3,4]
[]
Prelude> [2,3,7,9,16] /\ [1,2,4,9,15,16]
[2,9,16]
Prelude> [1,4,6,8] /\ [3,5,7,9]
[]
```

### Cartesian product: ``` `x` ```

This operator takes the cartesian product of the two input sets.
```
Prelude> [] `x` []
[]
Prelude> [] `x` [3,6,7,8]
[]
Prelude> [2,5,6,7] `x` [1,3,4,8]
[(2,1),(2,3),(2,4),(2,8),(5,1),(5,3),(5,4),(5,8),(6,1),(6,3),(6,4),(6,8),(7,1),(7,3),(7,4),(7,8)]
Prelude> [5,7,12,16,19] `x` [2,6,11]
[(5,2),(5,6),(5,11),(7,2),(7,6),(7,11),(12,2),(12,6),(12,11),(16,2),(16,6),(16,11),(19,2),(19,6),(19,11)]
```

### Using all of the operators together:
```
Prelude> [1,2,3,4] /\ [2,4] \/ [1,3]
[1,2,3,4]
Prelude> 3 : [1,5,6,7] /\ [5,7,8,9]
[3,5,7]
Prelude> [3,6,10] `x` [4,8,9] \/ [5,7] `x` [2,3,6,7]
[(3,4),(3,8),(3,9),(5,2),(5,3),(5,6),(5,7),(6,4),(6,8),(6,9),(7,2),(7,3),(7,6),(7,7),(10,4),(10,8),(10,9)]
```

Put the code for these functions in `src/Lib.hs`.
