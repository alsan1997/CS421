POTD 7 --- Tower of Hanoi
==============================

Your work is to solve [Tower of Hanoi](https://en.wikipedia.org/wiki/Tower_of_Hanoi)
problem using the concept of fixed-point computation. That is, you will use the
`fix'` function given below for your implementation.

``` {.haskell}
type Stack  = [Int]
type Config = (Stack, Stack, Stack)
data Hanoi = Hanoi Config [Config]
    deriving Show

instance Eq Hanoi where
    (==) (Hanoi c _) (Hanoi c' _) = c == c'

instance Hashable Hanoi where
    hashWithSalt s (Hanoi c prev) = s `hashWithSalt` c

fix' :: Eq a => (a -> a) -> a -> a
fix' f x = if g == x
           then g
           else fix' f g
    where g = f x

initConf = ([1,2,3], [], [])
initSet = H.singleton (Hanoi initConf [])

findPath :: Config -> Maybe [Config]
findPath conf
    = case H.toList $ H.filter (Hanoi conf [] ==) allReachSet of
        [Hanoi _ path] -> Just $ reverse $ conf:path
        _   -> Nothing
```

We model a configuration of type `Config` as a tuple of three integer stacks.
The type `Hanoi` is a wrapper type over `Config` that also records the history
since the initial configuration with a list `Config`. Notice how we implement
the `Eq` typeclass for `Hanoi` that we only compare `Config` and ignore the
history. Observer that similarly, we implemented the `Hashable` typeclass
ignoring the history.

`fix'` computes the fixed-point of a given function `f` with an initial value
`x`.  `initSet` is the initial configurations of Hanoi, and `findPath` should
find a path from any initial configuraion to a given configuration.

Problems
--------

You will be implementing the following functions `move`, `moveSet`,
`oneMoveReachSet` and `allReachSet`.

`move` takes as input a `Hanoi` type and returns a `HashSet` of all possible
`Hanoi` types after one move has been performed. Remember to add the current
configuration to the history.

```
move (Hanoi ([1], [2], [3]) [])
        == H.fromList [Hanoi ([],[2],[1,3]) [([1],[2],[3])],
                       Hanoi ([],[1,2],[3]) [([1],[2],[3])],
                       Hanoi ([1],[],[2,3]) [([1],[2],[3])]]
```

To help you implement `move`, you can write a helper function `movePiece`.
`movePiece` takes in two `Stack`s and returns a tuple with the top item in the
first `Stack` moved to the top of the 2nd `Stack` if the item is not larger than
the top of the 2nd `Stack`. If not, it returns the original `Stack`s as a tuple.
Note that his helper function is optional. There is no test for this function.

```
movePiece [1,2] [3] == ([2], [1,3])
movePiece [3] [2] == ([3], [2])
movePiece [1] [] == ([], [1])
```


`moveSet` is similar to `move` except that it takes as input a `HashSet` of
members of the `Hanoi` type and returns a `HashSet` containing all possible
configurations after one move have been performed.

```
moveSet (H.fromList [Hanoi ([1], [2], [3]) [],
                     Hanoi ([1, 2], [], [3]) []])
    == H.fromList [Hanoi ([2],[1],[3]) [([1,2],[],[3])],
                   Hanoi ([1,2],[3],[]) [([1,2],[],[3])],
                   Hanoi ([2],[],[1,3]) [([1,2],[],[3])],
                   Hanoi ([],[2],[1,3]) [([1],[2],[3])],
                   Hanoi ([],[1,2],[3]) [([1],[2],[3])],
                   Hanoi ([1],[],[2,3]) [([1],[2],[3])]]
```

`oneMoveReachSet` is a function taking a `HashSet` as input and return the
`HashSet` of all configurations reachable with at most one move taken (i.e.
don't move or move once).

```
oneMoveReachSet $ H.fromList [Hanoi ([1], [2], [3]) []]
        == H.fromList [Hanoi ([],[2],[1,3]) [([1],[2],[3])],
                       Hanoi ([],[1,2],[3]) [([1],[2],[3])],
                       Hanoi ([1],[],[2,3]) [([1],[2],[3])],
                       Hanoi ([1], [2], [3]) []]
```

`allReachSet` is just the set containing all reachable configurations from
`initSet`. Notice that `allReachSet` will be exactly the fixed-point of
`oneMoveReachSet` because no more reachable configurations will be added into
`allReachSet` even applying `oneMoveReachSet` more times.

Put the code for these functions in `src/Lib.hs`.
