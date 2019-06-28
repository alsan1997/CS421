# POTD 9 --- Lenses, Part 2: Some Basic Lenses

Now that we can use and construct lenses more easily, we should go
ahead and make some specific lenses to demonstrate the pattern.

## Problem 1 --- Implement `fstLens` and `sndLens`

The standard Haskell `fst` and `snd` functions for accessing the two
parts of a pair are both naturally the getter functions of their own
lenses; the corresponding setters simply replace that part of the pair
with the new target value, keeping the other part of the pair
unchanged.  Try implementing them!

```.haskell
fstLens :: Lens (a, b) a
sndLens :: Lens (a, b) b
```

Here is some example usage:

```.haskell
> get fstLens (4, "a")
4
> set fstLens (4, "a") 5
(5,"a")
> get sndLens (4, "a")
"a"
> set sndLens (4, "a") "b"
(4,"b")
```

## Problem 2 --- Implement `entryLens`

This lens is more complicated, but *extremely* useful in practical
settings; it takes as an initial parameter a key, and then gets and
sets the value or absence thereof at that key in any `Map`.  Notice
that the target type of this lens is wrapped in `Maybe`; a `Nothing`
means that the key wasn’t present (on get) or shouldn’t be present (on
set), while a `Just` means that the key *was* present (on get) or
*should* be present (on set) with the specified value.  This might
take some thinking to implement:

```.haskell
entryLens :: Ord k => k -> Lens (Map k v) (Maybe v)
```

Here is some example usage:

```.haskell
ghci> m = M.fromList [("a", 4), ("b", 5)]
ghci> get (entryLens "a") m
Just 4
ghci> get (entryLens "c") m
Nothing
ghci> set (entryLens "a") m (Just 6)
fromList [("a",6),("b",5)]
ghci> set (entryLens "a") m Nothing
fromList [("b",5)]
ghci> set (entryLens "c") m (Just 6)
fromList [("a",4),("b",5),("c",6)]
```
