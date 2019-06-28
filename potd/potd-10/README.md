# POTD 10 --- Lenses, Part 3: Additional Utility Combinators

While, in principle, we can do everything we want using only `get`,
`set`, and `makeLens`, there are some other common lens combinators
that can make our lives easier in certain situations.

As before, we'll be using the following example value throughout this
documentation:

```.haskell
ghci> p1 = Point3D 10 4 9.5
```

## Problem 1 --- Implement `modify`

This combinator is fairly straightforward to describe: it’s like
`set`, but rather than simply overwriting the target value with the
given one, it allows computation of the new target value from the old
one.  Consequently, it takes a function from the target type to the
target type in order to capture that computation.  See if you can
implement `modify`:

```.haskell
modify :: Lens s t -> (t -> t) -> s -> s
```

Here is some example usage:

```.haskell
ghci> modify xCoordLens (+pi) p1
Point3D {xCoord = 13.141592653589793, yCoord = 4.0, zCoord = 9.5}
```

## Problem 2 --- Implement `modifyMap`

This combinator’s type looks very weird at first.  The best intuition
for it initially is to imagine that the `Functor` `f` is specifically
`[]` (that is, the list functor), in which case `modifyMap` takes a
function from the old target value to potentially many new target
values, and the corresponding modifications of the source values are
also fanned out into an equally sized list.  It’s important to
remember, however, that `f` can be *any* `Functor`, not just `[]`; can
you think of interesting use cases for other type constructors that
`f` might be instantiated with?  (Here’s a hint: this combinator is
actually general enough to implement *every other lens combinator* by
itself.)  Can you figure out how to implement `modifyMap`?

```.haskell
modifyMap :: Functor f => Lens s t -> (t -> f t) -> s -> f s
```

Here is some example usage:

```.haskell
ghci> modifyMap xCoordLens (\oldX -> [oldX + 1, oldX + pi, 34, oldX]) p1
[Point3D {xCoord = 11.0, yCoord = 4.0, zCoord = 9.5},Point3D {xCoord = 13.141592653589793, yCoord = 4.0, zCoord = 9.5},Point3D {xCoord = 34.0, yCoord = 4.0, zCoord = 9.5},Point3D {xCoord = 10.0, yCoord = 4.0, zCoord = 9.5}]
ghci> modifyMap xCoordLens (\oldX -> \additionalArg -> oldX * additionalArg) p1 $ 40
Point3D {xCoord = 400.0, yCoord = 4.0, zCoord = 9.5}
ghci> modifyMap xCoordLens (\oldX -> (oldX, oldX)) p1
(10.0,Point3D {xCoord = 10.0, yCoord = 4.0, zCoord = 9.5})
ghci> modifyMap xCoordLens (\oldX -> Nothing) p1
Nothing
```

## Problem 3 --- Implement `instance Category Lens`

The Haskell standard library has a handy type class called `Category`
that describes type constructors of two arguments for which:

- There is an “empty” or “trivial” value.
- There is a linear composition of some kind.

Functions (that is, the type constructor `(->)`) implement `Category`,
and so do monads (in the form of Kleisli arrows).  Lenses also can
implement `Category`; the signatures of the two class methods needed
are given below, specialized to apply to `Lens`.  The `id` `Lens`
should be the trivial “accessor” that just gets and sets the entire
source value wholesale; the `(.)` operator for `Lens` should perform
lens composition, such that the result of composing two lenses is a
lens that acts as a nested accessor that gets and sets through both of
its components in the appropriate order:

```.haskell
id :: Lens s s
(.) :: Lens t v -> Lens s t -> Lens s v
```

Here is some example usage for `id`:

```.haskell
ghci> get id 6
6
ghci> set id 6 5
5
```

Here is some example usage for `(.)`:

```.haskell
ghci> :t (fstLens . sndLens)
(fstLens . sndLens) :: Lens (a, (c, b)) c
ghci> get (fstLens . sndLens) (4, ("a", True))
"a"
ghci> set (fstLens . sndLens) (4, ("a", True)) "b"
(4,("b",True))
```
