# POTD 8 --- Lenses, Part 1: What is a Lens?

This problem series is all about lenses!  Lenses are a powerful
construct in functional programming, and they’re worth seeing at least
once.

A lens is an abstraction of a modifiable property.  Put another way, a
lens is like an accessor on an object, except without the object.
Lenses let you compose heavily nested modifications very simply, and
have deep mathematical connections with concepts in programming
language theory and design.

A lens is, conceptually, a pair of a getter and a setter.  The getter
takes a value that has something you want to extract from it, and
extracts that thing.  The setter takes another such value, plus
something you want to put into it at that same position, and then
returns the altered value.

For example, the getter could obtain a particular field of a record,
and the setter could alter that same field.  For another example, the
getter could put together an integer from a list of digits, and the
setter could split an integer back out into a list of digits again.
(In the latter case, the setter doesn’t need access to the “old”
version of the value, because the extracted thing is just a different
representation of the *whole* thing; believe it or not, so-called
“*bijective*” lenses like this are sometimes quite useful.)

However, lenses aren’t usually *implemented* this way; instead, they
are generally more compact.  The style of lens implementation we’ll be
using is a very simple but illustrative one, as well as one of the
oldest versions of lenses around.  (For those interested, the official
name for this style of lens implementation is “`Store`-comonad
coalgebras,” but we won’t worry about why in this exercise; it won’t
matter.)

The type of lenses has already been defined for you in `Lib.hs`:

```.haskell
newtype Lens s t = Lens (s -> (t, t -> s))
```

This means we get a type `Lens` that takes two type parameters: the
source type `s` (the type of values you’re extracting stuff *from*)
and the target type `t` (the type of values you expect to *get* out of
extractions).  We also have the data constructor `Lens`, which takes a
single function from `s` to a tuple of `t` and a function from `t` to
`s`.  The function will accept an initial value of the source type and
return both an extracted value of the target type and a function that
will let you put a new value of the target type back into the original
source value to get a new, altered source value. This is all we need
to get started with lenses!

Some example lenses on a simple 3D-point datatype have been already
implemented for you in the provided skeleton `Lib.hs`.  You can play
around with them in GHCi if you'd like.  We'll be using the following
example value throughout this documentation:

```.haskell
ghci> p1 = Point3D 10 4 9.5
```

## Problem 1 --- Implement `get`

While our chosen lens representation is fairly compact, it’s not so
helpful when we don’t care about setting, only getting.  As such, we
need to implement the function `get`:

```.haskell
get :: Lens s t -> s -> t
```

Here's some example usage:

```.haskell
ghci> xCoord p1
10.0
ghci> get xCoordLens p1
10.0
```

## Problem 2 --- Implement `set`

Likewise, we might not care about getting the existing value; we might
just want to override whatever’s there with a new value.  Accordingly,
we need to implement the function `set`:

```.haskell
set :: Lens s t -> s -> t -> s
```

Here's some example usage:

```.haskell
ghci> p1 { xCoord = 8.21 }
Point3D {xCoord = 8.21, yCoord = 4.0, zCoord = 9.5}
ghci> set xCoordLens p1 8.21
Point3D {xCoord = 8.21, yCoord = 4.0, zCoord = 9.5}
```

## Problem 3 --- Implement `makeLens`

Finally, we might find it easier, when constructing a new lens, to
write our getter and setter functions separately, and only afterwards
package them up into a lens.  Thus, we need to implement the function
`makeLens`:

```.haskell
makeLens :: (s -> t) -> (s -> t -> s) -> Lens s t
```

Here's some example usage:

```.haskell
ghci> xCoordLens' = makeLens (\point -> xCoord point) (\oldPoint newX -> oldPoint { xCoord = newX })
ghci> get xCoordLens' p1 == get xCoordLens p1
True
ghci> set xCoordLens' p1 8.21 == set xCoordLens p1 8.21
True
```
