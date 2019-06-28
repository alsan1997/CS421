data Quux a = Quux a

instance Functor Quux where
  fmap f (Quux x) = Quux (f x)

instance Applicative Quux where
  pure x = Quux x
  (Quux f) <*> (Quux x)  = Quux (f x)

plus a b = a + b
inc x = x + 1

aplus x y = plus <$> x <*> y

data Couldbe a = Sure a
               | Nope

instance Show a => Show (Couldbe a) where 
  show (Sure x) = "Sure " ++ show x
  show Nope  = "Nope"

instance Eq a => Eq (Couldbe a) where
  Sure x == Sure y = x == y
  Nope == Nope = True
  _ == _ = False

instance Functor Couldbe where
  fmap f (Sure x) = Sure (f x)
  fmap _ Nope = Nope

instance Applicative Couldbe where
  pure x = Sure x
  (Sure f) <*> (Sure x) = Sure (f x)
  _ <*> _ = Nope


data Weird a = Foo a
             | Bar a a
             | Baz

instance (Show a) => Show (Weird a) where
  show (Foo x) = "Foo " ++ show x
  show (Bar x y) = "Bar " ++ show x ++ " " ++ show y
  show Baz = "Baz"

instance Eq a => Eq (Weird a) where 
  (Foo a) == (Foo b) = a == b
  (Bar a1 b1) == (Bar a2 b2) = a1 == a2 && b1 == b2
  Baz == Baz = True
  _ == _ = False

instance Functor Weird where
  fmap f (Foo x) = Foo (f x)
  fmap f (Bar x y) = Bar (f x) (f y)
  fmap f Baz  = Baz

instance Applicative Weird where
  pure x = Foo x
  Foo f <*> Foo x = Foo (f x)
  Foo f <*> (Bar a b) = Bar (f a) (f b)
  (Bar f g) <*> Foo x = Bar (f x) (g x)
  (Bar f g) <*> (Bar x y) = Bar (f x) (g y)
  _ <*> _ = Baz


  

--fmap inc (Foo 10)  -- Foo 11
--fmap plus (Foo 10)  -- Foo (plus 10)

--plus <$> (Foo 10) <*> (Foo 32)
-- (Foo (plus 10)) <*> (Foo 32)
-- Foo (plus 10 32)  ->  Foo 42

