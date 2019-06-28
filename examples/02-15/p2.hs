data Quuz a = Quuz a

instance Show a => Show (Quuz a) where
  show (Quuz x) = "Quuz " ++ show x

instance Functor Quuz where
  fmap f (Quuz x) = Quuz (f x)

finc x = fmap (+1) x

instance Applicative Quuz where
  pure x = Quuz x
  Quuz f <*> Quuz x = Quuz (f x)

plus3 a b c = a + b + c
aplus a b c = plus3 <$> a <*> pure b <*> c

  -- aplus (Foo 10) (Foo 20)
  -- (+) <$> (Foo 10) <*> (Foo 20)
  -- (Foo (+ 10)) <*> (Foo 20)
  -- (Foo (+ 10 20))

data Couldbe a = Sure a
               | Nope

instance Show a => Show (Couldbe a) where
  show (Sure a) = "Sure " ++ show a
  show Nope = "Nope"

instance Eq a => Eq (Couldbe a) where
  (==) (Sure x) (Sure y) = x == y
  Nope == Nope = True
  _ == _ = False

instance Functor Couldbe where
  fmap f (Sure x) = Sure (f x)
  fmap _ Nope = Nope

instance Applicative Couldbe where
  pure thing = Sure thing
  Sure f <*> Sure x = Sure (f x)
  _      <*> _      = Nope

data Weird a = Foo a
             | Bar a a
             | Baz

instance Show a => Show (Weird a) where
  show (Foo x) = "Foo " ++ show x
  show (Bar x y) = "Bar " ++ show x ++ " " ++ show y
  show Baz = "Baz"

instance Eq a => Eq (Weird a) where
  (==) (Foo x) (Foo y) = x == y
  (==) (Bar x1 y1) (Bar x2 y2) = x1 == x2 && y1 == y2
  Baz == Baz = True
  _ == _ = False

instance Functor Weird where
  fmap f (Foo x) = Foo (f x)
  fmap f (Bar x y) = Bar (f x) (f y)
  fmap _ Baz = Baz

instance Applicative Weird where
  pure x = Foo x
  Foo f <*> Foo x = Foo (f x)
  Foo f <*> Bar x y = Bar (f x) (f y)
  Bar f g <*> Foo x = Bar (f x) (g x)
  Bar f g <*> Bar x y = Bar (f x) (g y)
  _  <*>  _ = Baz

