data Foo a = Good
           | Bad
           | Ugly a
  deriving Show

instance Functor Foo where
   fmap f (Ugly a) = Ugly(f a)
   fmap f Good = Good
   fmap f Bad = Bad
