data Computation a = Result a
                   | Problem
                   deriving Show

instance Functor Computation where
  fmap f (Result a) = Result (f a)
  fmap f Problem = Problem

instance Applicative Computation where
  pure x = Result x

  (Result f) <*> (Result x) = Result (f x)
  _          <*> _         = Problem

instance Monad Computation where
  return a = Result a
  (>>=) f (Result a) = f a
  (>>=) _ _ = Problem