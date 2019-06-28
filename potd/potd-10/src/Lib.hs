{-# LANGUAGE InstanceSigs #-}
module Lib
  ( Lens(..)
  , Point3D(..)
  , xCoordLens
  , yCoordLens
  , zCoordLens
  , makeLens
  , fstLens
  , sndLens
  , entryLens
  , get
  , set
  , modify
  , modifyMap
  ) where
import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Trans.State.Lazy (State)
import qualified Control.Monad.Trans.State.Lazy as S
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

newtype Lens s t = Lens (s -> (t, t -> s))

data Point3D n = Point3D
                 { xCoord :: n
                 , yCoord :: n
                 , zCoord :: n
                 }
  deriving (Eq, Show, Read, Bounded)

xCoordLens :: Lens (Point3D n) n
xCoordLens = Lens $ \v -> (xCoord v, \d -> v { xCoord = d })

yCoordLens :: Lens (Point3D n) n
yCoordLens = Lens $ \v -> (yCoord v, \d -> v { yCoord = d })

zCoordLens :: Lens (Point3D n) n
zCoordLens = Lens $ \v -> (zCoord v, \d -> v { zCoord = d })

get :: Lens s t -> s -> t
get (Lens l) = fst . l

set :: Lens s t -> s -> t -> s
set (Lens l) = snd . l

makeLens :: (s -> t) -> (s -> t -> s) -> Lens s t
makeLens gf sf = Lens $ \v -> (gf v, sf v)

fstLens :: Lens (a, b) a
fstLens = makeLens fst (\(_, y) x -> (x, y))

sndLens :: Lens (a, b) b
sndLens = makeLens snd (\(x, _) y -> (x, y))

entryLens :: Ord k => k -> Lens (Map k v) (Maybe v)
entryLens ix = makeLens (M.lookup ix) $ \v d ->
  case d of
    Nothing -> M.delete ix v
    Just d' -> M.insert ix d' v

-- Your code here!

modify :: Lens s t -> (t -> t) -> s -> s
modify = undefined

modifyMap :: Functor f => Lens s t -> (t -> f t) -> s -> f s
modifyMap = undefined

instance Category Lens where
  id :: Lens s s
  id = undefined
  (.) :: Lens t v -> Lens s t -> Lens s v
  (.) = undefined
