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
  ) where
import Prelude hiding (id, (.))
import Control.Category
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

-- Your code here!

fstLens :: Lens (a, b) a
fstLens = undefined

sndLens :: Lens (a, b) b
sndLens = undefined

entryLens :: Ord k => k -> Lens (Map k v) (Maybe v)
entryLens = undefined
