module Lib
  ( Lens(..)
  , Point3D(..)
  , xCoordLens
  , yCoordLens
  , zCoordLens
  , makeLens
  , get
  , set
  ) where
import Prelude hiding (id, (.))
import Control.Category

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

-- Your code here!

get :: Lens s t -> s -> t
get = undefined

set :: Lens s t -> s -> t -> s
set = undefined

makeLens :: (s -> t) -> (s -> t -> s) -> Lens s t
makeLens = undefined
