{-# options_ghc -fno-warn-orphans #-}
module Data.Hashable.Lifted.Orphans where

import Data.Foldable (foldl')
import Data.Hashable.Lifted (Hashable1(..))
import Data.Vector (Vector)

instance Hashable1 Vector where
  liftHashWithSalt hws = foldl' hws
