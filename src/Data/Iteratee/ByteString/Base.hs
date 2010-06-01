module Data.Iteratee.ByteString.Base (
  length
) where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import Data.Iteratee.Base

import Prelude hiding (length, take, drop, break, span)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte

------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------

instance Streams ByteString where
  null = Byte.null

------------------------------------------------------------------------
-- Iteratees
------------------------------------------------------------------------

length :: (Monad m) => Iteratee ByteString m Int
length = length' 0
  where
  length' !n = iterCont (step n) Nothing
  step i (Chunk xs) = length' (i + Byte.length xs)
  step i stream     = iterDone i stream
