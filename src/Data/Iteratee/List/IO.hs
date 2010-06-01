module Data.Iteratee.List.IO  (
  defaultBufferSize
, enumHandleSize
, enumHandle
, enumFileSize
, enumFile
) where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import Data.Iteratee.Base
import Data.Iteratee.Exception
import Data.Iteratee.IO (defaultBufferSize, enumHandleWithSize)

import Data.Monoid (Monoid (..))

import Control.Exception
import Control.Monad
import Control.Monad.CatchIO (MonadCatchIO (..))
import qualified Control.Monad.CatchIO as CIO
import Control.Monad.IO.Class

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import System.IO

------------------------------------------------------------------------
-- IO Enumerators
------------------------------------------------------------------------

enumHandleSize :: (Storable a) => Int -> Handle -> Enumerator [a] IO b
enumHandleSize = enumHandleWithSize peekArray
{-# INLINE enumHandleSize #-}

{-# RULES "enumHandleSize/String" enumHandleSize = enumHandleSizeString #-}
enumHandleSizeString :: Int -> Handle -> Enumerator String IO a
enumHandleSizeString = enumHandleWithSize (flip (curry peekCAStringLen))
{-# INLINE enumHandleSizeString #-}

enumHandle :: (Storable a) => Handle -> Enumerator [a] IO b
enumHandle = enumHandleSize defaultBufferSize
{-# INLINE enumHandle #-}

enumFileSize :: (Storable a) => Int -> FilePath -> Enumerator [a] IO b
enumFileSize size file iter = bracket
  (openFile file ReadMode)
  (hClose)
  (\hand -> enumHandleSize size hand iter)
{-# INLINE enumFileSize #-}

enumFile :: (Storable a) => FilePath -> Enumerator [a] IO b
enumFile = enumFileSize defaultBufferSize
{-# INLINE enumFile #-}
