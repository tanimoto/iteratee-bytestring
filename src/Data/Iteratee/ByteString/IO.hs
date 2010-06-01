module Data.Iteratee.ByteString.IO (
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

import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte
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

enumHandleSize :: Int -> Handle -> Enumerator ByteString IO a
enumHandleSize = enumHandleWithSize (flip (curry Byte.packCStringLen))
{-# INLINE enumHandleSize #-}

enumHandle :: Handle -> Enumerator ByteString IO a
enumHandle = enumHandleSize defaultBufferSize
{-# INLINE enumHandle #-}

enumFileSize :: Int -> FilePath -> Enumerator ByteString IO a
enumFileSize size file iter = bracket
  (openFile file ReadMode)
  (hClose)
  (\hand -> enumHandleSize size hand iter)
{-# INLINE enumFileSize #-}

enumFile :: FilePath -> Enumerator ByteString IO a
enumFile = enumFileSize defaultBufferSize
{-# INLINE enumFile #-}
