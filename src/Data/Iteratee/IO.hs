{-# LANGUAGE CPP #-}
module Data.Iteratee.IO (
  defaultBufferSize
, enumHandleWithSize
) where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import Data.Iteratee.Base
import Data.Iteratee.Exception

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
-- Constants
------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__)

import GHC.IO.Handle.Internals (dEFAULT_CHAR_BUFFER_SIZE)

defaultBufferSize :: Int
defaultBufferSize = dEFAULT_CHAR_BUFFER_SIZE

#else

defaultBufferSize :: Int
defaultBufferSize = 2048

#endif

------------------------------------------------------------------------
-- IO Enumerators
------------------------------------------------------------------------

enumHandleWithSize :: (Int -> Ptr c -> IO s) -> Int -> Handle -> Enumerator s IO a
enumHandleWithSize peek size hand iter = do
  p <- mallocBytes size
  r <- loop p iter
  free p
  return r
  where
  loop p iter = runIter iter iterDoneM (on_cont p)

  on_cont p k Nothing = do_read k p
  on_cont p k e       = return $ iterCont k e

  do_read k p = do
    n <- liftIO (CIO.try $ hGetBuf hand p size :: IO (Either SomeException Int))
    case n of
      Left  e -> return $ k (End (Just (toException (ErrorCall "IO Error"))))
      Right 0 -> return $ liftIter k
      Right n -> do
        str <- peek n p
        loop p (k (Chunk str))
