module Data.Iteratee.IO
  ( defaultBufferSize
  , enumHandleSize
  , enumHandle
  , enumFileSize
  , enumFile
  ) where

------------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------------

import Data.Iteratee.Base

import qualified Data.ByteString as Byte
import qualified Data.Sequence as Seq

import Control.Exception (SomeException, ErrorCall (..), toException)
import qualified Control.Monad.CatchIO as CIO
import Control.Monad.IO.Class (liftIO)

import Foreign.Marshal.Alloc (mallocBytes, free)

import System.IO (Handle, IOMode (..), openFile, hClose, hGetBuf)

------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------

defaultBufferSize :: Int
defaultBufferSize = 2048

------------------------------------------------------------------------------
-- IO Enumerators
------------------------------------------------------------------------------

enumHandleSize :: Int -> Handle -> Enumerator IO a
enumHandleSize size hand iter = do
  p <- mallocBytes size
  r <- loop p iter
  free p
  return r
  where
  loop p iter' = runIter iter' iterDoneM (on_cont p)

  on_cont p k Nothing = do_read k p
  on_cont _ k e       = return $ iterCont k e

  do_read k p = do
    n <- liftIO (CIO.try $ hGetBuf hand p size :: IO (Either SomeException Int))
    case n of
      Left  _ -> return $ k (End (Just (toException (ErrorCall "IO Error"))))
      Right 0 -> return $ liftIter k
      Right m -> do
        str <- Byte.packCStringLen (p, m)
        loop p (k (Chunk (Seq.singleton str)))
{-# INLINE enumHandleSize #-}

enumHandle :: Handle -> Enumerator IO a
enumHandle = enumHandleSize defaultBufferSize
{-# INLINE enumHandle #-}

enumFileSize :: Int -> FilePath -> Enumerator IO a
enumFileSize size file iter = CIO.bracket
  (openFile file ReadMode)
  (hClose)
  (\hand -> enumHandleSize size hand iter)
{-# INLINE enumFileSize #-}

enumFile :: FilePath -> Enumerator IO a
enumFile = enumFileSize defaultBufferSize
{-# INLINE enumFile #-}
