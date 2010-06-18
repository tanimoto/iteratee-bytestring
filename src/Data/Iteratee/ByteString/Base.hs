module Data.Iteratee.ByteString.Base
  ( length
  , take
  , drop
  , span
  , break
  , takeWhile
  , dropWhile
  , filter
  , toByteString
  , toList
  , toHandle
  , toFile
  , enumTake
  ) where

------------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------------

import Prelude hiding
  (length, take, drop, takeWhile, dropWhile, filter, break, span)
import Data.Iteratee.Base

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte

import Data.Monoid (Monoid (..))
import Control.Monad.Trans.Class (lift)

import System.IO (Handle, IOMode (..), openFile, hClose)

------------------------------------------------------------------------------
-- Basic Iteratees
------------------------------------------------------------------------------

length :: (Monad m) => Iteratee m Int
length = liftIter (step 0)
  where
  step !acc (Chunk str) = liftIter (step (acc + Byte.length str))
  step !acc str         = iterDone acc str
{-# INLINE length #-}

take :: (Monad m) => Int -> Iteratee m ByteString
take n = liftIter (step n mempty)
  where
  step 0 acc str = iterDone acc str
  step m acc (Chunk str)
    | Byte.length str < m =
        liftIter (step (m - Byte.length str)
                 (acc `mappend` str))
    | otherwise  =
        iterDone (acc `mappend` Byte.take m str)
                 (Chunk (Byte.drop m str))
  step _ acc str = iterDone acc str
{-# INLINE take #-}

drop :: (Monad m) => Int -> Iteratee m ()
drop 0 = return ()
drop n = liftIter (step n)
  where
  step m (Chunk str)
    | Byte.length str < n = liftIter (step (m - Byte.length str))
    | otherwise           = iterDone () (Chunk (Byte.drop m str))
  step _ str              = iterDone () str
{-# INLINE drop #-}

break :: (Monad m) => (Word8 -> Bool) -> Iteratee m ByteString
break p = liftIter (step mempty)
  where
  step pre (Chunk str)
    | Byte.null str = liftIter (step pre)
    | otherwise     = case Byte.break p str of
        (x, xs)
          | Byte.null xs -> liftIter (step (pre `mappend` str))
          | otherwise    -> iterDone (pre `mappend` x) (Chunk xs)
  step pre str      = iterDone pre str
{-# INLINE break #-}

span :: (Monad m) => (Word8 -> Bool) -> Iteratee m ByteString
span p = break (not . p)
{-# INLINE span #-}

takeWhile :: (Monad m) => (Word8 -> Bool) -> Iteratee m ByteString
takeWhile p = liftIter (step mempty)
  where
  step acc (Chunk str)
    | Byte.null str = liftIter (step acc)
    | Byte.null s2  = liftIter (step (acc `mappend` s1))
    | otherwise     = iterDone (acc `mappend` s1) (Chunk s2)
    where (s1, s2)  = Byte.span p str
  step acc str      = iterDone acc str
{-# INLINE takeWhile #-}

dropWhile :: (Monad m) => (Word8 -> Bool) -> Iteratee m ()
dropWhile p = liftIter step
  where
  step (Chunk str)
    | Byte.null str' = liftIter step
    | otherwise      = iterDone () (Chunk str')
    where str' = Byte.dropWhile p str
  step str           = iterDone () str
{-# INLINE dropWhile #-}

filter :: (Monad m) => (Word8 -> Bool) -> Iteratee m ByteString
filter p = liftIter (step mempty)
  where
  step acc (Chunk str)
    | Byte.null str = liftIter (step acc)
    | otherwise     = liftIter (step (acc `mappend` Byte.filter p str))
  step acc str      = iterDone acc str
{-# INLINE filter #-}

------------------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------------------

toByteString :: (Monad m) => Iteratee m ByteString
toByteString = liftIter (step mempty)
  where
  step acc (Chunk str)
    | Byte.null str = liftIter (step acc)
    | otherwise     = liftIter (step (acc `mappend` str))
  step acc str      = iterDone acc str
{-# INLINE toByteString #-}

toList :: (Monad m) => Iteratee m [ByteString]
toList = liftIter (step mempty)
  where
  step acc (Chunk str)
    | Byte.null str = liftIter (step acc)
    | otherwise     = liftIter (step (acc `mappend` return str))
  step acc str      = iterDone acc str
{-# INLINE toList #-}

toHandle :: Handle -> Iteratee IO ()
toHandle h = liftIter step
  where
  step (Chunk str)
    | Byte.null str = liftIter step
    | otherwise     = lift (Byte.hPut h str)
                      >> liftIter step
  step str          = iterDone () str
{-# INLINE toHandle #-}

toFile :: FilePath -> Iteratee IO ()
toFile file =
  lift (openFile file WriteMode) >>= \h-> toHandle h >> lift (hClose h)
{-# INLINE toFile #-}

------------------------------------------------------------------------------
-- Enumeratees
------------------------------------------------------------------------------

enumTake :: (Monad m) => Int -> Enumeratee m a
enumTake 0 iter = return iter
enumTake n iter = Iteratee $ \od oc-> runIter iter (on_done od oc) (on_cont od oc)
  where
  on_done od oc x _ =
    runIter (drop n >> return (return x)) od oc

  on_cont od oc k Nothing =
    if n == 0
      then od (liftIter k) (Chunk mempty)
      else runIter (liftIter (step n k)) od oc
  on_cont od oc _ (Just e) =
    runIter (drop n >> throwErr e) od oc

  step m k (Chunk str)
    | Byte.null str = liftIter (step m k)
    | Byte.length str <= m = enumTake (m - Byte.length str) $ k (Chunk str)
    | otherwise = let (s1, s2) = Byte.splitAt m str
                  in iterDone (k (Chunk s1)) (Chunk s2)
  step _ k str  = iterDone (k str) str
{-# INLINE enumTake #-}
