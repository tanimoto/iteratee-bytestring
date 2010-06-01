module Data.Iteratee.List.Base (
  ident
, length
, drop
, dropWhile
, span
, break
, take
, enumCheck
, enumMap
, enumTake
, enumChunk
) where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import Data.Iteratee.Base

import Prelude hiding (length, take, drop, takeWhile, dropWhile,
  break, span, map)
import qualified Data.List as List

------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------

instance Streams [a] where
  null = List.null

------------------------------------------------------------------------
-- Iteratees
------------------------------------------------------------------------

ident :: (Monad m) => Iteratee [a] m [a]
ident = iterCont (step []) Nothing
  where
  step acc (Chunk str)
    | List.null str = iterCont (step acc) Nothing
    | otherwise     = iterCont (step (acc ++ str)) Nothing
  step acc str      = iterDone acc str

length :: (Monad m) => Iteratee [a] m Int
length = length' 0
  where
  length' !n = iterCont (step n) Nothing
  step i (Chunk str) = length' (i + List.length str)
  step i str         = iterDone i str

drop :: (Monad m) => Int -> Iteratee [a] m ()
drop 0 = return ()
drop n = liftIter (step n)
  where
  step n (Chunk str) | List.length str < n = liftIter (step (n - List.length str))
  step n (Chunk str) = iterDone () (Chunk (List.drop n str))
  step _ str         = iterDone () str

dropWhile :: (Monad m) => (a -> Bool) -> Iteratee [a] m ()
dropWhile p = liftIter step
  where
  step (Chunk [] ) = liftIter step
  step (Chunk str) = case List.dropWhile p str of
    []  -> liftIter step
    str -> iterDone () (Chunk str)
  step str         = iterDone () str

break :: (Monad m) => (a -> Bool) -> Iteratee [a] m [a]
break p = liftIter (step [])
  where
  step pre (Chunk [])  = liftIter $ step pre
  step pre (Chunk str) = case List.break p str of
    (_  , [])   -> liftIter (step (pre ++ str))
    (str, tail) -> iterDone (pre ++ str) (Chunk tail)
  step pre str = iterDone pre str

span :: (Monad m) => (a -> Bool) -> Iteratee [a] m [a]
span p = break (not . p)
{-# INLINE span #-}

take :: (Monad m) => Int -> Iteratee [a] m [a]
take n = liftIter (step n [])
  where
  step 0 acc str    = iterDone acc str
  step n acc s@(Chunk str)
    | List.null str = liftIter (step n acc)
    | otherwise     = liftIter (step (n-List.length str) (acc ++ List.take n str))
  step _ acc str    = iterDone acc str

------------------------------------------------------------------------
-- Enumeratees
------------------------------------------------------------------------

enumCheck :: Monad m =>
    ((Stream eli -> Iteratee eli m a) -> Iteratee [elo] m (Iteratee eli m a)) ->
    Enumeratee [elo] eli m a
enumCheck f inner = Iteratee $ \od oc ->
  let on_done x s = od (iterDone x s) (Chunk [])
      on_cont k Nothing  = runIter (f k) od oc
      on_cont _ (Just e) = runIter (throwErr e) od oc
  in runIter inner on_done on_cont

enumMap :: (Monad m) => (so -> si) -> Enumeratee [so] [si] m a
enumMap f = enumCheck (liftIter . step)
  where
  step k (Chunk [])  = liftIter (step k)
  step k (Chunk str) = enumMap f (k (Chunk (List.map f str)))
  step k str         = iterDone (liftIter k) str

enumTake :: (Monad m) => Int -> Enumeratee [a] [a] m b
enumTake n iter = Iteratee $ \od oc -> runIter iter (on_done od oc) (on_cont od oc)
 where
 on_done od oc x _        = runIter (drop n >> (return $ return x)) od oc
 on_cont od oc k Nothing  = if n == 0 then od (liftIter k) (Chunk [])
			    else runIter (liftIter (step n k)) od oc
 on_cont od oc _ (Just e) = runIter (drop n >> throwErr e) od oc
 step n k (Chunk []) = liftIter (step n k)
 step n k chunk@(Chunk str) | List.length str < n =
				enumTake (n - List.length str) $ k chunk
 step n k (Chunk str) = iterDone (k (Chunk s1)) (Chunk s2)
   where (s1,s2) = List.splitAt n str
 step n k stream      = iterDone (k stream) stream

------------------------------------------------------------------------
-- Enumerators
------------------------------------------------------------------------

enumChunk :: (Monad m) => Int -> [a] -> Iteratee [a] m b -> m (Iteratee [a] m b)
enumChunk n str iter
  | List.null str = return iter
  | n > 0         = enum' str iter
  | otherwise     = error $ "enumChunk called with n == " ++ show n
  where
  enum' str' iter'
    | List.null str' = return iter'
    | otherwise      = runIter iter' iterDoneM on_cont
        where
        (s1, s2) = List.splitAt n str'
        on_cont k Nothing = (enum' s2 . k) (Chunk s1)
        on_cont k e       = return $ iterCont k e
