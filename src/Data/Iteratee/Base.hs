module Data.Iteratee.Base (
  Stream (..)
, Streams (..)
, Iteratee (..)
, iterDone
, iterCont
, liftIter
, iterDoneM
, iterContM
, joinIter
, run
, Enumerator
, Enumeratee
, exceptEnd
, exceptDiv
, throwErr
, throwRecov
, setEnd
, enumEnd
, enumErr
, (>>>)
, enum
) where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import Prelude hiding (null)
import qualified Data.List as List
import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte
import Data.Data
import Data.Monoid (Monoid (..))
import Control.Exception (SomeException, ErrorCall (..), toException,
  fromException)
import Control.Applicative hiding (empty)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class


------------------------------------------------------------------------
--
-- Streams
--
------------------------------------------------------------------------

data Stream s where
  End   :: Maybe SomeException -> Stream s
  Chunk :: s -> Stream s
  deriving (Show, Typeable)


------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------

class Monoid s => Streams s where
  null :: s -> Bool

instance (Eq s) => Eq (Stream s) where
  (End Nothing)   == (End Nothing)   = True
  (End (Just s1)) == (End (Just s2)) = typeOf s1 == typeOf s2
  (Chunk s1)      == (Chunk s2)      = s1 == s2
  _ == _                             = False

instance (Monoid s) => Monoid (Stream s) where
  mempty = Chunk mempty
  mappend (End mErr) _ = End mErr
  mappend _ (End mErr) = End mErr
  mappend (Chunk s1) (Chunk s2) = Chunk (s1 `mappend` s2)

instance Functor Stream where
  fmap f (Chunk xs) = Chunk $ f xs
  fmap _ (End mErr) = End mErr


------------------------------------------------------------------------
--
-- Iteratees
--
------------------------------------------------------------------------

newtype Iteratee s m a = Iteratee
  { runIter
      :: forall r.
         (a -> Stream s -> m r)
      -> ((Stream s -> Iteratee s m a) -> Maybe SomeException -> m r)
      -> m r
  }


------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------

instance (Functor m, Monad m) => Functor (Iteratee s m) where
  fmap f m = Iteratee $ \on_done on_cont ->
    let od = on_done . f
        oc = on_cont . (fmap f .)
    in runIter m od oc

instance (Functor m, Monad m, Streams s) => Applicative (Iteratee s m) where
    pure x  = iterDone x (Chunk mempty)
    m <*> a = m >>= flip fmap a

instance (Monad m, Streams s) => Monad (Iteratee s m) where
    {-# INLINE return #-}
    return x = Iteratee $ \on_done _ -> on_done x (Chunk mempty)
    {-# INLINE (>>=) #-}
    m >>= f = Iteratee $ \on_done on_cont ->
       let m_done a (Chunk s)
                  | null s = runIter (f a) on_done on_cont
	   m_done a stream = runIter (f a) (\x _ -> on_done x stream) f_cont
              where f_cont k Nothing = runIter (k stream) on_done on_cont
                    f_cont k e       = on_cont k e
       in runIter m m_done (\k -> on_cont ((>>= f) . k))

instance (Streams s) => MonadTrans (Iteratee s) where
    {-# INLINE lift #-}
    lift m = Iteratee $ \on_done _ -> m >>= \x -> on_done x (Chunk mempty)

instance (MonadIO m, Streams s) => MonadIO (Iteratee s m) where
  liftIO = lift . liftIO


------------------------------------------------------------------------
-- Combinators
------------------------------------------------------------------------

iterDone :: a -> Stream s -> Iteratee s m a
iterDone x str = Iteratee $ \on_done _ -> on_done x str

iterCont :: (Stream s -> Iteratee s m a) -> Maybe SomeException -> Iteratee s m a
iterCont k e = Iteratee $ \_ on_cont -> on_cont k e

liftIter :: Monad m => (Stream s -> Iteratee s m a) -> Iteratee s m a
liftIter k = Iteratee $ \_ on_cont -> on_cont k Nothing

iterDoneM :: Monad m => a -> Stream s -> m (Iteratee s m a)
iterDoneM x str = return $ Iteratee $ \on_done _ -> on_done x str

iterContM :: Monad m =>
	    (Stream s -> Iteratee s m a) -> Maybe SomeException ->
	    m (Iteratee s m a)
iterContM k e = return $ Iteratee $ \_ on_cont -> on_cont k e

joinIter :: (Monad m, Streams s) => Iteratee s m (Iteratee s' m a) -> Iteratee s m a
joinIter outer = outer >>=
  \inner -> Iteratee $ \od oc ->
  let on_done  x _        = od x (Chunk mempty)
      on_cont  k Nothing  = runIter (k (End Nothing)) on_done on_cont'
      on_cont  _ (Just e) = runIter (throwErr e) od oc
      on_cont' _ e        = runIter (throwErr (maybe exceptDiv id e)) od oc
  in runIter inner on_done on_cont

run :: Monad m => Iteratee s m a -> m a
run iter = runIter iter on_done on_cont
  where
  on_done  x _       = return x
  on_cont  k Nothing = runIter (k (End Nothing)) on_done on_cont'
  on_cont  _ e       = error $ "control message: " ++ show e
  on_cont' _ e       = error $ "control message: " ++ show e

------------------------------------------------------------------------
--
-- Enumerators
--
------------------------------------------------------------------------

type Enumerator s m a =
  Iteratee s m a -> m (Iteratee s m a)

type Enumeratee elo eli m a =
  Iteratee eli m a -> Iteratee elo m (Iteratee eli m a)

------------------------------------------------------------------------
-- Combinators
------------------------------------------------------------------------

exceptEnd :: SomeException
exceptEnd = toException $ ErrorCall "End"

exceptDiv :: SomeException
exceptDiv = toException $ ErrorCall "Divergent Iteratee"

throwErr :: Monad m => SomeException -> Iteratee s m a
throwErr e = Iteratee $ \_ on_cont -> on_cont (\_ -> throwErr e) (Just e)

throwRecov :: Monad m => SomeException -> (Stream s -> Iteratee s m a)
		    -> Iteratee s m a
throwRecov e i = Iteratee $ \_ on_cont -> on_cont i (Just e)


-- Produce the End error message to be passed to throwErr.
-- If the stream was terminated because of an error, keep the original
-- error message.
setEnd :: Stream s -> SomeException
setEnd (End (Just e)) = e
setEnd _              = exceptEnd

-- The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee usually in the done state.
-- A `good' iteratee must move to the done state upon receiving the End
enumEnd :: Monad m => Enumerator s m a
enumEnd iter = runIter iter on_done on_cont
  where
  on_done  x _       = return $ iterDone x (End Nothing)
  on_cont  k Nothing = runIter (k (End Nothing)) on_done on_cont'
  on_cont  k e       = return $ iterCont k e
  on_cont' k Nothing = return $ throwErr exceptDiv
  on_cont' k e       = return $ iterCont k e

-- Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error
enumErr :: Monad m => SomeException ->  Enumerator s m a
enumErr e iter = runIter iter on_done on_cont
  where
  on_done  x _       = return $ iterDone x (End (Just e))
  on_cont  k Nothing = runIter (k (End (Just e))) on_done on_cont'
  on_cont  k e       = return $ iterCont k e
  on_cont' k Nothing = return $ throwErr exceptDiv
  on_cont' k e       = return $ iterCont k e

-- The composition of two enumerators: just the functional composition.
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >>> e2, e1 is executed first.
-- This operation is similar to the left-to-right composition in
-- Control.Category.
-- The composition of enumerators is not exactly (.): we take care
-- to force the result of the enumerator e1 before passing it to e2.
-- We are thus certain that all effects of enumerating e1 happen before
-- the effects of e2.
(>>>):: Monad m => Enumerator s m a -> Enumerator s m a -> Enumerator s m a
e1 >>> e2 = \i -> e1 i >>= e2

-- The pure 1-chunk enumerator
-- It passes a given string to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enum :: Monad m => s -> Enumerator s m a
enum str iter = runIter iter iterDoneM on_cont
  where
  on_cont k Nothing = return (k (Chunk str))
  on_cont k e       = return $ iterCont k e
