module Data.Iteratee.Base
  ( Stream (..)
  , Iteratee (..)
  , iterDone
  , iterCont
  , liftIter
  , iterDoneM
  , iterContM
  , joinIter
  , run
  , iterIdent
  , iterList
  , iterStream
  , iterConst
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
  , enumChunk
  , enumCheck
  , enumMap
  ) where

------------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------------

--import Prelude hiding (null)
import Prelude ()
import Prelude.Base

import qualified Data.List as List
import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
--import qualified Data.Traversable as Trav

import Data.Data
import Data.Monoid (Monoid (..))
import Control.Exception (SomeException, ErrorCall (..), toException)
import Control.Applicative hiding (empty)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class


------------------------------------------------------------------------------
--
-- Streams
--
------------------------------------------------------------------------------

type Buffer = Seq ByteString

data Stream where
  End   :: Maybe SomeException -> Stream
  Chunk :: Buffer -> Stream
  deriving (Show, Typeable)


------------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------------

instance Eq Stream where
  (End Nothing)   == (End Nothing)   = True
  (End (Just s1)) == (End (Just s2)) = typeOf s1 == typeOf s2
  (Chunk s1)      == (Chunk s2)      = s1 == s2
  _ == _                             = False

instance Monoid Stream where
  mempty = Chunk mempty
  mappend (End err) _ = End err
  mappend _ (End err) = End err
  mappend (Chunk s1) (Chunk s2) = Chunk (s1 `mappend` s2)


------------------------------------------------------------------------------
------------------------------------------------------------------------
--
-- Iteratees
--
------------------------------------------------------------------------------
------------------------------------------------------------------------

newtype Iteratee m a = Iteratee
  { runIter
      :: forall r.
         (a -> Stream -> m r)
      -> ((Stream -> Iteratee m a) -> Maybe SomeException -> m r)
      -> m r
  }


------------------------------------------------------------------------------
-- Classes
------------------------------------------------------------------------------

instance (Functor m, Monad m) => Functor (Iteratee m) where
  fmap f m = Iteratee $ \on_done on_cont ->
    let od = on_done . f
        oc = on_cont . (fmap f .)
    in runIter m od oc

instance (Functor m, Monad m) => Applicative (Iteratee m) where
    pure x  = iterDone x (Chunk mempty)
    m <*> a = m >>= flip fmap a

instance (Monad m) => Monad (Iteratee m) where
    {-# INLINE return #-}
    return x = Iteratee $ \on_done _ -> on_done x (Chunk mempty)
    {-# INLINE (>>=) #-}
    m >>= f = Iteratee $ \on_done on_cont ->
       let m_done a (Chunk s)
                  | Seq.null s = runIter (f a) on_done on_cont
	   m_done a stream = runIter (f a) (\x _ -> on_done x stream) f_cont
              where f_cont k Nothing = runIter (k stream) on_done on_cont
                    f_cont k e       = on_cont k e
       in runIter m m_done (\k -> on_cont ((>>= f) . k))

instance MonadTrans (Iteratee) where
    {-# INLINE lift #-}
    lift m = Iteratee $ \on_done _ -> m >>= \x -> on_done x (Chunk mempty)

instance (MonadIO m) => MonadIO (Iteratee m) where
  liftIO = lift . liftIO


------------------------------------------------------------------------------
-- Combinators
------------------------------------------------------------------------------

iterDone :: a -> Stream -> Iteratee m a
iterDone x str = Iteratee $ \on_done _ -> on_done x str

iterCont :: (Stream -> Iteratee m a) -> Maybe SomeException -> Iteratee m a
iterCont k e = Iteratee $ \_ on_cont -> on_cont k e

liftIter :: Monad m => (Stream -> Iteratee m a) -> Iteratee m a
liftIter k = Iteratee $ \_ on_cont -> on_cont k Nothing

iterDoneM :: Monad m => a -> Stream -> m (Iteratee m a)
iterDoneM x str = return $ Iteratee $ \on_done _ -> on_done x str

iterContM :: Monad m =>
	    (Stream -> Iteratee m a) -> Maybe SomeException ->
	    m (Iteratee m a)
iterContM k e = return $ Iteratee $ \_ on_cont -> on_cont k e

joinIter :: (Monad m) => Iteratee m (Iteratee m a) -> Iteratee m a
joinIter outer = outer >>=
  \inner -> Iteratee $ \od oc ->
  let on_done  x _        = od x (Chunk mempty)
      on_cont  k Nothing  = runIter (k (End Nothing)) on_done on_cont'
      on_cont  _ (Just e) = runIter (throwErr e) od oc
      on_cont' _ e        = runIter (throwErr (maybe exceptDiv id e)) od oc
  in runIter inner on_done on_cont

run :: Monad m => Iteratee m a -> m a
run iter = runIter iter on_done on_cont
  where
  on_done  x _       = return x
  on_cont  k Nothing = runIter (k (End Nothing)) on_done on_cont'
  on_cont  _ e       = error $ "control message: " List.++ show e
  on_cont' _ e       = error $ "control message: " List.++ show e

iterIdent :: (Monad m) => Iteratee m ByteString
iterIdent = liftIter (step mempty)
  where
  step acc (Chunk str)
    | Seq.null str = liftIter (step acc)
    | otherwise    = liftIter (step (acc `mappend` Fold.fold str))
  step acc str     = iterDone acc str


{-
iterIdent' :: (Monad m, Monoid a) => Iteratee m a
iterIdent' = liftIter (step mempty)
  where
  step acc (Chunk str)
    | Seq.null str = liftIter (step acc)
    | otherwise    = liftIter (step (acc `mappend` return (Fold.fold str)))
  step acc str     = iterDone acc str
-}

iterList :: (Monad m) => Iteratee m [ByteString]
iterList = liftIter (step mempty)
  where
  step acc (Chunk str)
    | Seq.null str = liftIter (step acc)
    | otherwise    = liftIter (step (acc `mappend` [Fold.fold str]))
  step acc str     = iterDone acc str

iterStream :: (Monad m) => Iteratee m Buffer
iterStream = liftIter (step mempty)
  where
  step acc (Chunk str)
    | Seq.null str = liftIter (step acc)
    | otherwise    = liftIter (step (acc `mappend` str))
  step acc str     = iterDone acc str

iterConst :: (Monad m) => a -> Iteratee m a
iterConst x = iterDone x mempty

------------------------------------------------------------------------------
--
-- Enumerators
--
------------------------------------------------------------------------------

type Enumerator m a =
  Iteratee m a -> m (Iteratee m a)

type Enumeratee m a =
  Iteratee m a -> Iteratee m (Iteratee m a)

------------------------------------------------------------------------------
-- Combinators
------------------------------------------------------------------------------

exceptEnd :: SomeException
exceptEnd = toException $ ErrorCall "End"

exceptDiv :: SomeException
exceptDiv = toException $ ErrorCall "Divergent Iteratee"

throwErr :: Monad m => SomeException -> Iteratee m a
throwErr e = Iteratee $ \_ on_cont -> on_cont (\_ -> throwErr e) (Just e)

throwRecov :: Monad m => SomeException -> (Stream -> Iteratee m a)
		    -> Iteratee m a
throwRecov e i = Iteratee $ \_ on_cont -> on_cont i (Just e)


-- Produce the End error message to be passed to throwErr.
-- If the stream was terminated because of an error, keep the original
-- error message.
setEnd :: Stream -> SomeException
setEnd (End (Just e)) = e
setEnd _              = exceptEnd

-- The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee usually in the done state.
-- A `good' iteratee must move to the done state upon receiving the End
enumEnd :: Monad m => Enumerator m a
enumEnd iter = runIter iter on_done on_cont
  where
  on_done  x _       = return $ iterDone x (End Nothing)
  on_cont  k Nothing = runIter (k (End Nothing)) on_done on_cont'
  on_cont  k e       = return $ iterCont k e
  on_cont' _ Nothing = return $ throwErr exceptDiv
  on_cont' k e       = return $ iterCont k e

-- Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error
enumErr :: Monad m => SomeException ->  Enumerator m a
enumErr e iter = runIter iter on_done on_cont
  where
  on_done  x _       = return $ iterDone x (End (Just e))
  on_cont  k Nothing = runIter (k (End (Just e))) on_done on_cont'
  on_cont  k e'      = return $ iterCont k e'
  on_cont' _ Nothing = return $ throwErr exceptDiv
  on_cont' k e'      = return $ iterCont k e'

-- The composition of two enumerators: just the functional composition.
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >>> e2, e1 is executed first.
-- This operation is similar to the left-to-right composition in
-- Control.Category.
-- The composition of enumerators is not exactly (.): we take care
-- to force the result of the enumerator e1 before passing it to e2.
-- We are thus certain that all effects of enumerating e1 happen before
-- the effects of e2.
(>>>):: Monad m => Enumerator m a -> Enumerator m a -> Enumerator m a
e1 >>> e2 = \i -> e1 i >>= e2


-- The pure 1-chunk enumerator
-- It passes a given string to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
--enum :: Monad m => ByteString -> Enumerator ByteString m a
enum :: Monad m => ByteString -> Enumerator m a
enum str iter = runIter iter iterDoneM on_cont
  where
  on_cont k Nothing = return (k (Chunk (Seq.singleton str)))
  on_cont k e       = return $ iterCont k e

enumChunk :: (Monad m) => Int -> ByteString -> Enumerator m a
enumChunk n str iter
  | Byte.null str = return iter
  | n > 0         = enum' str iter
  | otherwise     = error $ "enumChunk called with n == " List.++ show n
  where
  enum' str' iter'
    | Byte.null str' = return iter'
    | otherwise      = runIter iter' iterDoneM on_cont
        where
        (s1, s2) = Byte.splitAt n str'
        on_cont k Nothing = (enum' s2 . k) (Chunk (Seq.singleton s1))
        on_cont k e       = return $ iterCont k e

enumCheck :: (Monad m) =>
    ((Stream -> Iteratee m a) -> Iteratee m (Iteratee m a)) ->
    Enumeratee m a
enumCheck f inner = Iteratee $ \od oc ->
  let on_done x s = od (iterDone x s) (Chunk mempty)
      on_cont k Nothing  = runIter (f k) od oc
      on_cont _ (Just e) = runIter (throwErr e) od oc
  in runIter inner on_done on_cont

enumMap :: (Monad m) => (ByteString -> ByteString) -> Enumeratee m a
enumMap f = enumCheck (liftIter . step)
  where
  step k (Chunk str)
    | Seq.null str = liftIter (step k)
    | otherwise    = enumMap f (k (Chunk (fmap f str)))
  step k str       = iterDone (liftIter k) str
