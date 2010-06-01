module Main where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import Prelude hiding (head, take, drop, takeWhile, dropWhile, break, span, length, map)
import Data.Monoid (Monoid (..))
import qualified Data.List as List
import Data.Iteratee.List
import Control.Monad.Identity

import Test.QuickCheck

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

run' :: Identity (Iteratee s Identity a) -> a
run' = runIdentity . run . runIdentity

enum' = enumChunk 3


------------------------------------------------------------------------
-- Basic
------------------------------------------------------------------------

prop_const :: Eq a => a -> String -> Bool
prop_const x xs =
  const x xs
  ==
  run' (enum' xs (return x))

prop_id :: Eq a => [a] -> Bool
prop_id xs =
  id xs
  ==
  run' (enum' xs ident)

prop_length :: Eq a => [a] -> Bool
prop_length xs =
  List.length xs
  ==
  run' (enum' xs length)

prop_drop :: Eq a => Int -> [a] -> Bool
prop_drop n xs =
  List.drop n xs
  ==
  run' (enum' xs (drop n >> ident))

p_dropWhile :: Eq a => (a -> Bool) -> [a] -> Bool
p_dropWhile p xs =
  List.dropWhile p xs
  ==
  run' (enum' xs (dropWhile p >> ident))

prop_dropWhile :: Eq a => a -> [a] -> Bool
prop_dropWhile x = p_dropWhile (==x)

--prop_dropWhile1 = p_dropWhile (const True)
--prop_dropWhile2 = p_dropWhile (const False)

p_break :: Eq a => (a -> Bool) -> [a] -> Bool
p_break p xs =
  fst (List.break p xs)
  ==
  run' (enum' xs (break p))

prop_break :: Eq a => a -> [a] -> Bool
prop_break x = p_break (==x)

--prop_break1 = p_break (const True)
--prop_break2 = p_break (const False)

p_span :: Eq a => (a -> Bool) -> [a] -> Bool
p_span p xs =
  fst (List.span p xs)
  ==
  run' (enum' xs (span p))

prop_span :: Eq a => a -> [a] -> Bool
prop_span x = p_span (==x)

--prop_span1 = p_span (const True)
--prop_span2 = p_span (const False)

prop_take :: Eq a => Int -> [a] -> Bool
prop_take n xs =
  List.take n xs
  ==
  run' (enum' xs (take n))

------------------------------------------------------------------------------
-- Enumerators
------------------------------------------------------------------------------

p_enum :: (Eq a) => Iteratee [a] Identity [a] -> [a] -> Bool
p_enum iter xs =
  run' (enum xs iter)
  ==
  run' (enum' xs iter)

prop_enum_take :: Int -> String -> Bool
prop_enum_take n xs =
  run' (enum' xs (take n))
  ==
  runIdentity (run =<< run =<< enum' xs (enumTake n ident))

