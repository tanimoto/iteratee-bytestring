{-# LANGUAGE BangPatterns #-}
module Main where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import qualified Data.List as List

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Iteratee
import qualified Data.Iteratee.List as IL
import qualified Data.Iteratee.ByteString as IB
import Criterion.Main (defaultMain, bench)

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

file :: FilePath
file = "/usr/share/dict/words"

size :: Int
size = 2048

bench_string :: IO ()
bench_string = do
  !_ <- length `fmap` readFile file
  return ()

bench_byte :: IO ()
bench_byte = do
  !_ <- BS.length `fmap` BS.readFile file
  return ()

bench_byte_lazy :: IO ()
bench_byte_lazy = do
  !_ <- BL.length `fmap` BL.readFile file
  return ()

bench_iter_string :: IO ()
bench_iter_string = do
  !_ <- enumFileSize' size file IL.length >>= run
  return ()
  where
  enumFileSize' :: Int -> FilePath -> Enumerator String IO b
  enumFileSize' = IL.enumFileSize

bench_iter_byte :: IO ()
bench_iter_byte = do
  !_ <- IB.enumFileSize size file IB.length >>= run
  return ()

main :: IO ()
main = defaultMain
  [ bench "Vanilla String"          bench_string
  , bench "Vanilla ByteString"      bench_byte
  , bench "Vanilla Lazy ByteString" bench_byte_lazy
  , bench "Iteratee String"         bench_iter_string
  , bench "Iteratee ByteString"     bench_iter_byte
  ]
