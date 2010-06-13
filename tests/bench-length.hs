{-# LANGUAGE BangPatterns #-}
module Main where

------------------------------------------------------------------------
-- Imports
------------------------------------------------------------------------

import qualified Data.List as List

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Data.Iteratee as Iter

import Criterion.Main (defaultMain, bench)

import qualified System.IO as IO

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

file :: FilePath
file = "/usr/share/dict/words"

size :: Int
size = 2048

bench_string :: IO ()
bench_string = do
  !_ <- List.length `fmap` IO.readFile file
  return ()

bench_byte :: IO ()
bench_byte = do
  !_ <- BS.length `fmap` BS.readFile file
  return ()

bench_byte_lazy :: IO ()
bench_byte_lazy = do
  !_ <- BL.length `fmap` BL.readFile file
  return ()

bench_iter_byte :: IO ()
bench_iter_byte = do
  !_ <- Iter.enumFile file Iter.length >>= Iter.run
  return ()

main :: IO ()
main = defaultMain
  [ bench "Vanilla String"          bench_string
  , bench "Vanilla ByteString"      bench_byte
  , bench "Vanilla Lazy ByteString" bench_byte_lazy
  , bench "Iteratee ByteString"     bench_iter_byte
  ]
