module Main (main) where

import Split (splitOn)
import System.Exit (exitFailure)

main :: IO ()
main = do
  testEmpty
  testString

testEmpty :: IO ()
testEmpty = case splitOn ' ' "" of
  [] -> return ()
  _ -> exitFailure

testString :: IO ()
testString = case splitOn ' ' "foo bar" of
  ["foo", "bar"] -> return ()
  _ -> exitFailure
