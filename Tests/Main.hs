{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Tests.QuickCheck.BER as QB
import qualified Tests.HUnit.BER as HB
import qualified Tests.HUnit.Lab as L

import Test.QuickCheck (quickCheckResult, Property)
import Test.QuickCheck.Test (isSuccess)
import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  qcRes <- runQC QB.tests
  huRes <- runHU [L.test, HB.test]
  if qcRes && huRes
    then exitSuccess else exitFailure

runQC :: [(String, Property)] -> IO Bool
runQC ts = mapM verbCheck ts >>= return . all isSuccess
    where
      verbCheck (name, prop) = putStr (name ++ ": ") >> quickCheckResult prop

runHU :: [Test] -> IO Bool
runHU ts = do
  cnt <- runTestTT (TestList ts)
  return (errors cnt + failures cnt == 0)
