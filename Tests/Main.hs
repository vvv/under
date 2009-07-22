{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Tests.QuickCheck.BER as QB
import qualified Tests.HUnit.BER as HB
import qualified Tests.HUnit.Util as U
import qualified Tests.HUnit.Lab as L

import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  qcRes <- QB.runTests
  huCnt <- runTestTT (TestList [U.test, L.test, HB.test])
  if qcRes && (errors huCnt + failures huCnt == 0)
    then exitSuccess else exitFailure
