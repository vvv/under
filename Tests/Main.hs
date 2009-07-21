module Main where

import qualified Tests.HUnit.Util as U
import qualified Tests.HUnit.Lab as L
import qualified Tests.HUnit.BER as B

import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  cs <- runTestTT (TestList [U.test, L.test, B.test])
  if errors cs + failures cs == 0 then exitSuccess else exitFailure
