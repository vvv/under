{-# OPTIONS_GHC -Wall #-}
module Tests.QuickCheck.BER (runTests) where

import BBTest.BER (tagClass, TagID(..))

import Test.QuickCheck (quickCheckResult, Result(..))
import Data.Char (ord, chr)
import Data.Bits (shiftL, (.|.), (.&.))

prop_tagClass :: Char -> Int -> Bool
prop_tagClass rc rn = all id [ tagClass (rc `setB78` i) rn == f rn
                               | (i, f) <- zip [0..] ctors ]
    where
      ctors = [Universal, Application, ContextSpecific, Private]

      setB78 :: Char -> Int -> Char
      setB78 c n = chr $ ((ord c) .&. 0x3f) .|. (n `shiftL` 6)

runTests :: IO Bool
runTests = mapM verbCheck tests >>= return . allSuccess
    where
      verbCheck (name, prop) = putStr (name ++ ": ") >> quickCheckResult prop
      tests = [("tagClass", prop_tagClass)]

allSuccess :: [Result] -> Bool
allSuccess [] = True
allSuccess (Success _:rest) = allSuccess rest
allSuccess _ = False
