{-# OPTIONS_GHC -Wall #-}
module Tests.QuickCheck.BER (tests) where

import BBTest.BER
import BBTest.Parse (runParser)

import Test.QuickCheck.Property (property, Property, (==>))
import Data.Char (ord, chr)
import Data.Bits (shiftL, (.|.), (.&.), setBit, clearBit)
import qualified Data.ByteString.Lazy.Char8 as C

prop_tagInfo :: Char -> Bool
prop_tagInfo rc = and [ tagInfo rc' == (cls, consp, mnum rc')
                      | (ci, cls) <- zip [0..] classes
                      , consp <- [False, True]
                      , rc' <- [modify rc ci consp]
                      ]
    where
      classes = [Universal, Application, ContextSpecific, Private]

      mnum :: Char -> Maybe Int
      mnum c = case (ord c) .&. 0x1f of { 0x1f -> Nothing; n -> Just n }

      setClass :: Char -> Int -> Int
      setClass c n = ((ord c) .&. 0x3f) .|. (n `shiftL` 6)

      setConsp :: Int -> Bool -> Char
      setConsp n b = chr $ (if b then setBit else clearBit) n 5

      modify c ci cp = (c `setClass` ci) `setConsp` cp

prop_tagLen :: Int -> Int -> Property
prop_tagLen n pos =
    (n >= 0) ==>
    let el = enLen n
    in runParser tagLen (el, pos) ==
           (Right n, (C.empty, pos + fromIntegral (C.length el)))

tests :: [(String, Property)]
tests = [ ("tagInfo", property prop_tagInfo)
        , ("tagLen",  property prop_tagLen)
        ]
