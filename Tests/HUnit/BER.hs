module Tests.HUnit.BER (test) where

import BBTest.BER

import BBTest.Parse (Err(..))
import Tests.Util ((==>))

import Test.HUnit (Test(..))
import Data.Char (chr)
import qualified Data.ByteString.Lazy.Char8 as C

data SR = SR String
          deriving Show

sr = SR "e1 70 df 46 01 04 df 47 01 1b df 28 06 a1 76 49 \
        \13 73 f3 ee 10 d3 03 09 07 10 f4 06 df 4a 03 10 \
        \13 35 d1 01 18 30 13 c9 07 91 83 50 10 22 90 45 \
        \df 27 07 81 08 05 21 02 59 f4 ca 03 00 33 20 df \
        \49 0b 41 44 30 37 32 31 37 30 30 33 45 e5 0e df \
        \4b 06 42 4d 53 43 31 33 cc 03 00 04 1f e4 0e df \
        \4b 06 42 4d 53 43 32 38 cc 03 00 02 03 d9 03 13 \
        \6e 06"

dropB :: Int -> SR -> SR
dropB n (SR s) = (SR . unwords . drop n . words) s

raw :: SR -> C.ByteString
raw (SR s) = C.pack $ map toChar (words s)
    where
      toChar = chr . read . ("0x" ++)

len (SR s) = length s

pk = C.pack
------------------------------------------------------------------------

tg :: String -> [Test] -> Test
tg name ts = TestLabel name (TestList ts)

tst_parseTagNum = tg "parseTagNum" [
   parseTagNum (C.empty, 100) ==> ( Left (Err "no tag number: byte 100")
                                  , (C.empty, 100) )
 , parseTagNum (pk "\x81\x82", 1)
     ==> ( Left (Err "invalid tag number (no \"last octet\"): byte 1")
         , (pk "\x81\x82", 100) )

 , parseTagNum (pk "\x2a\&whatever", 5) ==> (Right 42, (pk "whatever", 6))
 , parseTagNum (pk "\x81\x9b\x73\&_", 10) ==> (Right 19955, (pk "_", 13))
 ]

tst_parseTag = tg "parseTag" [
   parseTag (C.empty, 1) ==> (Left EOF, (C.empty, 1))
 , parseTag (pk $ replicate 4 '\xff', 10) ==> (Left EOF, (C.empty, 14))
 , parseTag (raw sr, 10) ==> ( Right $ ConsU (Private, 1) (raw $ dropB 2 sr)
                             , (C.empty, 10 + len sr) )
 ]

test :: Test
test = tg "BER" [ tst_parseTagNum, tst_parseTag ]
