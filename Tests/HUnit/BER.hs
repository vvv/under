{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Tests.HUnit.BER (test) where

import BBTest.BER

import BBTest.Parse (Err(..), runParser)
import Tests.Util ((==>))

import Test.HUnit (Test(..))
import Data.Char (chr)
import qualified Data.ByteString.Lazy.Char8 as C

data SR = SR String deriving Show

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

len (SR s) = length (words s)

pk = C.pack

err :: String -> Int -> Either Err a
err msg pos = Left . Err $ msg ++ ": byte " ++ show pos

ni s = "not implemented " ++ s ==> ""
------------------------------------------------------------------------

tg :: String -> [Test] -> Test
tg name ts = TestLabel name (TestList ts)

tst_tagNum = tg "tagNum" [
   runParser tagNum (C.empty, 100) ==>
                 (err "invalid tag number encoding" 100, (C.empty, 100))
 , runParser tagNum (pk "\x81\x82", 1) ==>
                 (err "invalid tag number encoding" 1, (pk "\x81\x82", 1))

 , runParser tagNum (pk "\x2a\&whatever", 5) ==> (Right 42, (pk "whatever", 6))
 , runParser tagNum (pk "\x81\x9b\x73\&_", 10) ==> (Right 19955, (pk "_", 13))
 ]

tst_tagID = tg "tagID" [
   let (Left e, st) = runParser tagID (C.empty, 1)
   in (e, st) ==> (EOF, (C.empty, 1))

 , let (Left e, st) = runParser tagID (pk $ replicate 4 '\xff', 10)
   in (e, st) ==> (EOF, (C.empty, 14))

 , let (Right f, st) = runParser tagID (raw sr, 100)
   in (f (pk "contents"), st) ==> ( ConsU Private 1 (pk "contents")
                                  , (raw $ dropB 1 sr, 101) )
 ]

tst_enLen = tg "enLen" [
   enLen   0 ==> pk "\x0"
 , enLen  38 ==> pk "\x26"
 , enLen 127 ==> pk "\x7f"
 , enLen 128 ==> pk "\x81\x80"
 , enLen 201 ==> pk "\x81\xc9"
 , enLen 291 ==> pk "\x82\x01\x23"
 ]

tst_tagLen = tg "tagLen" [
   runParser tagLen (C.empty, 0) ==> ( err "invalid tag length encoding" 0
                                     , (C.empty, 0) )
 , runParser tagLen (pk "\xff", 6) ==> ( err "invalid tag length encoding" 6
                                       , (pk "\xff", 6) )
 , runParser tagLen (pk "\x80", 1) ==> runParser tagLen (pk "\0", 1)
 , runParser tagLen (raw (SR "84 07 5b cd 15 5f"), 10) ==> ( Right 123456789
                                                           , (pk "_", 15) )
  ]

tst_parseTag = tg "parseTag" [
   parseTag (C.empty, 1) ==> (Left EOF, (C.empty, 1))
 , parseTag (pk $ replicate 4 '\xff', 10) ==> (Left EOF, (C.empty, 14))
 , parseTag (raw sr, 10) ==> ( Right $ ConsU Private 1 (raw $ dropB 2 sr)
                             , (C.empty, 10 + len sr) )
 ]

tst_parseTags = tg "parseTags" [
   parseTags (C.empty, 1) ==> []
 , parseTags (pk $ replicate 9 '\xff', 1) ==> []
 , parseTags (raw sr, 1) ==> [Right $ ConsU Private 1 (raw $ dropB 2 sr)]
 , parseTags (raw $ SR "d3 03 09 07 10 f4 06 df 4a 03 10 13 59", 34) ==>
                 [ Right $ Prim Private 19 (pk "\x09\x07\x10")
                 , Right $ ConsU Private 20 $ raw $ SR "df 4a 03 10 13 59" ]
 , parseTags (raw $ SR "df 24 01 60 df 42 01", 136) ==>
             [ Right $ Prim Private 36 (pk "`")
             , err "not enough contents octets" 143 ]
 ]

test = tg "BER" [ tst_tagNum
                , tst_tagID
                , tst_enLen
                , tst_tagLen
                , tst_parseTag
                , tst_parseTags
                ]
