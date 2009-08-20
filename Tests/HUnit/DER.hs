{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Tests.HUnit.DER (test) where

import Codec.Binary.DER

import Tests.Util ((==>))

import Test.HUnit (Test(..))
import Data.Char (chr)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either (partitionEithers)

data SR = SR String deriving Show

sr = SR "e1 70 df 46 01 04 df 47 01 1b df 28 06 a1 76 49 \
        \13 73 f3 ee 10 d3 03 09 07 10 f4 06 df 4a 03 10 \
        \13 35 d1 01 18 30 13 c9 07 91 83 50 10 22 90 45 \
        \df 27 07 81 08 05 21 02 59 f4 ca 03 00 33 20 df \
        \49 0b 41 44 30 37 32 31 37 30 30 33 45 e5 0e df \
        \4b 06 42 4d 53 43 31 33 cc 03 00 04 1f e4 0e df \
        \4b 06 42 4d 53 43 32 38 cc 03 00 02 03 d9 03 13 \
        \6e 06"

drop' :: Int -> SR -> C.ByteString
drop' n (SR s) = (raw . SR . unwords . drop n . words) s

raw :: SR -> C.ByteString
raw (SR s) = C.pack $ map toChar (words s)
    where
      toChar = chr . read . ("0x" ++)

bs = raw . SR

len (SR s) = length (words s)

pk = C.pack

err :: String -> Int -> Either Err a
err msg pos = Left . Err $ msg ++ ": byte " ++ show pos

-- ni s = "not implemented " ++ s ==> ""

tg :: String -> [Test] -> Test
tg name ts = TestLabel name (TestList ts)
------------------------------------------------------------------------

tst_splitAt' = tg "splitAt'" [
   splitAt' 0 C.empty         ==> Just (C.empty, C.empty)
 , splitAt' (-1) (C.pack "_") ==> Nothing
 , splitAt' 1 C.empty         ==> Nothing
 , splitAt' 2 (C.pack "abc")  ==> Just (C.pack "ab", C.pack "c")
 , splitAt' 3 (C.pack "xyz")  ==> Just (C.pack "xyz", C.empty)
 , splitAt' 4 (C.pack "123")  ==> Nothing
 , splitAt' 0 (C.pack "x")    ==> Just (C.empty, C.pack "x")
 ]

tst_tagNum = tg "tagNum" [
   runParser tagNum (C.empty, 100) ==>
                 ( err "tagNum: invalid tag number encoding" 100
                 , (C.empty, 100) )
 , runParser tagNum (pk "\x81\x82", 1) ==>
                 ( err "tagNum: invalid tag number encoding" 1
                 , (pk "\x81\x82", 1) )

 , runParser tagNum (pk "\x2a\&whatever", 5) ==> (Right 42, (pk "whatever", 6))
 , runParser tagNum (pk "\x81\x9b\x73\&_", 10) ==> (Right 19955, (pk "_", 13))
 ]

tst_tagID = tg "tagID" [
   runParser tagID (C.empty, 1) ==>
                 (err "tagID: no identifier octets to parse" 1, (C.empty, 1))
 , runParser tagID (pk "\xff\xff\xff", 10) ==>
                 ( err "tagNum: invalid tag number encoding" 11
                 , (pk "\xff\xff", 11) )
 , runParser tagID (raw sr, 100) ==> ( Right (True, Private, 1)
                                     , (drop' 1 sr, 101) )
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
   runParser tagLen (C.empty, 0) ==>
                 (err "tagLen: too few length octets" 0, (C.empty, 0))
 , runParser tagLen (pk "\xff", 6) ==>
                 (err "tagLen: too few length octets" 6, (pk "\xff", 6))
 , runParser tagLen (pk "\x80", 1) ==> runParser tagLen (pk "\0", 1)
 , runParser tagLen (bs "84 07 5b cd 15 5f", 10) ==> ( Right 123456789
                                                     , (pk "_", 15) )
  ]

tst_parseTag = tg "parseTag" [
   parseTag (C.empty, 1) ==> (Left EOF, (C.empty, 1))
 , parseTag (pk $ replicate 4 '\xff', 10) ==> (Left EOF, (C.empty, 14))
 , parseTag (raw sr, 10) ==> ( Right $ ConsU Private 1 (drop' 2 sr, 12)
                             , (C.empty, 10 + len sr) )
 ]

tst_parseTags = tg "parseTags" [
   parseTags (C.empty, 1) ==> []
 , parseTags (pk $ replicate 9 '\xff', 1) ==> []
 , parseTags (raw sr, 1) ==> [Right $ ConsU Private 1 (drop' 2 sr, 3)]
 , parseTags (bs "d3 03 09 07 10 f4 06 df 4a 03 10 13 59", 34) ==>
                 [ Right $ Prim Private 19 (pk "\x09\x07\x10")
                 , Right $ ConsU Private 20 (bs "df 4a 03 10 13 59", 41) ]
 , parseTags (bs "df 24 01 60 df 42 01", 136) ==>
             [ Right $ Prim Private 36 (pk "`")
             , err "tag: not enough contents octets" 143 ]
 ]

tst_toSexp = tg "toSexp" [
   f (Prim ContextSpecific 4 $ pk "abc") ==> ([], "(c4 \"61 62 63\")")
 , f (Cons Universal 16 [ Prim Private 9 $ bs "91 83 50 10 22 90 45"
                        , Prim Private 39 $ bs "81 08 05 21 02 59 f4" ])
         ==> ([], "(u16 (p9 \"91 83 50 10 22 90 45\")\
                  \ (p39 \"81 08 05 21 02 59 f4\"))")
 , f (ConsU Application 20 (bs "df 4a 03 10 13 59", 1)) ==>
         ([], "(a20 (p74 \"10 13 59\"))")
 , f (ConsU Application 20 (bs "df 4a 03 10 13 59", 5)) ==>
     f (Cons Application 20 [Prim Private 74 $ bs "10 13 59"])

 , f (Cons Private 1 [ ConsU Private 2 (C.empty, 10), Prim Private 3 C.empty ])
               ==> ([], "(p1 (p2) (p3 \"\"))" )
 , f (Cons Private 1 [ ConsU Private 2 (pk "X", 10), Prim Private 3 C.empty ])
               ==> ( [Err "tagLen: too few length octets: byte 11"]
                   , "(p1  (p3 \"\"))" )

 , let (Right t, _) = parseTag (raw sr, 1)
   in toSexp' t ==>
      Right (pk "(p1 (p70 \"04\") (p71 \"1b\")\
                \ (p40 \"a1 76 49 13 73 f3\") (p14 (p19 \"09 07 10\")\
                \ (p20 (p74 \"10 13 35\")) (p17 \"18\"))\
                \ (u16 (p9 \"91 83 50 10 22 90 45\")\
                \ (p39 \"81 08 05 21 02 59 f4\")) (p10 \"00 33 20\")\
                \ (p73 \"41 44 30 37 32 31 37 30 30 33 45\")\
                \ (p5 (p75 \"42 4d 53 43 31 33\") (p12 \"00 04 1f\"))\
                \ (p4 (p75 \"42 4d 53 43 32 38\") (p12 \"00 02 03\"))\
                \ (p25 \"13 6e 06\"))")
 , toSexp' (ConsU Private 13 (bs "00", 1)) ==>
           err "tagLen: too few length octets" 2
 ]
    where
      f t = let (ls, rs) = partitionEithers (toSexp t)
                  in (ls, concatMap C.unpack rs)


test = tg "DER" [ tst_splitAt'
                , tst_tagNum
                , tst_tagID
                , tst_enLen
                , tst_tagLen
                , tst_parseTag
                , tst_parseTags
                , tst_toSexp
                ]
