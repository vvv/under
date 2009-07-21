module Tests.HUnit.Lab (test) where

import Lab.TLV
import Tests.Util ((==>))

import qualified Data.ByteString.Lazy.Char8 as C
import Test.HUnit (Test(..))

ptag s = parse tag (C.pack s, 1)
bs = C.pack

test :: Test
test = TestLabel "Lab" $ TestList [
   parse tag (C.empty, 1) ==> (Left EOF, (C.empty, 1))
 , ptag "FFF"             ==> (Left EOF, (C.empty, 4))
 , ptag "CA02hello" ==> (Right . ComplU 'A' $ bs "he", (bs "llo", 7))
 , ptag "P_123"     ==> (Right . Prim '_' $ bs "2", (bs "3", 5))
 , ptag "PX9abc"    ==> ( Left (Err "not enough content octets: byte 1")
                        , (bs "PX9abc", 1) )
 , ptag "C.1212"    ==> ( Left (Err "not enough content octets: byte 1")
                        , (bs "C.1212", 1))
 , ptag "FFFC.1212" ==> ( Left (Err "not enough content octets: byte 4")
                        , (bs "C.1212", 4))
 , ptag "pX1_" ==> (Left (Err "unknown tag type: byte 1"), (bs "pX1_", 1))
 , ptag "CXYZ" ==> ( Left (Err "invalid length encoding: byte 1")
                   , (bs "CXYZ", 1) )

 , ptag "CR38PT12PD820080713CS19PM9501234567PI4IMSIFF"
            ==> ( Right . ComplU 'R' $ bs
                            "PT12PD820080713CS19PM9501234567PI4IMSI"
                , (bs "FF", 43) )

 , parseTags (bs "Pa0FFFCa31Pb204Cb22Pc6090710Cc09Pd6101335FFF", 1)
                 ==> [ Right (Prim 'a' C.empty)
                     , Right . ComplU 'a' $ bs
                                 "Pb204Cb22Pc6090710Cc09Pd6101335" ]

 , parseTags (bs "P12howdy", 11)
                 ==> [ Right . Prim '1' $ bs "ho"
                     , Left (Err "unknown tag type: byte 16") ]

 , parseTags (C.empty,    1) ==> []
 , parseTags (bs "FFFFF", 1) ==> []
 ]