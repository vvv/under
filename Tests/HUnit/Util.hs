module Tests.HUnit.Util (test) where

import BBTest.Util
import Tests.Util ((==>))

import qualified Data.ByteString.Lazy.Char8 as C
import Test.HUnit (Test(..))

test :: Test
test = TestLabel "Util" $ TestList [
   splitAt' 0 C.empty          ==>  Just (C.empty, C.empty)
 , splitAt' (-1) (C.pack "_")  ==>  Nothing
 , splitAt' 1 C.empty          ==>  Nothing
 , splitAt' 2 (C.pack "abc")   ==>  Just (C.pack "ab", C.pack "c")
 , splitAt' 3 (C.pack "xyz")   ==>  Just (C.pack "xyz", C.empty)
 , splitAt' 4 (C.pack "123")   ==>  Nothing
 , splitAt' 0 (C.pack "x")     ==>  Just (C.empty, C.pack "x")
 ]
