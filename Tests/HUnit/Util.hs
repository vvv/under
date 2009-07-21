module Tests.HUnit.Util (test) where

import BBTest.Util
import Tests.Util ((==>))

import qualified Data.ByteString.Lazy.Char8 as C
import Test.HUnit (Test(..))

test :: Test
test = TestLabel "Util" $ TestList [
   takeExactly 0 C.empty          ==>  Just (C.empty, C.empty)
 , takeExactly (-1) (C.pack "_")  ==>  Nothing
 , takeExactly 1 C.empty          ==>  Nothing
 , takeExactly 2 (C.pack "abc")   ==>  Just (C.pack "ab", C.pack "c")
 , takeExactly 3 (C.pack "xyz")   ==>  Just (C.pack "xyz", C.empty)
 , takeExactly 4 (C.pack "123")   ==>  Nothing
 , takeExactly 0 (C.pack "x")     ==>  Just (C.empty, C.pack "x")
 ]
