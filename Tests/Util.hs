module Tests.Util where

import Test.HUnit

(==>) :: (Eq a, Show a) => a -> a -> Test
x ==> y = TestCase (assertEqual "" y x)
