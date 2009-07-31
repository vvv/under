module Tests.Util where

import Test.HUnit

infixr 1 ==>

(==>) :: (Eq a, Show a) => a -> a -> Test
x ==> y = TestCase (assertEqual "" y x)
