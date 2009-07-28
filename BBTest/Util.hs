module BBTest.Util (splitAt') where

import qualified Data.ByteString.Lazy.Char8 as C

splitAt' :: Int -> C.ByteString -> Maybe (C.ByteString, C.ByteString)
splitAt' = grow C.empty
    where
      grow acc n str | n == 0 = Just (C.reverse acc, str)
                     | n < 0  = Nothing
                     | True   = case C.uncons str of
                                  Just (c, rest) ->
                                      grow (c `C.cons` acc) (n-1) rest
                                  Nothing -> Nothing
