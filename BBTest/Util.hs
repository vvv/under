module BBTest.Util where

-- http://is.gd/1wSWU (haddock: Data.ByteString.Lazy.Char8)
import qualified Data.ByteString.Lazy.Char8 as C

takeExactly :: Int -> C.ByteString -> Maybe (C.ByteString, C.ByteString)
takeExactly = grow C.empty
    where
      grow acc n str | n == 0 = Just (C.reverse acc, str)
                     | n < 0  = Nothing
                     | True   = case C.uncons str of
                                  Just (c, rest) ->
                                      grow (c `C.cons` acc) (n-1) rest
                                  Nothing -> Nothing
