{-# OPTIONS_GHC -Wall #-}
module BCD (bcd) where

-- s = "0673762660"

bcd :: String -> [String]
bcd s = let (w, s') = f s
        in if null s' then [w] else (w:bcd s')
    where
      f (b:c:rest) = (c:[b], rest)
      f cs = ('f':cs, [])
