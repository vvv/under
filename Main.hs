{-# OPTIONS_GHC -Wall #-}
module Main where

import Codec.Binary.DER

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy (ByteString)

import System.Environment (getArgs)
------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= mapM_ (\fn -> L.readFile fn >>= process 1)

process :: Int -> ByteString -> IO ()
process pos str =
    let (m, (str', pos')) = parseTag (str, pos)
    in case m of
         Left (Err msg) -> fail msg
         Left EOF       -> return ()
         Right tag -> case toSexp' tag of
                        Left (Err msg) -> fail msg
                        Left EOF       -> fail "process: unexpected EOF"
                        Right repr     -> L.putStrLn repr
                                          >> process pos' str'
