{-# OPTIONS_GHC -Wall #-}
-- | Type-length-value decoding operations.
module Lab.TLV (Err(..), parse, Tag(..), tag, parseTags) where

import BBTest.Util (splitAt')

import Control.Monad.Error
import Control.Monad.State

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isDigit)

type BStr = C.ByteString
type StrPos = (BStr, Int) -- ^ String to parse and file position (byte #)

data Err = EOF | Err String
           deriving (Show, Eq)

instance Error Err where
    strMsg = Err

------------------------------------------------------------------------
-- general parsing ``combinators''

type Parser a = ErrorT Err (State StrPos) a

runParser, parse :: Parser a -> StrPos -> (Either Err a, StrPos)
runParser ev s = runState (runErrorT ev) s
parse = runParser

------------------------------------------------------------------------
-- Tag data type and its parsers

type TagID = Char

data Tag = Prim TagID BStr   -- ^ Primitive tag
--          | Compl TagID [Tag] -- ^ Complex tag
         | ComplU TagID BStr -- ^ Complex tag with unparsed content
           deriving (Show, Eq)

tag :: Parser Tag
tag = do (s, pos) <- get
         case C.uncons s of
           Nothing -> throwError EOF
           Just ('F', s') -> put (s', pos+1) >> tag
           Just ('P', s') -> g Prim   2 s' pos
           Just ('C', s') -> g ComplU 3 s' pos
           _ -> err "unknown tag type" pos
    where
      err msg pos = throwError . Err $ msg ++ ": byte " ++ show pos

      g mktag nbytes s pos = do
        case splitAt' nbytes s of
          Nothing -> err "incomplete tag" pos
          Just (idlen, s') ->
              let (tid, slen) = (C.head idlen, C.tail idlen)
              in if C.all isDigit slen
                 then let len = read (C.unpack slen)
                      in case splitAt' len s' of
                           Nothing -> err "not enough content octets" pos
                           Just (content, rest)
                               -> do put (rest, pos + 1 + nbytes + len)
                                     return (mktag tid content)
                 else err "invalid length encoding" pos

parseTags :: StrPos -> [Either Err Tag]
parseTags = acc []
    where
      acc ts sp = let (r, sp') = parse tag sp
                      ts'      = ts ++ [r]
                  in case r of
                       Right _  -> acc ts' sp'
                       Left EOF -> ts
                       _        -> ts'
