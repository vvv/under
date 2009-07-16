-- | Type-length-value decoding operations.
module Lab.TLV ( parse, Tag(..), tag ) where

import BBTest.Util (takeExactly)

import Control.Monad.Error
import Control.Monad.State

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isDigit)

type BStr = C.ByteString

------------------------------------------------------------------------
-- general parsing ``combinators''

type Parser a = ErrorT String (State BStr) a

runParser :: Parser a -> BStr -> (Either String a, BStr)
runParser ev s = runState (runErrorT ev) s

parse = runParser

------------------------------------------------------------------------
-- Tag data type and its parser

type TagID = Char

data Tag = Prim TagID BStr   -- ^ Primitive tag
--          | Compl TagID [Tag] -- ^ Complex tag
         | ComplU TagID BStr -- ^ Complex tag with unparsed content
           deriving (Show, Eq)

tag :: Parser Tag
tag = do s <- get
         case C.uncons s of
           Nothing -> throwError "EOF"
           Just ('F', s') -> put s' >> tag
           Just ('P', s') -> g Prim   2 s'
           Just ('C', s') -> g ComplU 3 s'
           _ -> throwError "unknown tag type"
    where
      g mktag nbytes s = do
        case takeExactly nbytes s of
          Nothing -> throwError "incomplete tag"
          Just (idlen, s') ->
              let (tid, len) = (C.head idlen, C.tail idlen)
              in if C.all isDigit len
                 then case takeExactly (read $ C.unpack len) s' of
                        Nothing -> throwError "not enough content octets"
                        Just (content, rest) -> do
                                    put rest
                                    return (mktag tid content)
                 else throwError "invalid length encoding"
