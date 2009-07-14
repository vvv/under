-- | Type-length-value decoding operations.
module Lab.TLV ( Tag (..), tag ) where

import BBTest.Util (takeExactly)

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isDigit)
-- import Control.Monad.State

type TagID = Char

data Tag = ComplU TagID C.ByteString -- ^ Complex tag with unparsed content
--          | Compl TagID [Tag]         -- ^ Complex tag
         | Prim TagID C.ByteString   -- ^ Primitive tag
           deriving (Show, Eq)

tag :: C.ByteString -> (Either String Tag, C.ByteString)

tag str = case C.uncons str of
            Nothing        -> (Left "EOF", C.empty)
            Just ('F', s1) -> tag s1
            Just ('P', s1) -> readTag Prim   2 s1
            Just ('C', s1) -> readTag ComplU 3 s1
            Just _ -> (Left "unknown tag type", str)
    where
      readTag tagType nbytes s =
          case takeExactly nbytes s of
            Nothing -> (Left "incomplete tag", str)
            Just (idlen, s') ->
                let (tid, len) = (C.head idlen, C.tail idlen)
                in if C.all isDigit len
                   then case takeExactly (read $ C.unpack len) s' of
                          Nothing -> (Left "not enough content octets", str)
                          Just (content, rest)
                              -> (Right $ tagType tid content, rest)
                   else (Left "invalid length encoding", str)
