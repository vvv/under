-- | Type-length-value decoding operations.
module Lab.TLV ( Tag (..), tag ) where

import BBTest.Util (takeExactly)

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.State
import Data.Char (isDigit)

type TagID = Char

data Tag = ComplU TagID C.ByteString -- ^ Complex tag with unparsed content
--          | Compl TagID [Tag]         -- ^ Complex tag
         | Prim TagID C.ByteString   -- ^ Primitive tag
           deriving (Show, Eq)

tag :: C.ByteString -> (Either String Tag, C.ByteString)
tag = runState tag1

tag1 :: State C.ByteString (Either String Tag)
tag1 = do str <- get
          case C.uncons str of
            Nothing       -> err "EOF"
            Just ('F', s) -> put s >> tag1
            Just ('P', s) -> g Prim   2 s
            Just ('C', s) -> g ComplU 3 s
            _ -> err "unknown tag type"
    where
      err = return . Left
      g mktag nbytes s =
          case takeExactly nbytes s of
            Nothing -> err "incomplete tag"
            Just (idlen, s') ->
                let (tid, len) = (C.head idlen, C.tail idlen)
                in if C.all isDigit len
                   then case takeExactly (read $ C.unpack len) s' of
                          Nothing -> err "not enough content octets"
                          Just (content, rest) -> do
                                      put rest
                                      return $ Right $ mktag tid content
                   else err "invalid length encoding"

{-
tag :: C.ByteString -> (Either String Tag, C.ByteString)
tag str = case C.uncons str of
            Nothing        -> (Left "EOF", str)
            Just ('F', s1) -> tag s1
            Just ('P', s1) -> readTag Prim   2 s1
            Just ('C', s1) -> readTag ComplU 3 s1
            _ -> (Left "unknown tag type", str)
    where
      readTag mktag nbytes s =
          case takeExactly nbytes s of
            Nothing -> (Left "incomplete tag", str)
            Just (idlen, s') ->
                let (tid, len) = (C.head idlen, C.tail idlen)
                in if C.all isDigit len
                   then case takeExactly (read $ C.unpack len) s' of
                          Nothing -> (Left "not enough content octets", str)
                          Just (content, rest)
                              -> (Right $ mktag tid content, rest)
                   else (Left "invalid length encoding", str)
-}
