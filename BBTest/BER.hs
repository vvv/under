{-# OPTIONS_GHC -Wall #-}
-- | Basic Encoding Rules (BER) parsing
--
-- XXX TODO:
--
--  * `takeExactly' to be defined locally
--
--  * configurable fillers ('\xff', etc.)
--
module BBTest.BER (
                   parseTag,
                   parseTags,
                   Tag(..),
                   TagID,
                   TagClass(..),

                   -- testing only (?)
                   tagInfo,
                  ) where

import BBTest.Parse
-- import BBTest.Util (takeExactly) -- XXX define locally?

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)
import Data.Bits ((.&.), shiftR, testBit)
import Data.Char (ord)

-- | ASN.1 tag
data Tag = Prim TagID BStr  -- ^ primitive tag
         | Cons TagID [Tag] -- ^ constructed tag
         | ConsU TagID BStr -- ^ constructed tag with unparsed contents
           deriving (Eq, Show)

type TagID = (TagClass, Int)

data TagClass = Universal
              | Application
              | ContextSpecific
              | Private
                deriving (Eq, Show)

-- | Get tag information from the identifier octet.
--
-- Returned value consists of:
--
--   - tag class,
--
--   - whether tag encoding is constructed ('False' = primitive)
--
--   - 'Just' \<tag number\> (if tag number \<= 31) or 'Nothing' (\>= 32).
tagInfo :: Char -> (TagClass, Bool, Maybe Int)
tagInfo c = (cls, consp, mnum)
    where
      cls = [Universal, Application, ContextSpecific, Private]
            !! ((n .&. 0xc0) `shiftR` 6)
      consp = n `testBit` 5
      mnum = case n .&. 0x1f of { 0x1f -> Nothing; n' -> Just n' }
      n = ord c

tag :: Parser Tag
tag = do (s, pos) <- get
         case C.uncons s of
           Nothing -> throwError EOF
           Just ('\xff', s') -> put (s', pos+1) >> tag
           Just (c, s') -> let (cls, consp, Just num) = tagInfo c -- XXX Just
                               f = if consp then ConsU else Prim
                           in return $ f (cls, num) s'
--     where
--       err msg pos = throwError . Err $ msg ++ ": byte " ++ show pos

parseTag :: StrPos -> (Either Err Tag, StrPos)
parseTag = runParser tag

-- | Parse tags until error or end of file
parseTags :: StrPos -> [Either Err Tag]
parseTags = acc []
    where
      acc ts sp = let (r, sp') = parseTag sp
                      ts'      = ts ++ [r]
                  in case r of
                       Right _  -> acc ts' sp'
                       Left EOF -> ts
                       _        -> ts'
