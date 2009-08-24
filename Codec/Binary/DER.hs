{-# LANGUAGE CPP #-}
---- {-# OPTIONS_GHC -Wall #-}
-- | Distinguished Encoding Rules (DER) decoding/encoding.
--
-- DER's are specified by `X.690' ASN.1 recommendation (see
-- <http://www.itu.int/ITU-T/studygroups/com17/languages/>).
--
-- XXX TODO:
--
--   - 'toSexp' can be implemented with 'Writer' monad
--
module Codec.Binary.DER
    ( -- * Types
      Tag(..)
    , TagClass(..)
    , TagNum
    -- * Parsing
    , Err(..)
    , parseTag
    , parseTags
    , deepen
    -- * S-expressions
    , toSexp
    , toSexp'

#ifdef TESTING
    -- * Testing-only exports
    -- ** Parsing API
    , runParser
    -- ** Parser implementations
    , tagID
    , tagID1
    , tagNum
    , tagLen
    -- ** Encoders/decoders
    , enLen
#endif
    ) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Lazy (ByteString)

import Control.Monad.State (State, runState, get, put)
import Control.Monad.Error (Error(..), ErrorT, runErrorT, throwError)

import Data.Bits
import Data.Char (ord, chr, intToDigit)
import Data.List (foldl')
import Data.Maybe (maybe)
import Data.Either (partitionEithers)

------------------------------------------------------------------------
-- Types

-- | String to parse + file position (1-based byte address)
type StrPos = (ByteString, Int)

-- | ASN.1 tag
data Tag = Prim TagClass TagNum ByteString -- ^ primitive tag
         | Cons TagClass TagNum [Tag]      -- ^ constructed tag
         | ConsU TagClass TagNum StrPos -- ^ constructed tag with unparsed
                                        --   contents
           deriving (Eq, Show)

data TagClass = Universal
              | Application
              | ContextSpecific
              | Private
                deriving (Eq, Show)

type TagNum = Int

------------------------------------------------------------------------
-- Parsing API

type Parser a = ErrorT Err (State StrPos) a

data Err = Err String -- ^ parsing error
         | EOF        -- ^ end of file
         deriving (Eq, Show)

instance Error Err where
    strMsg = Err

runParser :: Parser a -> StrPos -> (Either Err a, StrPos)
runParser m str = runState (runErrorT m) str

------------------------------------------------------------------------
-- Concrete parsers

-- | Identifier octets parser.
--
-- Returned value is made of:
--
--   - \"constructed\" tag predicate ('False' for primitive tags),
--
--   - tag class,
--
--   - tag number.
tagID :: Parser (Bool, TagClass, TagNum)
tagID = do (s, pos) <- get
           case C.uncons s of
             Nothing -> err "tagID: no identifier octets to parse"
             Just (c, s') -> do put (s', pos+1)
                                let (consp, cls, m) = tagID1 c
                                num <- (maybe tagNum return) m
                                return (consp, cls, num)

-- | Parse the first identifier octet.
--
-- Returned value is made of:
--
--   - whether tag encoding is constructed ('False' if tag is primitive),
--
--   - tag class,
--
--   - 'Just' \<tag number\> (if tag number \<= 30) or 'Nothing' (\> 30).
tagID1 :: Char -> (Bool, TagClass, Maybe TagNum)
tagID1 c = (consp, cls, mnum)
    where
      cls = [Universal, Application, ContextSpecific, Private]
            !! ((n .&. 0xc0) `shiftR` 6)
      consp = testBit n 5
      mnum = case n .&. 0x1f of { 0x1f -> Nothing; n' -> Just n' }
      n = ord c

-- | Tag number parser.
tagNum :: Parser TagNum
tagNum = do (s, pos) <- get
            case acc [] s of
              Nothing -> err "tagNum: invalid tag number encoding"
              Just bs -> do let n = length bs
                            put (C.drop (fromIntegral n) s, pos+n)
                            return (foldl' merge 0 bs)
    where
      acc bs s = case C.uncons s of
                   Nothing      -> Nothing
                   Just (c, s') -> let b   = ord c
                                       bs' = bs ++ [b .&. 0x7f]
                                   in if testBit b 7
                                      then acc bs' s'
                                      else Just bs'
      merge x y = (x `shiftL` 7) .|. y

-- | Length octets parser.
tagLen :: Parser Int
tagLen = do
  (s, pos) <- get
  case C.uncons s of
    Nothing      -> e
    Just (c, s') -> do let n = ord c
                       if testBit n 7
                         then -- long form
                             let nb = fromIntegral (n .&. 0x7f)
                                 (bs, rest) = C.splitAt nb s'
                             in if C.length bs /= nb
                                then e
                                else do
                                  put (rest, pos+1 + fromIntegral nb)
                                  return (C.foldl' merge 0 bs)
                         else -- short form
                             put (s', pos+1) >> return n
    where
      e = err "tagLen: too few length octets"
      merge n c = (n `shiftL` 8) .|. ord c

-- | Skip filling characters.
skip :: [Char] -> Parser ()
skip fillers = do (s, pos) <- get
                  case C.uncons s of
                    Nothing      -> eof
                    Just (c, s') -> if c `elem` fillers
                                    then put (s', pos+1) >> skip fillers
                                    else return ()

-- | ASN.1 value parser.
tag :: Parser Tag
tag = do skip "\xff"
         (consp, cls, num) <- tagID
         len               <- tagLen
         (s, pos)          <- get
         let nb = fromIntegral len
             (content, s') = C.splitAt nb s
         if C.length content /= nb
           then err "tag: not enough contents octets"
           else do put (s', pos+len)
                   return $ if consp
                            then ConsU cls num (content, pos)
                            else Prim  cls num content

------------------------------------------------------------------------
-- Exported parsers

parseTag :: StrPos -> (Either Err Tag, StrPos)
parseTag = runParser tag

-- | Parse tags until the end of file or error.
parseTags :: StrPos -> [Either Err Tag]
parseTags = acc []
    where
      acc ts sp = let (r, sp') = parseTag sp
                      ts'      = ts ++ [r]
                  in case r of
                       Right _  -> acc ts' sp'
                       Left EOF -> ts
                       _        -> ts'

-- | Parse contents of 'ConsU' tag.
--
-- For other tags @deepen = Right@.
deepen :: Tag -> Either Err Tag
deepen (ConsU cls num sp) = let (ls, rs) = partitionEithers (parseTags sp)
                            in if null ls
                               then Right (Cons cls num rs)
                               else Left (head ls)
deepen t = Right t

------------------------------------------------------------------------
-- Encoders/decoders

-- | Encode tag length.
enLen :: Int -> ByteString
enLen n | n < 0   = e
        | n < 128 = C.singleton (chr n)
        | True = let bs = bytes [] n
                     nb = length bs
                 in if nb > 0x7f
                    then e
                    else C.pack $ map chr ((nb .|. 0x80):bs)
    where
      e = error ("enLen: tag length cannot be encoded: " ++ show n)

      bytes bs 0 = bs
      bytes bs x = bytes ((x .&. 0xff):bs) (x `shiftR` 8)

------------------------------------------------------------------------
-- S-expressions


-- | Convert tag to a \"decomposed\" S-expression.
--
-- @Right@ elements of the resulting list can be written to stdout;
-- @Left@ is an error notification.
toSexp :: Tag -> [Either Err ByteString]
toSexp t = case t of
             Prim cls num s -> header cls num
                               ++ [ rpk $ ' ':'"':hexDump (C.unpack s) ]
                               ++ [ rch '"' ]
                               ++ footer
             Cons cls num ts -> header cls num ++ repr ts ++ footer
             _ -> case deepen t of
                    Left e   -> [Left e]
                    Right t' -> toSexp t'
    where
      header cls num = [ rpk $ '(':char cls:show num ]
      footer = [ rch ')' ]

      repr = concatMap $ ((:) (rch ' ')) . toSexp

      char Universal       = 'u'
      char Application     = 'a'
      char ContextSpecific = 'c'
      char Private         = 'p'

      rch = Right . C.singleton
      rpk = Right . C.pack

-- | A strict variant of 'toSexp'.
toSexp' :: Tag -> Either Err ByteString
toSexp' t = let (ls, rs) = partitionEithers (toSexp t)
            in if null ls
               then Right (C.concat rs)
               else Left (head ls)

------------------------------------------------------------------------
-- Utility functions

err msg = do (_, pos) <- get
             throwError . Err $ msg ++ ": byte " ++ show pos

eof = throwError EOF

hexDump :: String -> String
hexDump = unwords . map (f . (`quotRem` 16) . ord)
    where
      f (q, r) = (intToDigit q):[intToDigit r]
