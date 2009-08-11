---- {-# OPTIONS_GHC -Wall #-}
-- | Basic Encoding Rules (BER) decoding.
--
-- BER are specified by `X.690' ASN.1 recommendation (see
-- <http://www.itu.int/ITU-T/studygroups/com17/languages/>).
--
-- XXX TODO:
--
--  * hide testing-only exports by preprocessing directive
--
--  * configurable fillers ('\xff', etc.)
--
--  * (?) move `err' and `eof' to "BBTest.Parse"
--
module BBTest.BER (
 -- * Types
   Tag(..)
 , TagClass(..)
 , TagNum
 -- * High-level parsers
 , parseTag
 , parseTags
 -- * Encoders/decoders
 , toSexp

 -- * Testing-only exports
 -- ** Parser implementations
 , tagInfo
 , tagNum
 , tagID
 , tagLen
 -- ** Encoders/decoders
 , enLen
 -- ** Utility function(s)
 , splitAt'
 ) where

import BBTest.Parse

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)
import Data.Bits
import Data.Char (ord, chr, intToDigit)
import Data.List (foldl')

------------------------------------------------------------------------
-- Types

-- | ASN.1 tag
data Tag = Prim TagClass TagNum BStr  -- ^ primitive tag
         | Cons TagClass TagNum [Tag] -- ^ constructed tag
         | ConsU TagClass TagNum BStr -- ^ cons. tag with unparsed contents
           deriving (Eq, Show)

data TagClass = Universal
              | Application
              | ContextSpecific
              | Private
                deriving (Eq, Show)

type TagNum = Int

------------------------------------------------------------------------
-- Utility functions

err msg = do (_, pos) <- get
             throwError . Err $ msg ++ ": byte " ++ show pos

eof = throwError EOF

infixr 5 <:>
(<:>) = C.cons

splitAt' :: Int -> C.ByteString -> Maybe (C.ByteString, C.ByteString)
splitAt' = grow C.empty
    where
      grow acc n str | n == 0 = Just (C.reverse acc, str)
                     | n < 0  = Nothing
                     | True   = case C.uncons str of
                                  Just (c, rest) ->
                                      grow (c <:> acc) (n-1) rest
                                  Nothing -> Nothing

hexDump :: String -> String
hexDump = unwords . map (f . (`quotRem` 16) . ord)
    where
      f (q, r) = (intToDigit q):[intToDigit r]

hexDump' :: BStr -> BStr
hexDump' = C.pack . hexDump . C.unpack

------------------------------------------------------------------------
-- Parser implementations

-- | Get information from the first tag identifier octet.
--
-- Returned value is made of:
--
--   - tag class,
--
--   - whether tag encoding is constructed ('False' = primitive)
--
--   - 'Just' \<tag number\> (if tag number \<= 30) or 'Nothing' (\> 30).
tagInfo :: Char -> (TagClass, Bool, Maybe TagNum)
tagInfo c = (cls, consp, mnum)
    where
      cls = [Universal, Application, ContextSpecific, Private]
            !! ((n .&. 0xc0) `shiftR` 6)
      consp = testBit n 5
      mnum = case n .&. 0x1f of { 0x1f -> Nothing; n' -> Just n' }
      n = ord c

tagNum :: Parser TagNum
tagNum = do (s, pos) <- get
            case accum [] s of
              Nothing -> err "invalid tag number encoding"
              Just bs -> do let n = length bs
                            put (C.drop (fromIntegral n) s, pos + n)
                            return (foldl' merge 0 bs)
    where
      accum bs s = case C.uncons s of
                     Nothing      -> Nothing
                     Just (c, s') -> let b   = ord c
                                         bs' = bs ++ [b .&. 0x7f]
                                     in if testBit b 7
                                        then accum bs' s'
                                        else Just bs'
      merge x y = (x `shiftL` 7) .|. y

-- | Identifier octets parser.
tagID :: Parser (BStr -> Tag)
tagID = do (s, pos) <- get
           case C.uncons s of
             Nothing -> eof
             Just ('\xff', s') -> -- skip filler
                 put (s', pos+1) >> tagID
             Just (c, s') -> do put (s', pos+1)
                                let (cls, consp, mnum) = tagInfo c
                                    f = if consp then ConsU else Prim
                                case mnum of
                                  Just num -> return (f cls num)
                                  Nothing  -> -- ``high'' tag number (>= 31)
                                          do num <- tagNum
                                             return (f cls num)

-- | Length octets parser.
tagLen :: Parser Int
tagLen = do
  (s, pos) <- get
  case C.uncons s of
    Nothing -> e
    Just (c, s') -> do let n = ord c
                       if testBit n 7
                         then -- long form
                             case splitAt' (n .&. 0x7f) s' of
                               Nothing -> e
                               Just (bs, rest) -> do
                                 put (rest, pos+1 + fromIntegral (C.length bs))
                                 return (C.foldl' merge 0 bs)
                         else -- short form
                             put (s', pos+1) >> return n
    where
      e = err "invalid tag length encoding"
      merge n c = (n `shiftL` 8) .|. ord c

tag :: Parser Tag
tag = do mkTag <- tagID
         n <- tagLen
         (s, pos) <- get
         case splitAt' n s of
           Nothing -> err "not enough contents octets"
           Just (cont, s') -> put (s', pos + n) >> return (mkTag cont)

------------------------------------------------------------------------
-- High-level parsers

parseTag :: StrPos -> (Either Err Tag, StrPos)
parseTag = runParser tag

-- | Parse tags until error or end of file.
parseTags :: StrPos -> [Either Err Tag]
parseTags = acc []
    where
      acc ts sp = let (r, sp') = parseTag sp
                      ts'      = ts ++ [r]
                  in case r of
                       Right _  -> acc ts' sp'
                       Left EOF -> ts
                       _        -> ts'

------------------------------------------------------------------------
-- Encoders/decoders

-- | Encode tag length.
enLen :: Int -> BStr
enLen n | n < 0   = e
        | n < 128 = C.singleton (chr n)
        | True = let bs = bytes [] n
                     nb = length bs
                 in if nb > 0x7f
                    then e
                    else C.pack $ map chr ((nb .|. 0x80):bs)
    where
      e = error "tag length cannot be encoded"

      bytes bs 0 = if null bs then [0] else bs
      bytes bs x = bytes ((x .&. 0xff):bs) (x `shiftR` 8)

-- | Convert tag to S-expression.
toSexp :: Tag -> C.ByteString
toSexp t = case t of
             Prim cls num bs ->
                 C.concat [ '(' <:> hd cls num
                          , ' ' <:> '"' <:> hexDump' bs
                          , C.pack "\")"
                          ]

             Cons cls num ts ->
                 C.concat [ '(' <:> hd cls num
                          , C.singleton ' '
                          , C.singleton ' ' `C.intercalate` map toSexp ts
                          , C.singleton ')'
                          ]

             ConsU _ _ _ ->
                 error "XXX not implemented"
    where
      hd :: TagClass -> TagNum -> BStr
      hd Universal       = f 'u'
      hd Application     = f 'a'
      hd ContextSpecific = f 'c'
      hd Private         = f 'p'

      f c = C.pack . (:) c . show
