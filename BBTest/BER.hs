---- {-# OPTIONS_GHC -Wall #-}
-- | Basic Encoding Rules (BER) parsing
--
-- XXX TODO:
--
--  * refactor `tagID' and `tagInfo'
--
--  * test `parseTags'
--
--  * splitAt' to be defined locally
--
--  * configurable fillers ('\xff', etc.)
--
--  * move `err' and `eof' to "BBTest.Parse"?
--
module BBTest.BER (
                   Tag(..),
                   TagClass(..),
                   TagNum,
                   parseTag,
                   parseTags,

                   -- testing only (?)
                   parseTagID,
                   tagInfo,
                   parseTagNum,
                  ) where

import BBTest.Parse
-- import BBTest.Util (splitAt') -- XXX define locally?

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)
import Data.Bits
import Data.Char (ord)
import Data.List (foldl')

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

-- | Get information from the first tag identifier octet.
--
-- Returned value consists of:
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
      consp = n `testBit` 5
      mnum = case n .&. 0x1f of { 0x1f -> Nothing; n' -> Just n' }
      n = ord c

------------------------------------------------------------------------
err msg = do (_, pos) <- get
             throwError . Err $ msg ++ ": byte " ++ show pos

eof = throwError EOF

tagID :: Parser (BStr -> Tag)
tagID = do (s, pos) <- get
           case C.uncons s of
             Nothing -> eof
             Just ('\xff', s') -> put (s', pos+1) >> tagID
             Just (c, s') -> do put (s', pos+1)
                                let (cls, consp, mnum) = tagInfo c
                                    f = if consp then ConsU else Prim
                                case mnum of
                                  Just num -> return (f cls num)
                                  Nothing  -> -- ``high'' tag number (>= 31)
                                          do num <- tagNum
                                             return (f cls num)

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

tag :: Parser Tag
tag = undefined
-- tag = do
--   g <- tagID
--   n <- tagLen
--   (s, pos) <- get
--   case splitAt' n s of
--     Nothing         -> err "not enough contents octets"
--     Just (cont, s') -> put (s', pos + n) >> return (g cont)

------------------------------------------------------------------------
parseTag :: StrPos -> (Either Err Tag, StrPos)
parseTag = runParser tag

parseTagID :: StrPos -> (Either Err (BStr -> Tag), StrPos)
parseTagID = runParser tagID

parseTagNum :: StrPos -> (Either Err TagNum, StrPos)
parseTagNum = runParser tagNum

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
