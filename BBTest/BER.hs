-- | Basic Encoding Rules (BER) parsing
--
-- XXX TODO:
--
--  * `takeExactly' to be defined locally
--
--  * configurable fillers ('\xff', etc.)
--
module BBTest.BER (
                   TagID(..),
                   Tag(..),
                   parseTag,
                   parseTags,
                   tagClass
                  ) where

import BBTest.Parse
-- import BBTest.Util (takeExactly) -- XXX define locally?

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)
import Data.Bits ((.&.), shiftR)
import Data.Char (ord)

-- | ASN.1 tag
data Tag = Prim TagID BStr  -- ^ primitive tag
         | Cons TagID [Tag] -- ^ constructed tag
         | ConsU TagID BStr -- ^ constructed tag with unparsed contents
           deriving (Eq, Show)

-- | Tag identifier
data TagID = Universal Int
           | Application Int
           | ContextSpecific Int
           | Private Int
             deriving (Eq, Show)

tag :: Parser Tag
tag = do (s, pos) <- get
         case C.uncons s of
           Nothing -> throwError EOF
           Just ('\xff', s') -> put (s', pos+1) >> tag
           Just (c, s') -> return $ Prim (tagClass c $ 666) s'
    where
      err msg pos = throwError . Err $ msg ++ ": byte " ++ show pos

tagClass :: Char -> Int -> TagID
tagClass c = [Universal, Application, ContextSpecific, Private]
             !! (((ord c) .&. 0xc0) `shiftR` 6)

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
