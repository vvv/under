-- | Basic Encoding Rules (BER) parsing
--
-- XXX TODO:
--
--  * `takeExactly' to be defined locally
--
--  * configurable fillers ('\xff', etc.)
module BBTest.BER (TagID(..), Tag(..), parseTag) where

import BBTest.Parse
-- import BBTest.Util (takeExactly) -- XXX define locally?

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Error (throwError)
import Control.Monad.State (get, put)

-- | Tag identifier
data TagID = Universal Int
           | Application Int
           | ContextSpecific Int
           | Private Int
             deriving (Eq, Show)

-- | ASN.1 tag
data Tag = Prim TagID BStr  -- ^ primitive tag
         | Cons TagID [Tag] -- ^ constructed tag
         | ConsU TagID BStr -- ^ constructed tag with unparsed contents
           deriving (Eq, Show)

tag :: Parser Tag
tag = do (s, pos) <- get
         case C.uncons s of
           Nothing -> throwError EOF
           Just ('\xff', s') -> put (s', pos+1) >> tag
-- throwError . Err $ "XXX: byte " ++ show pos

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
