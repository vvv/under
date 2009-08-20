{-# OPTIONS_GHC -Wall #-}
module BBTest.Parse (BStr, StrPos, Err(..), Parser, runParser) where

import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Error
import Control.Monad.State (State, runState)

type BStr = C.ByteString

-- | String to be parsed + file position (1-based byte address)
type StrPos = (BStr, Int)

data Err = Err String -- ^ parsing error
         | EOF        -- ^ end of file
         deriving (Eq, Show)

instance Error Err where
    strMsg = Err

type Parser a = ErrorT Err (State StrPos) a

runParser :: Parser a -> StrPos -> (Either Err a, StrPos)
runParser p sp = runState (runErrorT p) sp
