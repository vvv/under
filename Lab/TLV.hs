-- | Type-length-value decoding operations.
module Lab.TLV where
-- module Lab.TLV ( Tag (..), tag ) where

import BBTest.Util (takeExactly)

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isDigit)

type BStr = C.ByteString

type TagID = Char

data Tag = Prim TagID BStr   -- ^ Primitive tag
--          | Compl TagID [Tag] -- ^ Complex tag
         | ComplU TagID BStr -- ^ Complex tag with unparsed content
           deriving (Show, Eq)

example = C.pack "PE3Howdy!"    -- XXX
test    = runEval4 tag4 example -- XXX
testErr = runEval4 tag4 C.empty -- XXX
tag = runEval4 tag4 -- XXX

-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

type Eval4 a = ErrorT String (StateT BStr Identity) a
runEval4 :: Eval4 a -> BStr -> (Either String a, BStr)
runEval4 ev s = runIdentity (runStateT (runErrorT ev) s)

tag4 :: Eval4 Tag
tag4 = do s <- get
          case C.uncons s of
            Nothing -> throwError "EOF"
            Just ('F', s') -> put s' >> tag4
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


type Eval3 a = ErrorT String Identity a
runEval3 :: Eval3 a -> Either String a
runEval3 = runIdentity . runErrorT

tag3 :: BStr -> Eval3 Tag
tag3 str = case C.uncons str of
             Nothing -> throwError "EOF"
             Just ('F', s) -> tag3 s
             Just ('P', s) -> g Prim 2 s
             Just ('C', s) -> g ComplU 3 s
             _ -> throwError "unknown tag type"
    where
      g mktag nbytes s =
          case takeExactly nbytes s of
            Nothing -> throwError "incomplete tag"
            Just (idlen, s') ->
                let (tid, len) = (C.head idlen, C.tail idlen)
                in if C.all isDigit len
                   then case takeExactly (read $ C.unpack len) s' of
                          Nothing -> throwError "not enough content octets"
                          Just (content, _) -> return (mktag tid content)
                   else throwError "invalid length encoding"


type Eval2 a = Identity a
runEval2 :: Eval2 a -> a
runEval2 = runIdentity

tag2 :: BStr -> Eval2 Tag
tag2 str = case C.uncons str of
             Nothing -> error "EOF"
             Just ('F', s) -> tag2 s
             Just ('P', s) -> g Prim 2 s
             Just ('C', s) -> g ComplU 3 s
             _ -> error "unknown tag type"
    where
      g :: (TagID -> BStr -> Tag) -> Int -> BStr -> Identity Tag
      g mktag nbytes s =
          case takeExactly nbytes s of
            Nothing -> error "incomplete tag"
            Just (idlen, s') ->
                let (tid, len) = (C.head idlen, C.tail idlen)
                in if C.all isDigit len
                   then case takeExactly (read $ C.unpack len) s' of
                          Nothing -> error "not enough content octets"
                          Just (content, _) -> return (mktag tid content)
                   else error "invalid length encoding"


tag1 :: State BStr (Either String Tag)
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


tag0 :: BStr -> (Either String Tag, BStr)
tag0 str = case C.uncons str of
            Nothing        -> (Left "EOF", str)
            Just ('F', s1) -> tag0 s1
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
