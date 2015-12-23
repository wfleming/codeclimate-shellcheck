{-# LANGUAGE OverloadedStrings #-}

module Data.Shebang (
  -- * Decoding
    decode
  , decodeEither
  -- * Smoke tests
  , hasShebang
  -- * Reading
  , readFirstLine
  -- * Core types
  , Shebang(..)
) where

import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Word

--------------------------------------------------------------------------------

-- | Value representing a Shebang of the form #!interpreter [optional-arg].
data Shebang = Shebang BS.ByteString BS.ByteString

--------------------------------------------------------------------------------

-- | Checks the magic characters of a ByteString
hasShebang :: BSL.ByteString -> Bool
hasShebang x = BSL.take 2 x == "#!"

--------------------------------------------------------------------------------

-- | Reads first line of a file.
readFirstLine :: FilePath -> IO BSL.ByteString
readFirstLine path = do
  contents <- BSL.readFile path
  return $ BSL.takeWhile (\x -> not $ x == 10 || x == 13) contents

--------------------------------------------------------------------------------

-- | Takes a ByteString and maybe returns a Shebang.
decode :: BSL.ByteString -> Maybe Shebang
decode = maybeResult . parse shebang

--------------------------------------------------------------------------------

-- | Takes a ByteString and returns a Shebang or an error string.
decodeEither :: BSL.ByteString -> Either String Shebang
decodeEither = eitherResult . parse shebang

--------------------------------------------------------------------------------

-- | Simple Shebang parser.
shebang :: Parser Shebang
shebang = do
  _            <- string "#!"
  interpretter <- takeTill whitespace
  arguments    <- option "" $ do
    _          <- string " "
    takeTill endOfLine
  return $ Shebang interpretter arguments

----------------------------------------------------------------------------

-- | Character representing a carriage return.
carriageReturn :: Word8 -> Bool
carriageReturn = (==) 13

----------------------------------------------------------------------------

-- | Characters representing end of line.
endOfLine :: Word8 -> Bool
endOfLine x = newline x || carriageReturn x

----------------------------------------------------------------------------

-- | Character representing a new line.
newline :: Word8 -> Bool
newline = (==) 10

----------------------------------------------------------------------------

-- | Character representing spaces.
whitespace :: Word8 -> Bool
whitespace = (==) 32
