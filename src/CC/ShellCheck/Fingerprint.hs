{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CC.ShellCheck.Fingerprint
    ( issueFingerprint
    ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import ShellCheck.Interface
    ( Comment(..)
    , Position(..)
    , PositionedComment(..)
    )

import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.Text as T

-- | Given a positioned comment and the file's contents, generate a fingerprint
--   unique to that issue
issueFingerprint :: PositionedComment -> Text -> Text
issueFingerprint (PositionedComment Position{..} _ (Comment _ code _)) script =
    md5 $ T.intercalate "|"
        [ T.pack $ posFile
        , T.pack $ show code
        , T.filter (not . isSpace) $ fetchLine (fromIntegral posLine) script
        ]

md5 :: Text -> Text
md5 = T.pack . show . MD5.md5 . encodeUtf8 . fromStrict

fetchLine :: Int -> Text -> Text
fetchLine idx = fromMaybe "" . safeIndex (idx - 1) . T.lines

safeIndex :: Int -> [a] -> Maybe a
safeIndex idx xs
    | idx >= 0 && idx < length xs = Just $ xs !! idx
    | otherwise = Nothing
