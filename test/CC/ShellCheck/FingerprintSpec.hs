{-# LANGUAGE OverloadedStrings #-}

module CC.ShellCheck.FingerprintSpec
    ( main
    , fingerprintSpecs
    ) where

import CC.ShellCheck.Fingerprint

import Data.Text (Text)
import ShellCheck.Interface
    ( Code
    , Comment(..)
    , Position(..)
    , PositionedComment(..)
    , Severity(..)
    )
import Test.Hspec

import qualified Data.Text as T

main :: IO ()
main = hspec fingerprintSpecs

fingerprintSpecs :: Spec
fingerprintSpecs = describe "issueFingerprint" $ do
    it "uniquely identifies an issue" $ do
        let content = T.unlines
                [ "#!/bin/sh"
                , ""
                , "foo = $*"
                , ""
                , "bar = $*"
                , ""
                ]
            fp1 = fingerprint 3 123 content
            fp2 = fingerprint 5 123 content
            fp3 = fingerprint 5 456 content

        fp1 `shouldNotBe` fp2
        fp2 `shouldNotBe` fp3

    it "is robust against the issue moving" $ do
        let fp1 = fingerprint 3 123 $ T.unlines
                [ "#!/bin/sh"
                , ""
                , "foo = $*"
                ]
            fp2 = fingerprint 5 123 $ T.unlines
                [ "#!/bin/sh"
                , ""
                , ""
                , ""
                , "foo = $*"
                ]

        fp1 `shouldBe` fp2

    it "is robust against whitespace" $ do
        let fp1 = fingerprint 3 123 $ T.unlines
                [ "#!/bin/sh"
                , ""
                , "foo = $*"
                ]
            fp2 = fingerprint 3 123 $ T.unlines
                [ "#!/bin/sh"
                , ""
                , "foo =  $*"
                ]

        fp1 `shouldBe` fp2

fingerprint :: Integer -> Code -> Text -> Text
fingerprint ln code =
  issueFingerprint $ PositionedComment (position ln) unused (comment code)
  where
    unused :: a
    unused = error "end position comment not implemented"

position :: Integer -> Position
position ln = Position
    { posFile = "foo.sh"
    , posLine = ln
    , posColumn = 0
    }

comment :: Code -> Comment
comment code = Comment ErrorC code ""
