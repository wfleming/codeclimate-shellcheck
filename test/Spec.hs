{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BSL
import           Data.String.QQ
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           Test.Tasty
import           Test.Tasty.Hspec

import           Data.Shebang

main :: IO ()
main = do
  sbSpecs <- testSpec "Shebang Specs"  shebangSpecs
  defaultMain (tests $ testGroup "All specs" [sbSpecs])

tests :: TestTree -> TestTree
tests specs = testGroup "Engine Tests" [specs]

shebangSpecs :: Spec
shebangSpecs = describe "Shebang parsing" $ do
  describe "decode" $ do
    it "should parse a valid shebang without optional args" $ do
      let subject = [s|#!/bin/sh
                       echo "hello world"|]
      let result = decode subject
      result `shouldBe` Just (Shebang (Interpretter "/bin/sh") Nothing)

    it "should parse a valid shebang with optional args" $ do
      let subject = [s|#!/bin/env ruby
                       puts "hello world"|]
      let result = decode subject
      result `shouldBe` Just (Shebang (Interpretter "/bin/env") (Just (Argument "ruby")))

  describe "decodeEither" $ do
    it "should parse a valid shebang without optional args" $ do
      let subject = [s|#!/bin/sh
                       echo "hello world"|]
      let result = decodeEither subject
      result `shouldBe` Right (Shebang (Interpretter "/bin/sh") Nothing)

  describe "hasShebang" $ do
    it "should be able to detect a valid shebang" $ do
      let subject = "#!/bin/sh"
      let result = hasShebang subject
      result `shouldBe` True

    it "should be able to detect an invalid shebang" $ do
      let subject = ""
      let result = hasShebang subject
      result `shouldBe` False

  describe "readFirstLine" $ do
    it "should only read the first line" $ do
      let contents = [s|#!/bin/sh
                        echo "hello world"|]
      withinTempDir $ do
        let subject = "example.sh"
        createFile subject contents
        result <- readFirstLine subject
        result `shouldBe` "#!/bin/sh"

withinTempDir :: IO a -> IO a
withinTempDir act = withSystemTempDirectory "cc-hlint" $ \tmp -> do
    E.bracket getCurrentDirectory setCurrentDirectory $ \_ ->
        setCurrentDirectory tmp >> act

createFile :: FilePath -> BSL.ByteString -> IO ()
createFile path content = do
    createDirectoryIfMissing True $ takeDirectory path
    BSL.writeFile path content
