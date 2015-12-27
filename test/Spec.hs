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

import           CC.ShellCheck.ShellScript
import           Data.Shebang

--------------------------------------------------------------------------------

main :: IO ()
main = do
  sbSpecs <- testSpec "Shebang Specs"  shebangSpecs
  ssSpecs <- testSpec "ShellScript Specs"  shellscriptSpecs
  defaultMain (tests $ testGroup "All specs" [ sbSpecs, ssSpecs ])

tests :: TestTree -> TestTree
tests specs = testGroup "Engine Tests" [ specs ]

--------------------------------------------------------------------------------

shebang :: BSL.ByteString
shebang = "#!/bin/sh"

shScript :: BSL.ByteString
shScript = [s|#!/bin/sh
            echo "hello world"|]

rbScript :: BSL.ByteString
rbScript = [s|#!/bin/env ruby
            puts "hello world"|]

--------------------------------------------------------------------------------

shebangSpecs :: Spec
shebangSpecs = describe "Shebang parsing" $ do
  describe "decode" $ do
    it "should parse a valid shebang without optional args" $ do
      let subject = shScript
      let result = decode subject
      result `shouldBe` Just (Shebang (Interpretter "/bin/sh") Nothing)

    it "should parse a valid shebang with optional args" $ do
      let subject = rbScript
      let result = decode subject
      result `shouldBe` Just (Shebang (Interpretter "/bin/env") (Just (Argument "ruby")))

  describe "decodeEither" $ do
    it "should parse a valid shebang without optional args" $ do
      let subject = shScript
      let result = decodeEither subject
      result `shouldBe` Right (Shebang (Interpretter "/bin/sh") Nothing)

  describe "hasShebang" $ do
    it "should be able to detect a valid shebang" $ do
      let subject = shebang
      let result = hasShebang subject
      result `shouldBe` True

    it "should be able to detect when shebang is missing" $ do
      let subject = BSL.empty
      let result = hasShebang subject
      result `shouldBe` False

  describe "readFirstLine" $ do
    it "should only read the first line" $ do
      let contents = shScript
      withinTempDir $ do
        let subject = "example.sh"
        createFile subject contents
        result <- readFirstLine subject
        result `shouldBe` shebang

--------------------------------------------------------------------------------

shellscriptSpecs :: Spec
shellscriptSpecs = describe "Shellscript validation and retrieval" $ do
  describe "isShellScript" $ do
    it "should be valid if file has .sh extension" $ do
      withinTempDir $ do
        let subject = "example.sh"
        createFile subject BSL.empty
        result <- isShellScript subject
        result `shouldBe` True

    it "should be valid if file has no extension but a valid shebang" $ do
      withinTempDir $ do
        let subject = "example"
        createFile subject shScript
        result <- isShellScript subject
        result `shouldBe` True

    it "should not be valid if file has no extension and an invalid shebang" $ do
      withinTempDir $ do
        let subject = "example"
        createFile subject rbScript
        result <- isShellScript subject
        result `shouldBe` False

  describe "hasShellExtension" $ do
    it "should be valid if file has .sh extension" $ do
      let subject = "example.sh"
      let result = hasShellExtension subject
      result `shouldBe` True

    it "should not be valid if file has no extension" $ do
      let subject = "example"
      let result = hasShellExtension subject
      result `shouldBe` False

  describe "hasValidInterpretter" $ do
    it "should be valid if sh is interpretter" $ do
      let subject = Shebang (Interpretter "/bin/sh") Nothing
      let result = hasValidInterpretter subject
      result `shouldBe` True

      let subject' = Shebang (Interpretter "/bin/env") (Just (Argument "sh"))
      let result' = hasValidInterpretter subject'
      result' `shouldBe` True

    it "should be valid if bash is interpretter" $ do
      let subject = Shebang (Interpretter "/bin/bash") Nothing
      let result = hasValidInterpretter subject
      result `shouldBe` True

      let subject' = Shebang (Interpretter "/bin/env") (Just (Argument "bash"))
      let result' = hasValidInterpretter subject'
      result' `shouldBe` True

    it "should be not be valid if ruby is interpretter" $ do
      let subject = Shebang (Interpretter "/bin/ruby") Nothing
      let result = hasValidInterpretter subject
      result `shouldBe` False

      let subject' = Shebang (Interpretter "/bin/env") (Just (Argument "ruby"))
      let result' = hasValidInterpretter subject'
      result' `shouldBe` False

  describe "findShellScripts" $ do
    describe "when i specify a list of files" $ do
      it "should filter out scripts that aren't shell scripts" $ do
        let subject = [ "test/fixtures/example"
                      , "test/fixtures/example.sh"
                      , "test/fixtures/example.rb"
                      ]
        result <- findShellScripts subject
        result `shouldBe` [ "test/fixtures/example", "test/fixtures/example.sh" ]

    describe "when i specify a list of directories" $ do
      it "should should only give me files that end with .sh" $ do
        let subject = [ "test/fixtures/" ]
        result <- findShellScripts subject
        result `shouldBe` [ "test/fixtures/example.sh" ]

--------------------------------------------------------------------------------

withinTempDir :: IO a -> IO a
withinTempDir act = withSystemTempDirectory "cc-hlint" $ \tmp -> do
    E.bracket getCurrentDirectory setCurrentDirectory $ \_ ->
        setCurrentDirectory tmp >> act

createFile :: FilePath -> BSL.ByteString -> IO ()
createFile path content = do
    createDirectoryIfMissing True $ takeDirectory path
    BSL.writeFile path content
