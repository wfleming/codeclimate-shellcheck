{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Shebang
import Data.String.QQ
import Test.Tasty
import Test.Tasty.Hspec

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

    it "should not parse an invalid shebang" $ do
      let subject = [s|/bin/sh
                       echo "hello world"|]
      let result = decode subject
      result `shouldBe` Nothing

  describe "hasShebang" $ do
    it "should be able to detect a valid shebang" $ do
      let subject = "#!/bin/sh"
      let result = hasShebang subject
      result `shouldBe` True

    it "should be able to detect an invalid shebang" $ do
      let subject = ""
      let result = hasShebang subject
      result `shouldBe` False
