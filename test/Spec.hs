{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Solves" $ do
    it "solves" $ do
      1 `shouldBe` 1

