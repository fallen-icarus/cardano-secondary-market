module Main where

import Test.Tasty

import Test.CreateSale as CreateSale
import Test.Purchase as Purchase

main :: IO ()
main = defaultMain $ testGroup "Cardano-Secondary-Market"
  [
    CreateSale.tests
  , Purchase.tests
  ]