module Main where

import Test.Tasty

import Test.CreateSale as CreateSale
import Test.CloseSale as CloseSale
import Test.Purchase as Purchase
import Test.UpdateSale as UpdateSale

main :: IO ()
main = defaultMain $ testGroup "Cardano-Secondary-Market"
  [
    CreateSale.tests
  , CloseSale.tests
  , Purchase.tests
  , UpdateSale.tests
  ]