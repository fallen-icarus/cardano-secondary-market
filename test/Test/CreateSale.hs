{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Test.CreateSale
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)
import Test.Tasty.HUnit

import Test.Common
import CardanoSecondaryMarket

-------------------------------------------------
-- Beacon Characteristics
-------------------------------------------------
-- | This will only be true if there is a unique beacon policy for every policy id.
uniqueBeaconsForEachConfig :: Assertion
uniqueBeaconsForEachConfig = assertBool "Beacons are not unique for each config" $
  marketBeaconPolicySym1 /= marketBeaconPolicySymADA

-------------------------------------------------
-- Create Sale UTxO Scenarios
-------------------------------------------------
successfullyCreateSaleUTxO :: EmulatorTrace ()
successfullyCreateSaleUTxO = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let address = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                          $ mockWalletPaymentPubKeyHash 
                                          $ knownWallet 1)
                        Nothing
      saleDatum = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken1
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = address
        }
      marketAddr = Address (ScriptCredential marketValidatorHash)
                           (Just $ StakingHash
                                 $ PubKeyCredential
                                 $ unPaymentPubKeyHash
                                 $ mockWalletPaymentPubKeyHash
                                 $ knownWallet 1)
  
  callEndpoint @"create-sale" h1 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr
      , createSaleInfo = 
          [ ( Just saleDatum
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create A Sale UTxO"
    [ testCase "Unique beacon for every policy id" uniqueBeaconsForEachConfig

    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig successfullyCreateSaleUTxO