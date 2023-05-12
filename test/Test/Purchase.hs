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

module Test.Purchase
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Control.Monad (void)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash,alwaysSucceedPolicy)
import Ledger.TimeSlot
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)

import Test.Common
import CardanoSecondaryMarket

-------------------------------------------------
-- Purchase Scenarios
-------------------------------------------------
successfullyPurchaseSingleNFT :: EmulatorTrace ()
successfullyPurchaseSingleNFT = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let pAddr = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                          $ mockWalletPaymentPubKeyHash 
                                          $ knownWallet 1)
                        Nothing
      saleDatum = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken1
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr
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

  void $ waitUntilSlot 2
  
  saleHash <- txIdWithValue ( lovelaceValueOf 3_000_000 
                           <> singleton marketBeaconPolicySym1 "Sale" 1
                           <> (uncurry singleton testToken1) 1
                            )

  let receiptToken = txIdAsToken saleHash

  callEndpoint @"purchase" h2 $
    PurchaseParams
      { purchaseBeaconsMinted = [[("Sale",-1),(receiptToken,1)]]
      , purchaseBeaconRedeemer = [MintReceiptTokens]
      , purchaseBeaconPolicies = [marketBeaconPolicy1]
      , purchaseVal = marketValidator
      , purchaseAddresses = [marketAddr]
      , purchaseSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , purchasePaymentAddresses = [pAddr]
      , purchasePaymentOutputs = 
          [ [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken 1
              )
            ]
          ]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Purchase NFT(s)"
    [ 
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig successfullyPurchaseSingleNFT