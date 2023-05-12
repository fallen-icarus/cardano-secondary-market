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

module Test.CloseSale
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
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)
import Ledger.TimeSlot

import Test.Common
import CardanoSecondaryMarket

-------------------------------------------------
-- Close Sale Scenarios
-------------------------------------------------
successfullyCloseSingleSaleUTxO :: EmulatorTrace ()
successfullyCloseSingleSaleUTxO = do
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

  void $ waitUntilSlot 2

  callEndpoint @"close-sale" h1 $
    CloseSaleParams
      { closeSaleBeaconsBurned = [[("Sale",-1)]]
      , closeSaleBeaconRedeemer = [BurnBeacons]
      , closeSaleBeaconPolicies = [marketBeaconPolicy1]
      , closeSaleMarketVal = marketValidator
      , closeSaleMarketAddress = marketAddr
      , closeSaleSpecificUTxOs = 
          [ ( saleDatum
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
            )
          ]
      }

beaconNotBurned :: EmulatorTrace ()
beaconNotBurned = do
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

  void $ waitUntilSlot 2

  callEndpoint @"close-sale" h1 $
    CloseSaleParams
      { closeSaleBeaconsBurned = []
      , closeSaleBeaconRedeemer = [BurnBeacons]
      , closeSaleBeaconPolicies = [marketBeaconPolicy1]
      , closeSaleMarketVal = marketValidator
      , closeSaleMarketAddress = marketAddr
      , closeSaleSpecificUTxOs = 
          [ ( saleDatum
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
            )
          ]
      }

stakingCredDidNotApprove :: EmulatorTrace ()
stakingCredDidNotApprove = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

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

  void $ waitUntilSlot 2

  callEndpoint @"close-sale" h2 $
    CloseSaleParams
      { closeSaleBeaconsBurned = [[("Sale",-1)]]
      , closeSaleBeaconRedeemer = [BurnBeacons]
      , closeSaleBeaconPolicies = [marketBeaconPolicy1]
      , closeSaleMarketVal = marketValidator
      , closeSaleMarketAddress = marketAddr
      , closeSaleSpecificUTxOs = 
          [ ( saleDatum
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
            )
          ]
      }

successfullyCloseInvalidSale :: EmulatorTrace ()
successfullyCloseInvalidSale = do
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
      { createSaleBeaconsMinted = []
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr
      , createSaleInfo = 
          [ ( Just saleDatum
            , lovelaceValueOf 3_000_000 
           <> (uncurry singleton testToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-sale" h1 $
    CloseSaleParams
      { closeSaleBeaconsBurned = []
      , closeSaleBeaconRedeemer = [BurnBeacons]
      , closeSaleBeaconPolicies = [marketBeaconPolicy1]
      , closeSaleMarketVal = marketValidator
      , closeSaleMarketAddress = marketAddr
      , closeSaleSpecificUTxOs = 
          [ ( saleDatum
            , lovelaceValueOf 3_000_000 
           <> (uncurry singleton testToken1) 1
            )
          ]
      }

successfullyCloseMultipleSaleUTxOs :: EmulatorTrace ()
successfullyCloseMultipleSaleUTxOs = do
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

  void $ waitUntilSlot 4

  callEndpoint @"create-sale" h1 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr
      , createSaleInfo = 
          [ ( Just saleDatum{nftOnSale = testToken2}
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken2) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"close-sale" h1 $
    CloseSaleParams
      { closeSaleBeaconsBurned = [[("Sale",-2)]]
      , closeSaleBeaconRedeemer = [BurnBeacons]
      , closeSaleBeaconPolicies = [marketBeaconPolicy1]
      , closeSaleMarketVal = marketValidator
      , closeSaleMarketAddress = marketAddr
      , closeSaleSpecificUTxOs = 
          [ ( saleDatum
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
            )
          , ( saleDatum{nftOnSale = testToken2}
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken2) 1
           )
          ]
      }

mintWithBurnRedeemer :: EmulatorTrace ()
mintWithBurnRedeemer = do
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

  void $ waitUntilSlot 2

  callEndpoint @"close-sale" h1 $
    CloseSaleParams
      { closeSaleBeaconsBurned = [[("Sale",-1),("other",1)]]
      , closeSaleBeaconRedeemer = [BurnBeacons]
      , closeSaleBeaconPolicies = [marketBeaconPolicy1]
      , closeSaleMarketVal = marketValidator
      , closeSaleMarketAddress = marketAddr
      , closeSaleSpecificUTxOs = 
          [ ( saleDatum
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
            )
          ]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close Sale UTxO(s)"
    [ checkPredicateOptions opts "Fail if Sale beacon not burned"
        (Test.not assertNoFailedTransactions) beaconNotBurned
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakingCredDidNotApprove

    , checkPredicateOptions opts "Fail if burn redeemer used to mint any tokens"
        (Test.not assertNoFailedTransactions) mintWithBurnRedeemer
    
    , checkPredicateOptions opts "Successfully close single sale UTxO"
        assertNoFailedTransactions successfullyCloseSingleSaleUTxO
    , checkPredicateOptions opts "Successfully close invalid sale UTxO"
        assertNoFailedTransactions successfullyCloseInvalidSale
    , checkPredicateOptions opts "Successfully close multiple sale UTxOs"
        assertNoFailedTransactions successfullyCloseMultipleSaleUTxOs
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig mintWithBurnRedeemer