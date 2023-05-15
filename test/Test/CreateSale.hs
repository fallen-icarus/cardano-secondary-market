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
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Data.Default
import Ledger.Address
import Plutus.V2.Ledger.Api
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

mintMultipleSaleBeacons :: EmulatorTrace ()
mintMultipleSaleBeacons = do
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
      { createSaleBeaconsMinted = [("Sale",2)]
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

saleBeaconHasWrongTokenName :: EmulatorTrace ()
saleBeaconHasWrongTokenName = do
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
      { createSaleBeaconsMinted = [("Sae",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr
      , createSaleInfo = 
          [ ( Just saleDatum
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sae" 1
           <> (uncurry singleton testToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

mintOtherTokens :: EmulatorTrace ()
mintOtherTokens = do
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
      { createSaleBeaconsMinted = [("Sale",1),("other",1)]
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

beaconGoesToNonDappAddress :: EmulatorTrace ()
beaconGoesToNonDappAddress = do
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
          []
      , createSaleAsInline = True
      }

beaconGoesToNonStakingDappAddress :: EmulatorTrace ()
beaconGoesToNonStakingDappAddress = do
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
                           Nothing
  
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

datumHasWrongBeaconSym :: EmulatorTrace ()
datumHasWrongBeaconSym = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let address = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                          $ mockWalletPaymentPubKeyHash 
                                          $ knownWallet 1)
                        Nothing
      saleDatum = MarketDatum
        { beaconSymbol = adaSymbol
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

datumHasWrongNftSymbol :: EmulatorTrace ()
datumHasWrongNftSymbol = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let address = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                          $ mockWalletPaymentPubKeyHash 
                                          $ knownWallet 1)
                        Nothing
      saleDatum = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = otherToken1
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
           <> (uncurry singleton otherToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

datumHasNegativePrice :: EmulatorTrace ()
datumHasNegativePrice = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let address = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                          $ mockWalletPaymentPubKeyHash 
                                          $ knownWallet 1)
                        Nothing
      saleDatum = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken1
        , salePrice = ((adaSymbol,adaToken),-10_000_000)
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

datumAddressIsPaymentScript :: EmulatorTrace ()
datumAddressIsPaymentScript = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let address = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                          $ mockWalletPaymentPubKeyHash 
                                          $ knownWallet 1)
                        Nothing
      saleDatum = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken1
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
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

saleBeaconStoredWithoutMinAda :: EmulatorTrace ()
saleBeaconStoredWithoutMinAda = do
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
            , lovelaceValueOf 2_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

saleBeaconStoredWithoutNFT :: EmulatorTrace ()
saleBeaconStoredWithoutNFT = do
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
           )
          ]
      , createSaleAsInline = True
      }

saleBeaconStoredWithOtherAssets :: EmulatorTrace ()
saleBeaconStoredWithOtherAssets = do
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
           <> (uncurry singleton testToken2) 1
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
    , checkPredicateOptions opts "Fail if multiple Sale beacons minted in tx"
        (Test.not assertNoFailedTransactions) mintMultipleSaleBeacons
    , checkPredicateOptions opts "Fail if beacon has wrong token name"
        (Test.not assertNoFailedTransactions) saleBeaconHasWrongTokenName
    , checkPredicateOptions opts "Fail if any additional tokens minted in tx"
        (Test.not assertNoFailedTransactions) mintOtherTokens
    , checkPredicateOptions opts "Fail if beacon goes to a non dapp address"
        (Test.not assertNoFailedTransactions) beaconGoesToNonDappAddress
    , checkPredicateOptions opts "Fail if beacon goes to non staking dapp address"
        (Test.not assertNoFailedTransactions) beaconGoesToNonStakingDappAddress
    , checkPredicateOptions opts "Fail if datum has wrong beaconSymbol"
        (Test.not assertNoFailedTransactions) datumHasWrongBeaconSym
    , checkPredicateOptions opts "Fail if datum has different NFT than what policy is for"
        (Test.not assertNoFailedTransactions) datumHasWrongNftSymbol
    , checkPredicateOptions opts "Fail if datum has negative price"
        (Test.not assertNoFailedTransactions) datumHasNegativePrice
    , checkPredicateOptions opts "Fail if datum uses a payment script address"
        (Test.not assertNoFailedTransactions) datumAddressIsPaymentScript
    , checkPredicateOptions opts "Fail if beacon stored without min ada"
        (Test.not assertNoFailedTransactions) saleBeaconStoredWithoutMinAda
    , checkPredicateOptions opts "Fail if beacon stored without NFT"
        (Test.not assertNoFailedTransactions) saleBeaconStoredWithoutNFT
    , checkPredicateOptions opts "Fail if beacon stored with additional assets"
        (Test.not assertNoFailedTransactions) saleBeaconStoredWithOtherAssets
    
    , checkPredicateOptions opts "Successfully create Sale UTxO"
        assertNoFailedTransactions successfullyCreateSaleUTxO
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig datumHasWrongNftSymbol