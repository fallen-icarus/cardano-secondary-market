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
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash,alwaysSucceedPolicy)
import Cardano.Node.Emulator.TimeSlot
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

successfullyPurchaseMultipleNFTsUnderSamePolicy :: EmulatorTrace ()
successfullyPurchaseMultipleNFTsUnderSamePolicy = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints
  h6 <- activateContractWallet (knownWallet 6) endpoints

  let pAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 1)
                        Nothing
      pAddr2 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 2)
                        Nothing
      pAddr3 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 3)
                        Nothing
      pAddr4 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 4)
                        Nothing
      saleDatum1 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken1
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr1
        }
      saleDatum2 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken3
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr2
        }
      saleDatum3 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken4
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr3
        }
      saleDatum4 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken5
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr4
        }
      marketAddr1 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 1)
      marketAddr2 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 2)
      marketAddr3 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 3)
      marketAddr4 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 4)
    
  callEndpoint @"create-sale" h1 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr1
      , createSaleInfo = 
          [ ( Just saleDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-sale" h2 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr2
      , createSaleInfo = 
          [ ( Just saleDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken3) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"create-sale" h3 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr3
      , createSaleInfo = 
          [ ( Just saleDatum3
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken4) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"create-sale" h4 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr4
      , createSaleInfo = 
          [ ( Just saleDatum4
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken5) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 8
  
  saleHash1 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken1) 1
                             )
  saleHash2 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken3) 1
                             )
  saleHash3 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken4) 1
                             )
  saleHash4 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken5) 1
                             )

  let receiptToken1 = txIdAsToken saleHash1
      receiptToken2 = txIdAsToken saleHash2
      receiptToken3 = txIdAsToken saleHash3
      receiptToken4 = txIdAsToken saleHash4

  callEndpoint @"purchase" h6 $
    PurchaseParams
      { purchaseBeaconsMinted = 
          [ [ ("Sale",-4)
            , (receiptToken1,1)
            , (receiptToken2,1)
            , (receiptToken3,1)
            , (receiptToken4,1)
            ]
          ]
      , purchaseBeaconRedeemer = [MintReceiptTokens]
      , purchaseBeaconPolicies = [marketBeaconPolicy1]
      , purchaseVal = marketValidator
      , purchaseAddresses = [marketAddr1,marketAddr2,marketAddr3,marketAddr4]
      , purchaseSpecificUTxOs = 
          [ [ ( saleDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          , [ ( saleDatum2
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken3) 1
              )
            ]
          , [ ( saleDatum3
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken4) 1
              )
            ]
          , [ ( saleDatum4
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken5) 1
              )
            ]
          ]
      , purchasePaymentAddresses = [pAddr1,pAddr2,pAddr3,pAddr4]
      , purchasePaymentOutputs = 
          [ [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken1 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken2 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken3 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken4 1
              )
            ]
          ]
      }

receiptTokenNotMinted :: EmulatorTrace ()
receiptTokenNotMinted = do
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
      { purchaseBeaconsMinted = [[("Sale",-1)]]
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
              )
            ]
          ]
      }

saleBeaconMissing :: EmulatorTrace ()
saleBeaconMissing = do
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
  
  saleHash <- txIdWithValue ( lovelaceValueOf 3_000_000 
                           <> (uncurry singleton testToken1) 1
                            )

  let receiptToken = txIdAsToken saleHash

  callEndpoint @"purchase" h2 $
    PurchaseParams
      { purchaseBeaconsMinted = [[(receiptToken,1)]]
      , purchaseBeaconRedeemer = [MintReceiptTokens]
      , purchaseBeaconPolicies = [marketBeaconPolicy1]
      , purchaseVal = marketValidator
      , purchaseAddresses = [marketAddr]
      , purchaseSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
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

atLeastOneSaleBeaconMissing :: EmulatorTrace ()
atLeastOneSaleBeaconMissing = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints
  h6 <- activateContractWallet (knownWallet 6) endpoints

  let pAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 1)
                        Nothing
      pAddr2 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 2)
                        Nothing
      pAddr3 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 3)
                        Nothing
      pAddr4 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 4)
                        Nothing
      saleDatum1 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken1
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr1
        }
      saleDatum2 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken3
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr2
        }
      saleDatum3 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken4
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr3
        }
      saleDatum4 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken5
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr4
        }
      marketAddr1 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 1)
      marketAddr2 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 2)
      marketAddr3 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 3)
      marketAddr4 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 4)
    
  callEndpoint @"create-sale" h1 $
    CreateSaleParams
      { createSaleBeaconsMinted = []
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr1
      , createSaleInfo = 
          [ ( Just saleDatum1
            , lovelaceValueOf 3_000_000 
           <> (uncurry singleton testToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-sale" h2 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr2
      , createSaleInfo = 
          [ ( Just saleDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken3) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"create-sale" h3 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr3
      , createSaleInfo = 
          [ ( Just saleDatum3
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken4) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"create-sale" h4 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr4
      , createSaleInfo = 
          [ ( Just saleDatum4
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken5) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 8
  
  saleHash1 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> (uncurry singleton testToken1) 1
                             )
  saleHash2 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken3) 1
                             )
  saleHash3 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken4) 1
                             )
  saleHash4 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken5) 1
                             )

  let receiptToken1 = txIdAsToken saleHash1
      receiptToken2 = txIdAsToken saleHash2
      receiptToken3 = txIdAsToken saleHash3
      receiptToken4 = txIdAsToken saleHash4

  callEndpoint @"purchase" h6 $
    PurchaseParams
      { purchaseBeaconsMinted = 
          [ [ ("Sale",-3)
            , (receiptToken1,1)
            , (receiptToken2,1)
            , (receiptToken3,1)
            , (receiptToken4,1)
            ]
          ]
      , purchaseBeaconRedeemer = [MintReceiptTokens]
      , purchaseBeaconPolicies = [marketBeaconPolicy1]
      , purchaseVal = marketValidator
      , purchaseAddresses = [marketAddr1,marketAddr2,marketAddr3,marketAddr4]
      , purchaseSpecificUTxOs = 
          [ [ ( saleDatum1
              , lovelaceValueOf 3_000_000 
             <> (uncurry singleton testToken1) 1
              )
            ]
          , [ ( saleDatum2
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken3) 1
              )
            ]
          , [ ( saleDatum3
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken4) 1
              )
            ]
          , [ ( saleDatum4
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken5) 1
              )
            ]
          ]
      , purchasePaymentAddresses = [pAddr1,pAddr2,pAddr3,pAddr4]
      , purchasePaymentOutputs = 
          [ [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken1 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken2 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken3 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken4 1
              )
            ]
          ]
      }

missingPaymentOutput :: EmulatorTrace ()
missingPaymentOutput = do
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
          []
      }

wrongPaymentAmount :: EmulatorTrace ()
wrongPaymentAmount = do
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
              , lovelaceValueOf 12_000_000
             <> singleton marketBeaconPolicySym1 receiptToken 1
              )
            ]
          ]
      }

paymentMissingReceiptToken :: EmulatorTrace ()
paymentMissingReceiptToken = do
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
              )
            ]
          ]
      }

paymentsMixedUp :: EmulatorTrace ()
paymentsMixedUp = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints
  h6 <- activateContractWallet (knownWallet 6) endpoints

  let pAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 1)
                        Nothing
      pAddr2 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 2)
                        Nothing
      pAddr3 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 3)
                        Nothing
      pAddr4 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 4)
                        Nothing
      saleDatum1 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken1
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr1
        }
      saleDatum2 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken3
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr2
        }
      saleDatum3 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken4
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr3
        }
      saleDatum4 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken5
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr4
        }
      marketAddr1 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 1)
      marketAddr2 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 2)
      marketAddr3 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 3)
      marketAddr4 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 4)
    
  callEndpoint @"create-sale" h1 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr1
      , createSaleInfo = 
          [ ( Just saleDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-sale" h2 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr2
      , createSaleInfo = 
          [ ( Just saleDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken3) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"create-sale" h3 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr3
      , createSaleInfo = 
          [ ( Just saleDatum3
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken4) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"create-sale" h4 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr4
      , createSaleInfo = 
          [ ( Just saleDatum4
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken5) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 8
  
  saleHash1 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken1) 1
                             )
  saleHash2 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken3) 1
                             )
  saleHash3 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken4) 1
                             )
  saleHash4 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken5) 1
                             )

  let receiptToken1 = txIdAsToken saleHash1
      receiptToken2 = txIdAsToken saleHash2
      receiptToken3 = txIdAsToken saleHash3
      receiptToken4 = txIdAsToken saleHash4

  callEndpoint @"purchase" h6 $
    PurchaseParams
      { purchaseBeaconsMinted = 
          [ [ ("Sale",-4)
            , (receiptToken1,1)
            , (receiptToken2,1)
            , (receiptToken3,1)
            , (receiptToken4,1)
            ]
          ]
      , purchaseBeaconRedeemer = [MintReceiptTokens]
      , purchaseBeaconPolicies = [marketBeaconPolicy1]
      , purchaseVal = marketValidator
      , purchaseAddresses = [marketAddr1,marketAddr2,marketAddr3,marketAddr4]
      , purchaseSpecificUTxOs = 
          [ [ ( saleDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          , [ ( saleDatum2
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken3) 1
              )
            ]
          , [ ( saleDatum3
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken4) 1
              )
            ]
          , [ ( saleDatum4
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken5) 1
              )
            ]
          ]
      , purchasePaymentAddresses = [pAddr1,pAddr2,pAddr3,pAddr4]
      , purchasePaymentOutputs = 
          [ [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken1 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken3 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken2 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken4 1
              )
            ]
          ]
      }

receiptTokenDoesntMatchInput :: EmulatorTrace ()
receiptTokenDoesntMatchInput = do
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

  let receiptToken = "Not correct"

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

notAllSaleBeaconsBurned :: EmulatorTrace ()
notAllSaleBeaconsBurned = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints
  h6 <- activateContractWallet (knownWallet 6) endpoints

  let pAddr1 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 1)
                        Nothing
      pAddr2 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 2)
                        Nothing
      pAddr3 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 3)
                        Nothing
      pAddr4 = Address (PubKeyCredential $ unPaymentPubKeyHash 
                                         $ mockWalletPaymentPubKeyHash 
                                         $ knownWallet 4)
                        Nothing
      saleDatum1 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken1
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr1
        }
      saleDatum2 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken3
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr2
        }
      saleDatum3 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken4
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr3
        }
      saleDatum4 = MarketDatum
        { beaconSymbol = marketBeaconPolicySym1
        , nftOnSale = testToken5
        , salePrice = ((adaSymbol,adaToken),10_000_000)
        , payToAddress = pAddr4
        }
      marketAddr1 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 1)
      marketAddr2 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 2)
      marketAddr3 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 3)
      marketAddr4 = Address (ScriptCredential marketValidatorHash)
                            (Just $ StakingHash
                                  $ PubKeyCredential
                                  $ unPaymentPubKeyHash
                                  $ mockWalletPaymentPubKeyHash
                                  $ knownWallet 4)
    
  callEndpoint @"create-sale" h1 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr1
      , createSaleInfo = 
          [ ( Just saleDatum1
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-sale" h2 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr2
      , createSaleInfo = 
          [ ( Just saleDatum2
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken3) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"create-sale" h3 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr3
      , createSaleInfo = 
          [ ( Just saleDatum3
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken4) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"create-sale" h4 $
    CreateSaleParams
      { createSaleBeaconsMinted = [("Sale",1)]
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr4
      , createSaleInfo = 
          [ ( Just saleDatum4
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken5) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 8
  
  saleHash1 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken1) 1
                             )
  saleHash2 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken3) 1
                             )
  saleHash3 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken4) 1
                             )
  saleHash4 <- txIdWithValue ( lovelaceValueOf 3_000_000 
                            <> singleton marketBeaconPolicySym1 "Sale" 1
                            <> (uncurry singleton testToken5) 1
                             )

  let receiptToken1 = txIdAsToken saleHash1
      receiptToken2 = txIdAsToken saleHash2
      receiptToken3 = txIdAsToken saleHash3
      receiptToken4 = txIdAsToken saleHash4

  callEndpoint @"purchase" h6 $
    PurchaseParams
      { purchaseBeaconsMinted = 
          [ [ ("Sale",-3)
            , (receiptToken1,1)
            , (receiptToken2,1)
            , (receiptToken3,1)
            , (receiptToken4,1)
            ]
          ]
      , purchaseBeaconRedeemer = [MintReceiptTokens]
      , purchaseBeaconPolicies = [marketBeaconPolicy1]
      , purchaseVal = marketValidator
      , purchaseAddresses = [marketAddr1,marketAddr2,marketAddr3,marketAddr4]
      , purchaseSpecificUTxOs = 
          [ [ ( saleDatum1
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          , [ ( saleDatum2
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken3) 1
              )
            ]
          , [ ( saleDatum3
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken4) 1
              )
            ]
          , [ ( saleDatum4
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken5) 1
              )
            ]
          ]
      , purchasePaymentAddresses = [pAddr1,pAddr2,pAddr3,pAddr4]
      , purchasePaymentOutputs = 
          [ [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken1 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken2 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken3 1
              )
            ]
          , [ ( Nothing
              , lovelaceValueOf 13_000_000
             <> singleton marketBeaconPolicySym1 receiptToken4 1
              )
            ]
          ]
      }

mintOtherTokens :: EmulatorTrace ()
mintOtherTokens = do
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
      { purchaseBeaconsMinted = [[("Sale",-1),(receiptToken,1),("other",1)]]
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
    [ checkPredicateOptions opts "Fail if receipt token not minted"
        (Test.not assertNoFailedTransactions) receiptTokenNotMinted
    , checkPredicateOptions opts "Fail if there are no Sale beacons in tx inputs"
        (Test.not assertNoFailedTransactions) saleBeaconMissing
    , checkPredicateOptions opts "Fail if at least one marketplace input is missing a sale beacon"
        (Test.not assertNoFailedTransactions) atLeastOneSaleBeaconMissing
    , checkPredicateOptions opts "Fail if tx is missing the only required payment output"
        (Test.not assertNoFailedTransactions) missingPaymentOutput
    , checkPredicateOptions opts "Fail if payment has wrong amount"
        (Test.not assertNoFailedTransactions) wrongPaymentAmount
    , checkPredicateOptions opts "Fail if payment output is missing receipt token"
        (Test.not assertNoFailedTransactions) paymentMissingReceiptToken
    , checkPredicateOptions opts "Fail if payment outputs mixed up"
        (Test.not assertNoFailedTransactions) paymentsMixedUp
    , checkPredicateOptions opts "Fail if minted token doesn't match tx hash of Sale input"
        (Test.not assertNoFailedTransactions) receiptTokenDoesntMatchInput
    , checkPredicateOptions opts "Fail if not all Sale beacons burned"
        (Test.not assertNoFailedTransactions) notAllSaleBeaconsBurned
    , checkPredicateOptions opts "Fail if other tokens minted by policy"
        (Test.not assertNoFailedTransactions) mintOtherTokens

    , checkPredicateOptions opts "Successfully purchase single NFT"
        assertNoFailedTransactions successfullyPurchaseSingleNFT
    , checkPredicateOptions opts "Successfully purchase multiple NFTs under same policy"
        assertNoFailedTransactions successfullyPurchaseMultipleNFTsUnderSamePolicy
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def benchConfig successfullyPurchaseMultipleNFTsUnderSamePolicy