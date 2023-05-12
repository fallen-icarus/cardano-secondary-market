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

module Test.UpdateSale
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
-- Update Sale Scenarios
-------------------------------------------------
successfullyUpdateSaleUTxO :: EmulatorTrace ()
successfullyUpdateSaleUTxO = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
              , lovelaceValueOf 3_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      }

multipleSaleBeaconsInTx :: EmulatorTrace ()
multipleSaleBeaconsInTx = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
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
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
            )
          , ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000),nftOnSale = testToken2}
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken2) 1
           )
            ]
          ]
      }

saleBeaconMissingFromTx :: EmulatorTrace ()
saleBeaconMissingFromTx = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      }

saleBeaconMissingFromInput :: EmulatorTrace ()
saleBeaconMissingFromInput = do
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
      { createSaleBeaconsMinted = []
      , createSaleBeaconRedeemer = MintSaleBeacon
      , createSaleBeaconPolicy = marketBeaconPolicy1
      , createSaleAddress = marketAddr
      , createSaleInfo = 
          [ ( Just saleDatum{nftOnSale = testToken2}
            , lovelaceValueOf 3_000_000 
           <> (uncurry singleton testToken2) 1
           )
          ]
      , createSaleAsInline = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
            )
          , ( saleDatum{nftOnSale = testToken2}
            , lovelaceValueOf 3_000_000 
           <> (uncurry singleton testToken2) 1
           )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken1) 1
            )
          , ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000),nftOnSale = testToken2}
            , lovelaceValueOf 3_000_000 
           <> (uncurry singleton testToken2) 1
           )
            ]
          ]
      }

saleBeaconMinted :: EmulatorTrace ()
saleBeaconMinted = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = [[("Sale",1)]]
      , updateBeaconRedeemer = [MintSaleBeacon]
      , updateBeaconPolicies = [marketBeaconPolicy1]
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
              , lovelaceValueOf 3_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ,  ( Just saleDatum{nftOnSale = testToken2}
            , lovelaceValueOf 3_000_000 
           <> singleton marketBeaconPolicySym1 "Sale" 1
           <> (uncurry singleton testToken2) 1
           )
            ]
          ]
      }

saleBeaconBurned :: EmulatorTrace ()
saleBeaconBurned = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = [[("Sale",-1)]]
      , updateBeaconRedeemer = [BurnBeacons]
      , updateBeaconPolicies = [marketBeaconPolicy1]
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      }

saleBeaconWithdrawn :: EmulatorTrace ()
saleBeaconWithdrawn = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          []
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),-15_000_000)}
              , lovelaceValueOf 3_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      }

beaconSymbolChangedInDatum :: EmulatorTrace ()
beaconSymbolChangedInDatum = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{beaconSymbol = adaSymbol}
              , lovelaceValueOf 3_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      }

datumNFTChanged :: EmulatorTrace ()
datumNFTChanged = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{nftOnSale = testToken2}
              , lovelaceValueOf 3_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      }

datumChangedToScriptAddr :: EmulatorTrace ()
datumChangedToScriptAddr = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{payToAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing}
              , lovelaceValueOf 3_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      }

beaconNotStoredWithNFT :: EmulatorTrace ()
beaconNotStoredWithNFT = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
              , lovelaceValueOf 3_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
              )
            ]
          ]
      }

beaconNotStoredWithMinADA :: EmulatorTrace ()
beaconNotStoredWithMinADA = do
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

  callEndpoint @"update" h1 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
              , lovelaceValueOf 2_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
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

  callEndpoint @"update" h2 $
    UpdateParams
      { updateBeaconsMinted = []
      , updateBeaconRedeemer = []
      , updateBeaconPolicies = []
      , updateVal = marketValidator
      , updateAddresses = [marketAddr]
      , updateSpecificUTxOs = 
          [ [ ( saleDatum
              , lovelaceValueOf 3_000_000 
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
              )
            ]
          ]
      , updatePaymentAddresses = [marketAddr]
      , updatePaymentOutputs = 
          [ [ ( Just saleDatum{salePrice = ((adaSymbol,adaToken),15_000_000)}
              , lovelaceValueOf 3_000_000
             <> singleton marketBeaconPolicySym1 "Sale" 1
             <> (uncurry singleton testToken1) 1
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
  testGroup "Update Sale UTxO"
    [ checkPredicateOptions opts "Fail if multiple Sale beacons in tx"
        (Test.not assertNoFailedTransactions) multipleSaleBeaconsInTx
    , checkPredicateOptions opts "Fail if Sale beacon missing from tx"
        (Test.not assertNoFailedTransactions) saleBeaconMissingFromTx
    , checkPredicateOptions opts "Fail if Sale beacon missing from UTxO"
        (Test.not assertNoFailedTransactions) saleBeaconMissingFromInput
    , checkPredicateOptions opts "Fail if Sale beacon minted in tx"
        (Test.not assertNoFailedTransactions) saleBeaconMinted
    , checkPredicateOptions opts "Fail if Sale beacon burned in tx"
        (Test.not assertNoFailedTransactions) saleBeaconBurned
    , checkPredicateOptions opts "Fail if Sale beacon withdrawn"
        (Test.not assertNoFailedTransactions) saleBeaconWithdrawn
    , checkPredicateOptions opts "Fail if new datum has negative price"
        (Test.not assertNoFailedTransactions) datumHasNegativePrice
    , checkPredicateOptions opts "Fail if new datum has different beaconSymbol"
        (Test.not assertNoFailedTransactions) beaconSymbolChangedInDatum
    , checkPredicateOptions opts "Fail if new datum has different nftOnSale"
        (Test.not assertNoFailedTransactions) datumNFTChanged
    , checkPredicateOptions opts "Fail if new datum has a payment script address"
        (Test.not assertNoFailedTransactions) datumChangedToScriptAddr
    , checkPredicateOptions opts "Fail if new output missing NFT"
        (Test.not assertNoFailedTransactions) beaconNotStoredWithNFT
    , checkPredicateOptions opts "Fail if new output missind min ADA"
        (Test.not assertNoFailedTransactions) beaconNotStoredWithMinADA
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakingCredDidNotApprove

    , checkPredicateOptions opts "Successfully update sale UTxO"
        assertNoFailedTransactions successfullyUpdateSaleUTxO
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig stakingCredDidNotApprove