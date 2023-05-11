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
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

module Test.Common where

import qualified Data.Map as Map
import Control.Lens hiding (from,index,to)
import Data.Default
import Data.Void (Void)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Ledger.Value (singleton)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl',repeat)
import Prelude as Haskell (Semigroup (..), String)
import Cardano.Api.Shelley (ExecutionUnits (..),ProtocolParameters (..))
import Ledger.Tx.Internal as I
import Plutus.Script.Utils.V2.Generators (alwaysSucceedPolicy)

import CardanoSecondaryMarket

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
txIdWithValue :: Value -> EmulatorTrace TxId
txIdWithValue value' = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      findTxId v ((TxOutRef txId' _,o):ys)
        | I.txOutValue o == v = txId'
        | otherwise = findTxId v ys
  return $ findTxId value' xs

toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum Nothing val

instance ToJSON MarketDatum
instance FromJSON MarketDatum

instance ToJSON MarketRedeemer
instance FromJSON MarketRedeemer

instance ToJSON MarketBeaconRedeemer
instance FromJSON MarketBeaconRedeemer

-------------------------------------------------
-- Params
-------------------------------------------------
data CreateSaleParams = CreateSaleParams
  { createSaleBeaconsMinted :: [(TokenName,Integer)]
  , createSaleBeaconRedeemer :: MarketBeaconRedeemer
  , createSaleBeaconPolicy :: MintingPolicy
  , createSaleAddress :: Address
  , createSaleInfo :: [(Maybe MarketDatum, Value)]
  , createSaleAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON)

data PurchaseParams = PurchaseParams
  { purchaseBeaconsMinted :: [[(TokenName,Integer)]]
  , purchaseBeaconRedeemer :: [MarketBeaconRedeemer]
  , purchaseBeaconPolicies :: [MintingPolicy]
  , purchaseVal :: Validator
  , purchaseAddresses :: [Address]
  , purchaseSpecificUTxOs :: [[(MarketDatum,Value)]]
  , purchasePaymentAddresses :: [Address]
  , purchasePaymentOutputs :: [[(Maybe MarketDatum,Value)]]
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "create-sale" CreateSaleParams
  .\/ Endpoint "purchase" PurchaseParams

-------------------------------------------------
-- Configs
-------------------------------------------------
testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken2")

testToken3 :: (CurrencySymbol,TokenName)
testToken3 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken3")

testToken4 :: (CurrencySymbol,TokenName)
testToken4 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken4")

marketBeaconPolicyADA :: MintingPolicy
marketBeaconPolicyADA = marketBeaconPolicy adaSymbol

marketBeaconPolicySymADA :: CurrencySymbol
marketBeaconPolicySymADA = UScripts.scriptCurrencySymbol marketBeaconPolicyADA

marketBeaconPolicy1 :: MintingPolicy
marketBeaconPolicy1 = marketBeaconPolicy "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d"

marketBeaconPolicySym1 :: CurrencySymbol
marketBeaconPolicySym1 = UScripts.scriptCurrencySymbol marketBeaconPolicy1

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: Value
    user1 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1
         <> (uncurry singleton testToken2) 1000

    user2 :: Value
    user2 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1
    
    user3 :: Value
    user3 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken4) 1
    
    user4 :: Value
    user4 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
    
    user5 :: Value
    user5 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000

    user6 :: Value
    user6 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
  
    wallets :: [(Wallet,Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      , (knownWallet 5, user5)
      , (knownWallet 6, user6)
      ]

-------------------------------------------------
-- Trace Models
-------------------------------------------------
createSaleUTxO :: CreateSaleParams -> Contract () TraceSchema Text ()
createSaleUTxO CreateSaleParams{..} = do
  let beaconPolicyHash = mintingPolicyHash createSaleBeaconPolicy
      beaconRedeemer = toRedeemer createSaleBeaconRedeemer
      
      toDatum'
        | createSaleAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      lookups = plutusV2MintingPolicy createSaleBeaconPolicy
      
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          createSaleBeaconsMinted
        )
        -- | Add createSale
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith createSaleAddress (fmap toDatum' d) v)
              mempty
              createSaleInfo
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Sale UTxO created"

purchase :: PurchaseParams -> Contract () TraceSchema Text ()
purchase PurchaseParams{..} = do
  saleUTxOs <- Map.unions <$> mapM utxosAt purchaseAddresses

  let beaconPolicyHashes = map mintingPolicyHash purchaseBeaconPolicies
      beaconRedeemer = toRedeemer purchaseBeaconRedeemer

      toDatum' = TxOutDatumInline . toDatum

      purchaseRedeemer = toRedeemer Purchase

      lPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy b) 
                         (plutusV2MintingPolicy alwaysSucceedPolicy)
                         purchaseBeaconPolicies

      lookups = Constraints.unspentOutputs saleUTxOs
             <> plutusV2OtherScript purchaseVal
             <> lPolicies

      tx' =
        -- | Mint/Burn Beacons
        (mconcat $ zipWith (\z b -> foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer z beaconRedeemer t i) 
          mempty
          b)
          beaconPolicyHashes
          purchaseBeaconsMinted
        )
        -- | Must spend all utxos to be accepted
        <> ( mconcat $ map (foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash purchaseVal) 
                        (== toDatum d)
                        (==v) 
                        purchaseRedeemer
                  ) 
                  mempty)
                  purchaseSpecificUTxOs
           )
        -- | Make the payments
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toDatum' d) v)
              mempty
              b)
              purchasePaymentAddresses
              purchasePaymentOutputs
           )
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Purchased NFT(s)"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createSaleUTxO' = endpoint @"create-sale" createSaleUTxO
    purchase' = endpoint @"purchase" purchase
    choices = 
      [ createSaleUTxO'
      , purchase'
      ]