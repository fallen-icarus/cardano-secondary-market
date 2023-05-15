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
import Ledger hiding (singleton,mintingPolicyHash,Value,lovelaceValueOf)
import Ledger.Tx.Constraints as Constraints
import Ledger.Tx.Constraints.TxConstraints hiding (singleton)
import qualified Ledger.Tx.Constraints.TxConstraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Plutus.Script.Utils.Value (singleton,Value)
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl',zipWith3)
import Prelude as Haskell (Semigroup (..), String)
import Cardano.Api.Shelley (ExecutionUnits (..),ProtocolParameters (..))
import Ledger.Tx.Internal as I
import Plutus.Script.Utils.V2.Generators (alwaysSucceedPolicy)
import Cardano.Node.Emulator.Params
import qualified Cardano.Api as C
import Cardano.Api hiding (TxOutDatum,TxOutDatumInline,TxOutDatumHash,Address,TxId,Value)
import Ledger.Tx.CardanoAPI.Internal

import CardanoSecondaryMarket

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
unsafeFromRight :: Either a b -> b
unsafeFromRight (Right b) = b

txIdWithValue :: Value -> EmulatorTrace TxId
txIdWithValue value' = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      findTxId v ((TxOutRef txId' _,o):ys)
        | fromCardanoValue (I.txOutValue o) == v = txId'
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

data CloseSaleParams = CloseSaleParams
  { closeSaleBeaconsBurned :: [[(TokenName,Integer)]]
  , closeSaleBeaconRedeemer :: [MarketBeaconRedeemer]
  , closeSaleBeaconPolicies :: [MintingPolicy]
  , closeSaleMarketVal :: Validator
  , closeSaleMarketAddress :: Address
  , closeSaleSpecificUTxOs :: [(MarketDatum,Value)]
  } deriving (Generic,ToJSON,FromJSON)

data UpdateParams = UpdateParams
  { updateBeaconsMinted :: [[(TokenName,Integer)]]
  , updateBeaconRedeemer :: [MarketBeaconRedeemer]
  , updateBeaconPolicies :: [MintingPolicy]
  , updateVal :: Validator
  , updateAddresses :: [Address]
  , updateSpecificUTxOs :: [[(MarketDatum,Value)]]
  , updatePaymentAddresses :: [Address]
  , updatePaymentOutputs :: [[(Maybe MarketDatum,Value)]]
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "create-sale" CreateSaleParams
  .\/ Endpoint "close-sale" CloseSaleParams
  .\/ Endpoint "purchase" PurchaseParams
  .\/ Endpoint "update" UpdateParams

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

testToken5 :: (CurrencySymbol,TokenName)
testToken5 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken5")

testToken6 :: (CurrencySymbol,TokenName)
testToken6 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken6")

testToken7 :: (CurrencySymbol,TokenName)
testToken7 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken7")

testToken8 :: (CurrencySymbol,TokenName)
testToken8 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken8")

otherToken1 :: (CurrencySymbol,TokenName)
otherToken1 = ("7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d","OtherToken1")

otherToken2 :: (CurrencySymbol,TokenName)
otherToken2 = ("7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d","OtherToken2")

otherToken3 :: (CurrencySymbol,TokenName)
otherToken3 = ("7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d","OtherToken3")

otherToken4 :: (CurrencySymbol,TokenName)
otherToken4 = ("7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d","OtherToken4")

otherToken5 :: (CurrencySymbol,TokenName)
otherToken5 = ("7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d","OtherToken5")

otherToken6 :: (CurrencySymbol,TokenName)
otherToken6 = ("7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d","OtherToken6")

otherToken7 :: (CurrencySymbol,TokenName)
otherToken7 = ("7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d","OtherToken7")

otherToken8 :: (CurrencySymbol,TokenName)
otherToken8 = ("7a922050dc48ec0914a3e1c74aad5056de536e7f7cef71f1581a294d","OtherToken8")

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
    user1 :: C.Value
    user1 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1
         <> (uncurry singleton otherToken1) 1
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton otherToken2) 1000

    user2 :: C.Value
    user2 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1
         <> (uncurry singleton otherToken2) 1000
         <> (uncurry singleton otherToken3) 1
    
    user3 :: C.Value
    user3 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken4) 1
         <> (uncurry singleton otherToken2) 1000
         <> (uncurry singleton otherToken4) 1
    
    user4 :: C.Value
    user4 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken5) 1
         <> (uncurry singleton otherToken2) 1000
         <> (uncurry singleton otherToken5) 1
    
    user5 :: C.Value
    user5 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken6) 1
         <> (uncurry singleton otherToken2) 1000
         <> (uncurry singleton otherToken6) 1

    user6 :: C.Value
    user6 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken7) 1
         <> (uncurry singleton otherToken2) 1000
         <> (uncurry singleton otherToken7) 1
  
    wallets :: [(Wallet,C.Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      , (knownWallet 5, user5)
      , (knownWallet 6, user6)
      ]

benchConfig :: EmulatorConfig
benchConfig = emConfig & params .~ params'
  where 
    params' :: Params
    params' = def{emulatorPParams = pParams'}

    pParams' :: PParams
    pParams' = pParamsFromProtocolParams protoParams

    protoParams :: ProtocolParameters
    protoParams = def{ protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000
                                                                        ,executionMemory = 11000000})
                    --  , protocolParamMaxTxSize = 12300
                     }

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

closeSaleUTxO :: CloseSaleParams -> Contract () TraceSchema Text ()
closeSaleUTxO CloseSaleParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  assetsUtxos <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet closeSaleMarketAddress

  let beaconPolicyHashes = map mintingPolicyHash closeSaleBeaconPolicies
      beaconRedeemers = map toRedeemer closeSaleBeaconRedeemer

      closeRedeemer = toRedeemer CloseSale

      lPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy b) 
                         (plutusV2MintingPolicy alwaysSucceedPolicy)
                         closeSaleBeaconPolicies
      
      lookups = lPolicies
             <> plutusV2OtherScript closeSaleMarketVal
             <> Constraints.unspentOutputs assetsUtxos
      
      tx' =
        -- | Burn Beacons
        (mconcat $ zipWith3 (\x y z -> foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer y x t i) 
          mempty
          z)
          beaconRedeemers
          beaconPolicyHashes
          closeSaleBeaconsBurned
        )
        -- | Must spend all utxos to be closed
        <> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash closeSaleMarketVal) 
                        (== toDatum d)
                        (==v) 
                        closeRedeemer
                  ) 
                  mempty 
                  closeSaleSpecificUTxOs
        -- | Must be signed by address staking pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Closed Sale UTxO(s)"

purchase :: PurchaseParams -> Contract () TraceSchema Text ()
purchase PurchaseParams{..} = do
  saleUTxOs <- Map.unions <$> mapM (utxosAt . unsafeFromRight . toCardanoAddressInEra Mainnet) purchaseAddresses

  let beaconPolicyHashes = map mintingPolicyHash purchaseBeaconPolicies
      beaconRedeemers = map toRedeemer purchaseBeaconRedeemer

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
        (mconcat $ zipWith3 (\x y z -> foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer y x t i) 
          mempty
          z)
          beaconRedeemers
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

update :: UpdateParams -> Contract () TraceSchema Text ()
update UpdateParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  saleUTxOs <- Map.unions <$> mapM (utxosAt . unsafeFromRight . toCardanoAddressInEra Mainnet) updateAddresses

  let beaconPolicyHashes = map mintingPolicyHash updateBeaconPolicies
      beaconRedeemers = map toRedeemer updateBeaconRedeemer

      toDatum' = TxOutDatumInline . toDatum

      updateRedeemer = toRedeemer UpdateSale

      lPolicies = foldl' (\a b -> a <> plutusV2MintingPolicy b) 
                         (plutusV2MintingPolicy alwaysSucceedPolicy)
                         updateBeaconPolicies

      lookups = Constraints.unspentOutputs saleUTxOs
             <> plutusV2OtherScript updateVal
             <> lPolicies

      tx' =
        -- | Mint/Burn Beacons
        (mconcat $ zipWith3 (\x y z -> foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer y x t i) 
          mempty
          z)
          beaconRedeemers
          beaconPolicyHashes
          updateBeaconsMinted
        )
        -- | Must spend all utxos to be updated
        <> ( mconcat $ map (foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash updateVal) 
                        (== toDatum d)
                        (==v) 
                        updateRedeemer
                  ) 
                  mempty)
                  updateSpecificUTxOs
           )
        -- | Make change output.
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toDatum' d) v)
              mempty
              b)
              updatePaymentAddresses
              updatePaymentOutputs
           )
        -- | Must be signed by address staking pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Updated Sale UTxO"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createSaleUTxO' = endpoint @"create-sale" createSaleUTxO
    closeSaleUTxO' = endpoint @"close-sale" closeSaleUTxO
    purchase' = endpoint @"purchase" purchase
    update' = endpoint @"update" update
    choices = 
      [ createSaleUTxO'
      , closeSaleUTxO'
      , purchase'
      , update'
      ]