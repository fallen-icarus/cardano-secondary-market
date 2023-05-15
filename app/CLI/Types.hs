{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module CLI.Types where

import Data.Aeson
import Data.Text
import GHC.Word (Word32)

import CardanoSecondaryMarket

data Command
  = ExportScript Script FilePath
  | CreateMarketDatum MarketDatum FilePath
  | CreateMarketRedeemer MarketRedeemer FilePath
  | CreateBeaconRedeemer MarketBeaconRedeemer FilePath
  | ConvertAddress ConvertAddress Output
  | QueryBeacons Query

data Script = BeaconPolicy CurrencySymbol | MarketScript

data ConvertAddress
  = Plutus Address
  | Bech32 Text

data ApiKey
  = PreProd String  -- ^ Api key

data Query
  = QueryAllSales ApiKey CurrencySymbol Output
  | QueryOwnSales ApiKey CurrencySymbol MarketAddress Output

-- | For when saving to file is optional
data Output = Stdout | File FilePath

newtype MarketAddress = MarketAddress String

instance Show MarketAddress where
  show (MarketAddress s) = s

data AddressInfo' = AddressInfo'
  { addressSpendingKeyHash :: Maybe Text
  , addressSpendingScriptHash :: Maybe Text
  , addressStakeKeyHash :: Maybe Text
  , addressStakeScriptHash :: Maybe Text
  , addressNetworkTag :: Word32
  }

instance ToJSON AddressInfo' where
  toJSON AddressInfo'{..} =
    object [ "payment_pubkey_hash" .= addressSpendingKeyHash 
           , "payment_script_hash" .= addressSpendingScriptHash
           , "staking_pubkey_hash" .= addressStakeKeyHash
           , "staking_script_hash" .= addressStakeScriptHash
           , "network_tag" .= addressNetworkTag
           ]

data Asset = Asset
  { assetPolicyId :: String
  , assetTokenName :: String
  , assetQuantity :: Integer
  } deriving (Show)

instance ToJSON Asset where
  toJSON Asset{..} =
    object [ "asset" .= if assetPolicyId == "lovelace" 
                        then "lovelace" 
                        else assetPolicyId <> "." <> assetTokenName
           , "quantity" .= assetQuantity
           ]

data SaleInfo = SaleInfo
  { txHash :: String
  , outputIndex :: String
  , utxoValue :: [Asset]
  , saleInfo :: MarketDatum
  }

instance ToJSON SaleInfo where
  toJSON SaleInfo{..} =
    object [ "tx_hash" .= txHash
           , "output_index" .= outputIndex
           , "utxo_assets" .= utxoValue
           , "sale_info" .= saleInfo
           ]

newtype PurchaseAsset = PurchaseAsset ((CurrencySymbol,TokenName),Integer)

instance ToJSON PurchaseAsset where
  toJSON (PurchaseAsset (a,n)) =
    object [ "asset" .= toAsset a
           , "amount" .= n
           ]

instance ToJSON MarketDatum where
  toJSON MarketDatum{..} =
    object [ "beacon_symbol" .= show beaconSymbol
           , "nft_for_sale" .= toAsset nftOnSale
           , "sale_price" .= PurchaseAsset salePrice
           , "pay_to_address_payment_pubkey_hash" .= (show <$> toPubKeyHash payToAddress)
           , "pay_to_address_payment_script_hash" .= (show <$> toValidatorHash payToAddress)
           , "pay_to_address_staking_pubkey_hash" .= (show <$> toStakePubKeyHash payToAddress)
           , "pay_to_address_staking_script_hash" .= (show <$> toStakeValidatorHash payToAddress)
           ]