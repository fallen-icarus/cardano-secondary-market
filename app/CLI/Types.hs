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

data Script = BeaconPolicy CurrencySymbol | MarketScript

data ConvertAddress
  = Plutus Address
  | Bech32 Text

data ApiKey
  = PreProd String  -- ^ Api key

-- | For when saving to file is optional
data Output = Stdout | File FilePath

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