{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey(..),
  
  queryAllSales,
  queryOwnSales,
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust,fromJust)

import CLI.Types
import CardanoSecondaryMarket

-------------------------------------------------
-- Core Types
-------------------------------------------------
-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Newtype wrapper around the beacon asset being queried.
data BeaconId = BeaconId (String,String)

instance ToHttpApiData BeaconId where
  toQueryParam (BeaconId (currSym,tokName)) = T.pack $ currSym <> tokName

-- | An address that contains a beacon.
-- The response type of the beaconAddressList api.
newtype BeaconAddress = BeaconAddress { unBeaconAddress :: String } deriving (Show)

instance FromJSON BeaconAddress where
  parseJSON (Object o) = BeaconAddress <$> o .: "address"
  parseJSON _ = mzero

instance ToHttpApiData BeaconAddress where
  toQueryParam = T.pack . unBeaconAddress

-- | The response type of the beaconInfoApi. This has all the information that may be needed
-- to interact with this UTxO.
data RawBeaconInfo = RawBeaconInfo
  { rawTxHash :: String
  , rawOutputIndex :: Integer
  , rawAmount :: [RawAssetInfo]
  , rawBeaconDataHash :: Maybe String
  } deriving (Show)

instance FromJSON RawBeaconInfo where
  parseJSON (Object o) =
    RawBeaconInfo
      <$> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "amount"
      <*> o .: "data_hash"
  parseJSON _ = mzero

-- | Blockfrost does not separate symbol and name with '.'
data RawAssetInfo = RawAssetInfo
  { rawUnit :: String  -- ^ CurrencySymbol <> TokenName
  , rawQuantity :: Integer
  } deriving (Show)

instance FromJSON RawAssetInfo where
  parseJSON (Object o) =
    RawAssetInfo
      <$> o .: "unit"
      <*> fmap read (o .: "quantity")
  parseJSON _ = mzero

instance FromJSON MarketDatum where
  parseJSON (Object o) = do
    r <- o .: "json_value" >>= return . decodeDatum
    case r of
      Just x -> return x
      Nothing -> mzero
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "addresses"
    :> Get '[JSON] [BeaconAddress]

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" BeaconAddress
    :> "utxos"
    :> Capture "asset" BeaconId
    :> Get '[JSON] [RawBeaconInfo]

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] Value

beaconAddressListApi :<|> beaconUTxOInfoApi :<|> datumApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Blockfrost Query Functions
-------------------------------------------------
queryAllSales :: BlockfrostApiKey -> String -> ClientM [SaleInfo]
queryAllSales apiKey policyId = do
  let beaconId = BeaconId (policyId,"53616c65")
  -- | Get all the addresses that currently hold the beacon.
  addrs <- beaconAddressListApi apiKey beaconId
  -- | Get all the beacon UTxOs for those addresses.
  beaconUTxOs <- concat <$> mapM (\z -> beaconUTxOInfoApi apiKey z beaconId) addrs
  -- | Get all the datums attached to the beacon UTxOs.
  datums <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToSaleInfo beaconUTxOs datums

queryOwnSales :: BlockfrostApiKey -> String -> String -> ClientM [SaleInfo]
queryOwnSales apiKey policyId addr = do
  let beaconAddr = BeaconAddress addr
      beaconId = BeaconId (policyId,"53616c65")
  -- | Get all the beacon UTxOs for those addresses.
  beaconUTxOs <- beaconUTxOInfoApi apiKey beaconAddr beaconId
  -- | Get all the datums attached to the beacon UTxOs.
  datums <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToSaleInfo beaconUTxOs datums

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Skips ones that fail to decode.
fetchDatumsLenient :: FromJSON a => BlockfrostApiKey -> [Maybe String] -> ClientM (Map String a)
fetchDatumsLenient apiKey dhs =
  let go _ datumMap [] = return datumMap
      go key datumMap ((Just d):ds) = do
        i' <- fromJSON <$> datumApi key d
        case i' of
          Success i -> go key (Map.insert d i datumMap) ds
          Error _ -> go key datumMap ds
      go key datumMap (Nothing:ds) = go key datumMap ds
  in go apiKey Map.empty dhs

convertToAsset :: RawAssetInfo -> Asset
convertToAsset RawAssetInfo{rawUnit=u,rawQuantity=q} =
  if u == "lovelace"
  then Asset
        { assetPolicyId = u
        , assetTokenName = ""
        , assetQuantity = q
        }
  else Asset
        { assetPolicyId = take 56 u  -- ^ The policy id is always 56 characters
        , assetTokenName = drop 56 u
        , assetQuantity = q
        }

convertToSaleInfo :: [RawBeaconInfo] -> Map String MarketDatum -> [SaleInfo]
convertToSaleInfo [] _ = []
convertToSaleInfo ((RawBeaconInfo tx ix amount dHash):rs) datumMap =
    info : convertToSaleInfo rs datumMap
  where info = SaleInfo
                { txHash = tx
                , outputIndex = show ix
                , utxoValue = map convertToAsset amount
                , saleInfo = fromJust $ join $ fmap (\z -> Map.lookup z datumMap) dHash
                }