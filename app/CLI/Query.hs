{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  runQueryAllSales,
  runQueryOwnSales
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi as Blockfrost
import CLI.Types
import CardanoSecondaryMarket

runQueryAllSales :: ApiKey -> CurrencySymbol -> IO [SaleInfo]
runQueryAllSales (PreProd apiKey) currSym = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllSales apiKey' (show currSym)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnSales :: ApiKey -> CurrencySymbol -> MarketAddress -> IO [SaleInfo]
runQueryOwnSales (PreProd apiKey) currSym addr = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryOwnSales apiKey' (show currSym) (show addr)) env
  case res of
    Right r -> return r
    Left err -> throw err