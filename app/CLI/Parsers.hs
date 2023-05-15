{-# LANGUAGE OverloadedStrings #-}

module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative
import Data.Text (pack)

import CardanoSecondaryMarket
import CLI.Types

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "export-script"
      (info parseExportScript $ progDesc "Export a dApp plutus script.")
  , command "market-datum"
      (info pCreateMarketDatum $ progDesc "Create a marketplace datum.")
  , command "market-redeemer"
      (info pCreateMarketRedeemer $ progDesc "Create a marketplace redeemer.")
  , command "beacon-redeemer"
      (info pCreateBeaconRedeemer $ progDesc "Create a beacon redeemer.")
  , command "convert-address"
      (info pConvertAddress $ progDesc "Convert plutus address <--> Bech32 address.")
  , command "query"
      (info parseQueryBeacons $ progDesc "Query the dApp's beacons.")
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = hsubparser $ mconcat
    [ command "beacon-policy"
        (info pExportPolicy $ progDesc "Export the beacon policy for a specific policy id.")
    , command "market-script"
        (info pExportOptions $ progDesc "Export the marketplace validator script.")
    ]
  where
    pExportPolicy :: Parser Command
    pExportPolicy = ExportScript <$> pPolicy <*> pOutputFile

    pExportOptions :: Parser Command
    pExportOptions = ExportScript <$> pure MarketScript <*> pOutputFile

    pPolicy :: Parser Script
    pPolicy = BeaconPolicy <$> pNFTPolicyId

-------------------------------------------------
-- CreateMarketDatum Parser
-------------------------------------------------
pCreateMarketDatum :: Parser Command
pCreateMarketDatum = CreateMarketDatum <$> pMarketDatum <*> pOutputFile
  where
    pMarketDatum :: Parser MarketDatum
    pMarketDatum = 
      MarketDatum
        <$> pBeaconPolicy
        <*> pNftForSale
        <*> pSalePrice
        <*> pAddress

-------------------------------------------------
-- CreateMarketRedeemer
-------------------------------------------------
pCreateMarketRedeemer :: Parser Command
pCreateMarketRedeemer =
    CreateMarketRedeemer
      <$> (pCloseSale <|> pUpdateSale <|> pPurchase)
      <*> pOutputFile
  where
    pCloseSale :: Parser MarketRedeemer
    pCloseSale = flag' CloseSale
      (  long "close"
      <> help "Close current sale(s)."
      )
    
    pUpdateSale :: Parser MarketRedeemer
    pUpdateSale = flag' UpdateSale
      (  long "update"
      <> help "Update the terms of a sale."
      )

    pPurchase :: Parser MarketRedeemer
    pPurchase = flag' Purchase
      (  long "purchase"
      <> help "Purchase NFT(s)"
      )

-------------------------------------------------
-- CreateBeaconRedeemer
-------------------------------------------------
pCreateBeaconRedeemer :: Parser Command
pCreateBeaconRedeemer =
    CreateBeaconRedeemer
      <$> (pMintSale <|> pMintReceipt <|> pBurn)
      <*> pOutputFile
  where
    pMintSale :: Parser MarketBeaconRedeemer
    pMintSale = flag' MintSaleBeacon
      (  long "mint-sale"
      <> help "Mint a Sale beacon."
      )
    
    pMintReceipt :: Parser MarketBeaconRedeemer
    pMintReceipt = flag' MintReceiptTokens
      (  long "mint-receipt"
      <> help "Mint receipt tokens."
      )

    pBurn :: Parser MarketBeaconRedeemer
    pBurn = flag' BurnBeacons
      (  long "burn"
      <> help "Burn beacons."
      )

-------------------------------------------------
-- ConvertAddress Parser
-------------------------------------------------
pConvertAddress :: Parser Command
pConvertAddress = 
    ConvertAddress <$> (pBech <|> pPlutus) <*> pOutput
  where
    pBech :: Parser ConvertAddress
    pBech = Bech32 . pack <$> pBech32Address

    pPlutus :: Parser ConvertAddress
    pPlutus = Plutus <$> pAddress

-------------------------------------------------
-- QueryBeacons Parser
-------------------------------------------------
parseQueryBeacons :: Parser Command
parseQueryBeacons = fmap QueryBeacons . hsubparser $ mconcat
    [ command "all-sales"
        (info pAllSales $ progDesc "Query all sales for a certain policy id.")
    , command "own-sales"
        (info pOwnSales $ progDesc "Query all own Sales for a given policy id.")
    ]
  where
    pAllSales :: Parser Query
    pAllSales = QueryAllSales <$> pApiKey <*> pBeaconPolicy <*> pOutput

    pOwnSales :: Parser Query
    pOwnSales = QueryOwnSales <$> pApiKey <*> pBeaconPolicy <*> pMarketAddr <*> pOutput

    pMarketAddr :: Parser MarketAddress
    pMarketAddr = MarketAddress <$> pBech32Address

-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )

pNFTPolicyId :: Parser CurrencySymbol
pNFTPolicyId = option (eitherReader readCurrencySymbol)
  (  long "nft-policy-id"
  <> metavar "STRING"
  <> help "The desired NFT policy id."
  )

pBeaconPolicy :: Parser CurrencySymbol
pBeaconPolicy = option (eitherReader readCurrencySymbol)
  (  long "beacon-policy-id"
  <> metavar "STRING"
  <> help "Policy id for that trading pair's beacon policy.")

pNftForSale :: Parser (CurrencySymbol,TokenName)
pNftForSale = (,) <$> pNftForSaleCurrencySymbol <*> pNftForSaleTokenName
  where
    pNftForSaleCurrencySymbol :: Parser CurrencySymbol
    pNftForSaleCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "nft-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the NFT being sold."
      )

    pNftForSaleTokenName :: Parser TokenName
    pNftForSaleTokenName = option (eitherReader readTokenName)
      (  long "nft-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the NFT being sold."
      )

pSalePrice :: Parser ((CurrencySymbol,TokenName),Integer)
pSalePrice = (,) <$> pDesiredAsset <*> pAmount
  where
    pDesiredAsset :: Parser (CurrencySymbol,TokenName)
    pDesiredAsset = 
      pDesiredAssetLovelace <|> ((,) <$> pDesiredAssetCurrencySymbol <*> pDesiredAssetTokenName)
    
    pDesiredAssetLovelace :: Parser (CurrencySymbol,TokenName)
    pDesiredAssetLovelace = flag' (adaSymbol,adaToken)
      (  long "desired-asset-is-lovelace"
      <> help "The desired asset is lovelace"
      )

    pDesiredAssetCurrencySymbol :: Parser CurrencySymbol
    pDesiredAssetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "desired-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the desired asset."
      )

    pDesiredAssetTokenName :: Parser TokenName
    pDesiredAssetTokenName = option (eitherReader readTokenName)
      (  long "desired-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the desired asset."
      )

    pAmount :: Parser Integer
    pAmount = option auto
      (  long "desired-amount"
      <> metavar "INT"
      <> help "The amount of the desired asset required for purchase."
      )

pAddress :: Parser Address
pAddress = 
    Address
      <$> pPaymentCredential
      <*> (pStakingCredential <|> pure Nothing)
  where
    pPaymentScriptCredential :: Parser Credential
    pPaymentScriptCredential = ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "payment-script-hash"
      <> metavar "STRING"
      <> help "The hash of the payment script used in the address."
      )

    pPaymentPubKeyCredential :: Parser Credential
    pPaymentPubKeyCredential = PubKeyCredential <$> option (eitherReader readPubKeyHash)
      ( long "payment-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the payment pubkey used in the address."
      )

    pPaymentCredential :: Parser Credential
    pPaymentCredential = pPaymentPubKeyCredential <|> pPaymentScriptCredential

    pStakingScriptCredential :: Parser StakingCredential
    pStakingScriptCredential = StakingHash . ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "staking-script-hash"
      <> metavar "STRING"
      <> help "The hash of the staking script used in the address."
      )

    pStakingPubKeyCredential :: Parser StakingCredential
    pStakingPubKeyCredential = StakingHash . PubKeyCredential <$> option (eitherReader readPubKeyHash)
      (  long "staking-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the staking pubkey used in the address."
      )

    pStakingCredential :: Parser (Maybe StakingCredential)
    pStakingCredential = Just <$> (pStakingPubKeyCredential <|> pStakingScriptCredential)

pBech32Address :: Parser String
pBech32Address = strOption
  (  long "address"
  <> metavar "STRING"
  <> help "Address in bech32 format."
  )

pApiKey :: Parser ApiKey
pApiKey = pPreProd
  where
    pPreProd :: Parser ApiKey
    pPreProd = PreProd <$> strOption
      (  long "preprod"
      <> metavar "STRING"
      <> help "Query the preproduction testnet using the Blockfrost Api with the supplied api key."
      )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )