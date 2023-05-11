{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}

module CardanoSecondaryMarket
(
  MarketDatum(..),
  MarketRedeemer(..),
  MarketBeaconRedeemer(..),
  CurrencySymbol(..),
  TokenName(..),
  adaSymbol,
  adaToken,
  txIdAsToken,

  marketValidator,
  marketValidatorHash,
  marketValidatorScript,

  marketBeaconPolicy,
  marketBeaconPolicyScript,
  marketBeaconPolicySymbol,
)
where

import Data.Aeson hiding (Value,Market)
import qualified Data.Aeson as Aeson
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as B
import Prelude (IO,FilePath,seq) 
import qualified Prelude as Haskell
import Data.String (fromString)
import Data.Text (Text,pack)
import GHC.Generics (Generic)

import Cardano.Api hiding (Script,Value,TxOut,Address,ScriptHash,TxId)
import Cardano.Api.Shelley (PlutusScript (..))
import Ledger.Tx.CardanoAPI.Internal
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api hiding (getPubKeyHash)
import qualified Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger.Address
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Ledger.Bytes (fromHex)
import qualified Plutonomy
import Ledger.Value (valueOf,flattenValue)
import PlutusTx.Numeric as Num
import PlutusTx.Ratio as Ratio
import PlutusPrelude (foldl')
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import Plutus.V1.Ledger.Bytes (encodeByteString)
import Ledger.Ada (lovelaceValueOf)

-------------------------------------------------
-- Data Types
-------------------------------------------------
-- | Extra parameter for the spending script.
-- This creates a unique secondary market for every policy id. This assumes there is a unique
-- policy id for every NFT that can be resold on the secondary market. For example,
-- an option contract for ADA/DJED will have a unique policy ID. This allows a user to see only
-- the open orders for that specific financial item. This does not sub-divide markets by purchase
-- asset. This market can be composed with cardano-swaps inputs so it is possible to use a different
-- asset to purchase an NFT (assuming there is an intermediate conversion within the tx).
type MarketConfig = CurrencySymbol

data MarketDatum = MarketDatum 
  { -- | The currency symbol of the beacon policy. Every available sale will have a 'Sale'
    -- beacon that will be burned when purchased. The currency symbol is needed for that.
    beaconSymbol :: CurrencySymbol
  
  , nftOnSale :: (CurrencySymbol,TokenName)

    -- | The desired currency with its relative value.
    -- The relative values are always desiredCurrency/NFT. Since there is only one
    -- NFT, the denominator is always one. So the relative values can be simplified
    -- to just an integer.
  , salePrice :: ((CurrencySymbol,TokenName),Integer)

    -- | Address where payment should go to. Since there is no way to specify a datum,
    -- this address must use a pubkey for the payment credential. The staking credential
    -- can stil be a script.
  , payToAddress :: Address
  }
  deriving (Haskell.Show,Generic)

data MarketRedeemer
  = CloseSale -- ^ Owner action.
  | UpdateSale -- ^ Owner action.
  | Purchase -- ^ Buyer action.
  deriving (Haskell.Show,Generic)

data MarketBeaconRedeemer
  = MintSaleBeacon
  | MintReceiptTokens -- ^ Not a beacon. Used to guarantee uniqueness of payment outputs.
  | BurnBeacons
  deriving (Haskell.Show,Generic)

-- | A helper type used to create testing beacons.
type AppName = BuiltinString

PlutusTx.unstableMakeIsData ''MarketDatum
PlutusTx.unstableMakeIsData ''MarketRedeemer
PlutusTx.unstableMakeIsData ''MarketBeaconRedeemer

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Used to create a testing set of beacons/IDs without having to change the logic.
app :: AppName
app = "testing"

{-# INLINABLE parseMarketDatum #-}
parseMarketDatum :: OutputDatum -> MarketDatum
parseMarketDatum d = case d of
  (OutputDatum (Datum d')) -> unsafeFromBuiltinData d'
  _ -> traceError "All market datums must be inline datums."

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput (ScriptContext info (Spending ref)) = getScriptInput (txInfoInputs info) ref
ownInput _ = traceError "script input error"

{-# INLINABLE getScriptInput #-}
getScriptInput :: [TxInInfo] -> TxOutRef -> TxOut
getScriptInput [] _ = traceError "script input error"
getScriptInput ((TxInInfo iRef ot) : tl) ref
  | iRef == ref = ot
  | otherwise = getScriptInput tl ref

{-# INLINABLE signed #-}
signed :: [PubKeyHash] -> PubKeyHash -> Bool
signed [] _ = False
signed (k:ks) k'
  | k == k' = True
  | otherwise = signed ks k'

-- | This function is used to overload the staking credential for owner authorizations.
{-# INLINABLE stakingCredApproves #-}
stakingCredApproves :: Address -> TxInfo -> Bool
stakingCredApproves addr info = case addressStakingCredential addr of
  -- | This is to prevent permanent locking of funds.
  -- The dApp is not meant to be used without a staking credential.
  Nothing -> True

  -- | Check if staking credential signals approval.
  Just stakeCred@(StakingHash cred) -> case cred of
    PubKeyCredential pkh -> signed (txInfoSignatories info) pkh
    ScriptCredential _ -> isJust $ Map.lookup stakeCred $ txInfoWdrl info
  
  Just _ -> traceError "Wrong kind of staking credential."

{-# INLINABLE txIdAsToken #-}
txIdAsToken :: TxId -> TokenName
txIdAsToken (TxId n) = TokenName n

{-# INLINABLE isPaymentPubKeyCred #-}
isPaymentPubKeyCred :: Address -> Bool
isPaymentPubKeyCred (Address (PubKeyCredential _) _) = True
isPaymentPubKeyCred _ = False

-------------------------------------------------
-- On-Chain Market Validator
-------------------------------------------------
-- | This validator is used to guarantee trustless sales of financial NFTs. Every user will
-- get there own address instance for this validator. This ensures users maintain delegation control
-- and custody of their assets at all times. The validator overloads the staking credential for
-- owner related actions.
-- 
-- In order to prevent double satisfaction of payments, each payment output must be guaranteed
-- to be unique. To this end, the following steps are taken:
-- 1) Only one 'Sale' beacon can be minted in a given tx.
-- 2) A tx can never have more than one output with a 'Sale' beacon. It is for this reason that
--    updating sales require separate txs for each sale.
-- 3) Upon purchase, a receipt token must be minted and included in the payment output. The
--    token name of this receipt token will be the tx hash of the corresponding 'Sale' UTxO being
--    consumed.
-- These three steps guarantee that every 'Sale' UTxO will have a unique tx hash and therefore every
-- corresponding receipt token will be unique. Thus, by requiring this receipt token to be included
-- in the corresponding payment output, each payment output is guaranteed to be unique and double
-- satisfaction is impossible.
mkMarketValidator :: MarketDatum -> MarketRedeemer -> ScriptContext -> Bool
mkMarketValidator marketDatum r ctx@ScriptContext{scriptContextTxInfo=info} =
  case r of
    CloseSale ->
      -- | All 'Sale' beacons in tx inputs must be burned.
      traceIfFalse "Sale beacons not burned."
        ( valueOf totalInputValue (beaconSymbol marketDatum) (TokenName "Sale") == 
            Num.negate (valueOf minted (beaconSymbol marketDatum) (TokenName "Sale"))
        ) &&
      -- | The staking credential must signal approval.
      traceIfFalse "Staking credential did not approve" stakingCredApproves'
      -- Note: This can be used to close Sale UTxOs missing the Sale beacon.
    UpdateSale ->
      -- | There can only be one 'Sale' beacon in tx inputs.
      traceIfFalse "Only one Sale beacon allowed in tx"
        (valueOf totalInputValue (beaconSymbol marketDatum) (TokenName "Sale") == 1) &&
      -- | The 'Sale' beacon must be from this UTxO.
      traceIfFalse "This input does not have a Sale beacon"
        (valueOf inputValue (beaconSymbol marketDatum) (TokenName "Sale") == 1) &&
      -- | No 'Sale' beacons can be minted this tx.
      traceIfFalse "No Sale beacons can be minted this tx"
        (valueOf minted (beaconSymbol marketDatum) (TokenName "Sale") == 0) &&
      -- | The following function checks:
      -- 1) The 'Sale' beacon must be re-output to this address.
      -- 2) The new output must have a valid MarketDatum.
      -- 3) The new output must have the proper val: 3 ADA <> nft <> Sale beacon.
      -- It will fail with an appropriate error message or return True.
      updateCheck &&
      -- | The staking credential must signal approval.
      traceIfFalse "Staking credential did not approve" stakingCredApproves'
    Purchase ->
      -- | The receiptID must be minted. This guarantees the minting policy is executed.
      -- The Minting policy does all the other checks.
      traceIfFalse "Receipt token not minted"
        (valueOf minted (beaconSymbol marketDatum) (txIdAsToken inputTxId) == 1)
  where
    -- | Get the credential for this input as well as its value.
    -- Credential is used to ensure staking credential approves when necessary. The value is 
    -- used to quickly check for beacon tokens.
    (inputCredentials,inputValue) = 
      let TxOut{txOutAddress=addr,txOutValue=iVal} = ownInput ctx
      in (addr,iVal)
    
    inputTxId :: TxId
    inputTxId = (\(Spending (TxOutRef tId _)) -> tId) $ scriptContextPurpose ctx

    -- | This tends to build up a thunk so its evaluation is forced even though it is not always
    -- needed.
    stakingCredApproves' :: Bool
    !stakingCredApproves' = stakingCredApproves inputCredentials info

    totalInputValue :: Value
    totalInputValue = valueSpent info

    minted :: Value
    minted = txInfoMint info

    -- | This either returns True or fails with an error.
    validMarketDatum :: MarketDatum -> Bool
    validMarketDatum (MarketDatum bs nft sp pa)
      | bs /= beaconSymbol marketDatum = traceError "Invalid MarketDatum beaconSymbol"
      | nft /= nftOnSale marketDatum = traceError "Invalid MarketDatum nftOnSale"
      | snd sp <= 0 = traceError "Invalid MarketDatum salePrice"
      | not $ isPaymentPubKeyCred pa = traceError "Invalid MarketDatum payToAddress"
      | otherwise = True

    -- | Checks whether the Sale UTxO was updated correctly.
    updateCheck :: Bool
    updateCheck =
      let outputs = txInfoOutputs info
          foo iCred sym nft acc TxOut{ txOutValue = oVal
                                     , txOutAddress = addr
                                     , txOutDatum = d
                                     } =
            if valueOf oVal sym (TokenName "Sale") == 1 then
              if addr == iCred then
                validMarketDatum (parseMarketDatum d) && 
                traceIfFalse "Sale beacon stored with wrong value"
                  ( oVal == lovelaceValueOf 3_000_000 
                         <> (uncurry singleton nft) 1 
                         <> singleton sym (TokenName "Sale") 1
                  ) 
              else traceError "Sale beacon not stored at proper dApp address"
            else acc
      in foldl' (foo inputCredentials (beaconSymbol marketDatum) (nftOnSale marketDatum)) 
                False 
                outputs

data Market
instance ValidatorTypes Market where
  type instance RedeemerType Market = MarketRedeemer
  type instance DatumType Market = MarketDatum

marketValidator :: Validator
marketValidator = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Market
    $$(PlutusTx.compile [|| mkMarketValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

marketValidatorScript :: Script
marketValidatorScript = unValidatorScript marketValidator

marketValidatorHash :: ValidatorHash
marketValidatorHash = Scripts.validatorHash marketValidator

-------------------------------------------------
-- On-Chain Market Beacon Policy
-------------------------------------------------
mkMarketBeaconPolicy :: AppName -> ValidatorHash -- ^ Extra parameters
                     -> MarketBeaconRedeemer -> ScriptContext -> Bool
mkMarketBeaconPolicy appName valHash r ctx@ScriptContext{scriptContextTxInfo = info} =
  case r of
    MintSaleBeacon ->
      -- | The following function checks:
      -- 1) Only one Sale beacon can be minted in this tx.
      -- 2) The Sale beacon must have the token name 'Sale'.
      -- 3) No other tokens can be minted by this policy.
      mintCheck &&
      -- | The following function checks:
      -- 1) The Sale beacon must go to the proper dApp address:
      --     a) The address must use the valHash for the payment credential.
      --     b) The address must have a staking credential.
      -- 2) The Sale beacon must be stored with the proper inline MarketDatum:
      --     a) beaconSymbol == this policy id.
      --     b) nftOnSale == nft stored in UTxO.
      --     c) snd salePrice > 0
      --     d) the payToAddress must use a payment pubkey.
      -- 3) The Sale beacon must be stored with the proper value:
      --     1) 3 ADA <> nft <> Sale beacon
      --     2) No other assets
      destinationCheck
    MintReceiptTokens ->
      -- | The following function checks:
      -- 1) There must be at least one valid Sale input.
      -- 2) There must be a corresponding payment output for every Sale input.
      paymentsMade &&
      -- | The following function checks:
      -- 1) Every Sale beacon in inputs must be burned.
      -- 2) The proper receiptIDs must be minted.
      -- 3) No other tokens can be minted.
      mintCheck
    BurnBeacons ->
      -- | Always allowed as long as this redeemer is only used to burn.
      mintCheck
  where
    beaconSym :: CurrencySymbol
    !beaconSym = ownCurrencySymbol ctx

    minted :: Value
    !minted = txInfoMint info

    -- | Returns only the beacons minted/burned. This is useful for ensuring only
    -- the required beacons are minting.
    beaconMint :: Value
    !beaconMint = case Map.lookup beaconSym $ getValue minted of
      Nothing -> traceError "MintError"
      Just bs -> Value $ Map.insert beaconSym bs Map.empty -- ^ a Value with only beacons

    mintCheck :: Bool
    mintCheck = case r of
      MintSaleBeacon ->
        traceIfFalse "This redeemer only allows minting a single Sale beacon" $
          beaconMint == singleton beaconSym (TokenName "Sale") 1
      MintReceiptTokens ->
        let (_,_,!v) = saleInputs
        in traceIfFalse "This redeemer only allows burning Sale beacons and minting receipt tokens" $
          beaconMint == v
      BurnBeacons ->
        traceIfFalse "Beacons can only be burned with this redeemer" 
          (all (\(_,_,n) -> n < 0) $ flattenValue beaconMint)

    -- | Returns a list of all purchases made this tx. This tries to prepare all the
    -- information so that the tx outputs only needs to be traversed once.
    -- The returned TokenName is the required receipt token name for that purchase.
    -- The outer integer returned is the size of the Map.
    -- The outer Value is the Value that must be minted/burned by this policy this tx.
    -- This also checks for invalid UTxOs since those belong to the address owners.
    saleInputs :: ( Map Address (TokenName,((CurrencySymbol,TokenName),Integer))
                  , Integer
                  , Value
                  )
    saleInputs =
      let inputs = txInfoInputs info
          foo sym (!macc,!s,!mint) TxInInfo{txInInfoOutRef = TxOutRef{txOutRefId = txId}
                                           ,txInInfoResolved = 
                                              TxOut{txOutValue = iVal
                                                   ,txOutDatum = d
                                                   ,txOutAddress = Address{addressCredential = pAddr}
                                                   }
                                           } =
            if pAddr == ScriptCredential valHash then
              if valueOf iVal sym (TokenName "Sale") == 1 then
                let !receiptId = txIdAsToken txId
                    !datum = parseMarketDatum d
                    !tAddr = payToAddress datum
                    !price = salePrice datum
                    !newMap = Map.insert tAddr (receiptId,price) macc
                    !newSize = s + 1
                    !newMint = mint <> singleton sym (TokenName "Sale") (-1) 
                                    <> singleton sym receiptId 1
                in (newMap,newSize,newMint)
              else traceError "Invalid Sale UTxO found"
            else (macc,s,mint)
      in foldl' (foo beaconSym) (Map.empty,0,mempty) inputs

    -- | This returns whether every sale has a corresponding payment output.
    paymentsMade :: Bool
    paymentsMade =
      let outputs = txInfoOutputs info
          (!saleMap,!mapSize,_) = saleInputs
          -- | This returns how many of the saleInputs have a corresponding payment output.
          foo sym sMap acc TxOut{ txOutValue = oVal
                                , txOutAddress = oAddr
                                } =
            case Map.lookup oAddr sMap of
              Nothing -> acc
              Just (receiptId,(curr,i)) ->
                if valueOf oVal adaSymbol adaToken == 3_000_000 &&
                   valueOf oVal sym receiptId == 1 &&
                   uncurry (valueOf oVal) curr == i 
                then acc + 1
                else acc
      in if mapSize == 0 then
          traceError "No Sale inputs found"
         else if foldl' (foo beaconSym saleMap) 0 outputs /= mapSize then 
          traceError "Payment(s) not satisfied"
         else True

    -- | This either returns True or fails with an error.
    validMarketDatum :: MarketDatum -> Bool
    validMarketDatum (MarketDatum bs _ sp pa)
      | bs /= beaconSym = traceError "Invalid MarketDatum beaconSymbol"
      | snd sp <= 0 = traceError "Invalid MarketDatum salePrice"
      | not $ isPaymentPubKeyCred pa = traceError "Invalid MarketDatum payToAddress"
      | otherwise = True

    -- | A helper function for destinationCheck to make the code easier to reason about.
    -- This uses the appName in the error message so that it isn't optimized away.
    validDestination :: ValidatorHash -> Bool
    validDestination spendVh
      | spendVh /= valHash = 
          traceError ("Beacon not minted to the proper " <> appName <> " address")
      | otherwise = True

    -- | This checks that the beacons are stored with the proper values. Helps simplify
    -- destinationCheck. Either fails with error or returns True.
    utxoHasProperValue :: Value -> MarketDatum -> Bool
    utxoHasProperValue oVal MarketDatum{..} =
      if oVal == lovelaceValueOf 3_000_000
              <> (uncurry singleton nftOnSale) 1
              <> singleton beaconSym (TokenName "Sale") 1
      then True
      else traceError "Sale UTxO not stored with proper value"

    destinationCheck :: Bool
    destinationCheck =
      let outputs = txInfoOutputs info
          foo acc TxOut{txOutDatum = d
                       ,txOutValue = oVal
                       ,txOutAddress = Address{addressCredential = addrCred
                                              ,addressStakingCredential = maybeStakeCred}
                       } =
            if valueOf oVal beaconSym (TokenName "Sale") > 0 then
              let datum = parseMarketDatum d
              in case (addrCred,maybeStakeCred) of
                (ScriptCredential vh, Just (StakingHash _)) -> 
                      acc && validDestination vh && validMarketDatum datum && 
                      utxoHasProperValue oVal datum
                _ -> traceError "Sale beacon must go to a dApp address with a staking credential"
            else acc
      in foldl' foo True outputs

marketBeaconPolicy :: MintingPolicy
marketBeaconPolicy = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
  ($$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode app
    `PlutusTx.applyCode` PlutusTx.liftCode marketValidatorHash)
  where
    wrap x y = mkUntypedMintingPolicy $ mkMarketBeaconPolicy x y

marketBeaconPolicyScript :: Script
marketBeaconPolicyScript = unMintingPolicyScript marketBeaconPolicy

marketBeaconPolicySymbol :: CurrencySymbol
marketBeaconPolicySymbol = scriptCurrencySymbol marketBeaconPolicy

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

toJSONValue :: PlutusTx.ToData a => a -> Aeson.Value
toJSONValue = scriptDataToJson ScriptDataJsonDetailedSchema
           . dataToScriptData
           . PlutusTx.toData

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . toJSONValue

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                         $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData = writeJSON

decodeDatum :: (FromData a) => Aeson.Value -> Maybe a
decodeDatum = unsafeFromRight . fmap (PlutusTx.fromBuiltinData . fromCardanoScriptData)
            . scriptDataFromJson ScriptDataJsonDetailedSchema

-------------------------------------------------
-- Off-Chain Helper Functions and Types
-------------------------------------------------
unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = Haskell.error "unsafeFromRight used on Left"