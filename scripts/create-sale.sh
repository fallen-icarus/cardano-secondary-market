#!/bin/sh

# A helper script for showing how to create a new Sale UTxO.

## Variables
dir="../assets/marketplace-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

marketScriptFile="${dir}market.plutus"
marketAddrFile="${dir}market.addr"

beaconRedeemerFile="${dir}mintSale.json"

saleDatumFile="${dir}saleDatum.json"

saleTokenName="53616c65" # Hexidecimal for 'Sale'.

ownerStakingPubKeyFile="../assets/wallets/01Stake.vkey"

## Generate the hash for the owner's staking verification key.
echo "Calculating the staking pubkey hash for the owner..."
ownerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $ownerStakingPubKeyFile)

## Export the marketplace validator script.
echo "Exporting the market validator script..."
cardano-secondary-market export-script market-script \
  --out-file $marketScriptFile

## Create the market address.
echo "Creating the owner's market address..."
cardano-cli address build \
  --payment-script-file $marketScriptFile \
  --stake-verification-key-file $ownerStakingPubKeyFile \
  --testnet-magic 1 \
  --out-file $marketAddrFile

## Export the beacon policy for that policy id.
echo "Exporting the beacon policy script..."
cardano-secondary-market export-script beacon-policy \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Create the minting redeemer.
echo "Creating the minting redeemer..."
cardano-secondary-market beacon-redeemer \
  --mint-sale \
  --out-file $beaconRedeemerFile

## Helper Sale beacon variable.
saleBeacon="${beaconPolicyId}.${saleTokenName}"

## Create the datum.
echo "Creating the datum..."
cardano-secondary-market market-datum \
  --beacon-policy-id $beaconPolicyId \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --nft-token-name 4f74686572546f6b656e0a \
  --desired-asset-is-lovelace \
  --desired-amount 10000000 \
  --payment-pubkey-hash "$(cat ../assets/wallets/01.pkh)" \
  --out-file $saleDatumFile

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in ea9c1447035a1513fc3af66ff8194ee7ec74633b11c4d6c32ac95d831a9f68db#1 \
  --tx-in ea9c1447035a1513fc3af66ff8194ee7ec74633b11c4d6c32ac95d831a9f68db#0 \
  --tx-out "$(cat ${marketAddrFile}) + 3000000 lovelace + 1 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a + 1 ${saleBeacon}" \
  --tx-out-inline-datum-file $saleDatumFile \
  --mint "1 ${saleBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"