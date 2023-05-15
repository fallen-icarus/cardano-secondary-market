#!/bin/sh

# A helper script for showing how to close a Sale UTxO.

## Variables
dir="../assets/marketplace-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

marketScriptFile="${dir}market.plutus"

beaconRedeemerFile="${dir}burnBeacons.json"

spendingRedeemerFile="${dir}update.json"

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

## Export the beacon policy for that policy id.
echo "Exporting the beacon policy script..."
cardano-secondary-market export-script beacon-policy \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Helper Sale beacon variable.
saleBeacon="${beaconPolicyId}.${saleTokenName}"

## Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-secondary-market market-redeemer \
  --close \
  --out-file $spendingRedeemerFile

## Create the burning redeemer.
echo "Creating the burn redeemer..."
cardano-secondary-market beacon-redeemer \
  --burn \
  --out-file $beaconRedeemerFile

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 47d4806204175ec18c923d05e7a04636f091f0dd4107c1a0d5d2f8da1d552d10#1 \
  --tx-in 47d4806204175ec18c923d05e7a04636f091f0dd4107c1a0d5d2f8da1d552d10#0 \
  --tx-in-script-file $marketScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $spendingRedeemerFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 3000000 lovelace + 1 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "-1 ${saleBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash $ownerPubKeyHash \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --signing-key-file ../assets/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"