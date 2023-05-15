#!/bin/sh

# A helper script for showing how to purchase an NFT.

## Variables
dir="../assets/marketplace-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

marketScriptFile="${dir}market.plutus"

beaconRedeemerFile="${dir}mintReceipt.json"

spendingRedeemerFile="${dir}purchase.json"

saleTokenName="53616c65" # Hexidecimal for 'Sale'.

targetSaleHash="2fa45e5d847911809ff557e39fd063ddeeace5a8489a44fb7156b65fa41168af"

# Export the marketplace validator script.
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
receiptToken="${beaconPolicyId}.${targetSaleHash}"

## Create the spending redeemer.
echo "Creating the spending redeemer..."
cardano-secondary-market market-redeemer \
  --purchase \
  --out-file $spendingRedeemerFile

## Create the minting redeemer.
echo "Creating the minting redeemer..."
cardano-secondary-market beacon-redeemer \
  --mint-receipt \
  --out-file $beaconRedeemerFile

## Generate the bech32 payment address.
echo "Converting the payment address to bech32..."
paymentAddr=$(cardano-secondary-market convert-address \
  --payment-pubkey-hash fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f \
  --stdout)

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 1c2798410ce33f1d6efc473d9a4971bc002a2846681255d8fd2e04d61d7e54aa#2 \
  --tx-in 2fa45e5d847911809ff557e39fd063ddeeace5a8489a44fb7156b65fa41168af#0 \
  --tx-in-script-file $marketScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $spendingRedeemerFile \
  --tx-out "$(cat ../assets/wallets/02.addr) + 3000000 lovelace + 1 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "${paymentAddr} + 13000000 lovelace + 1 ${receiptToken}" \
  --mint "-1 ${saleBeacon} + 1 ${receiptToken}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"