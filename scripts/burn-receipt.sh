#!/bin/sh

# A helper script for showing how to burn receipt tokens.

## Variables
dir="../assets/marketplace-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

beaconRedeemerFile="${dir}burnBeacons.json"

receipt="a18b36e32e3fbb3f132dfd98f4e958539c28912d7e485768fd74c89e.2fa45e5d847911809ff557e39fd063ddeeace5a8489a44fb7156b65fa41168af"

## Export the beacon policy for that policy id.
echo "Exporting the beacon policy script..."
cardano-secondary-market export-script beacon-policy \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --out-file $beaconPolicyFile

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
  --tx-in 7d1b271cccd1e6b1b15bf4e0e0abc7be37cf14a128400e11544bfd2a64fba841#1 \
  --mint "-1 ${receipt}" \
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