# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

When integration testing, it is highly recommended that you change the string passed to the mkBeaconPolicy function [here](src/CardanoSecondaryMarket.hs#150). When developers make mistakes (myself included), it can create bad/locked utxos that will appear when you query the beacons. This can complicate your own testing. To avoid this, this extra parameter was added. Change the string to something unique to you. You should remember to change it to the desired string for mainnet.

**Make this change before building the executable in the next section.**

The `cardano-secondary-market` CLI uses the Blockfrost API endpoints for the Preproduction Testnet (Koios does not have endpoints for the Preproduction Testnet). You will need an api key to query the beacon tokens. You can go [here](https://blockfrost.io/#pricing) to get one for free; only an email address is required.

If a specific beacon token has never been minted before, querying the Blockfrost endpoints will return "The requested component has not been found." This is due to the beacon name being part of the Blockfrost api url like:

``` Url
https://cardano-preprod.blockfrost.io/api/v0/assets/{beacon_name}/addresses
```

If the beacon has not been minted before, this URL does not exist yet. Once the beacon is minted, the URL is generated. If the beacons have been minted before but there are currently no beacons in circulation, then the API will return an empty list.

---
## Table of Contents
- [Installing](#installing)
- [Minting test tokens](#minting-test-tokens)
- [Address Conversions](#address-conversions)
- [Calculate Credential Hashes](#calculate-credential-hashes)
- [Create a new Sale](#create-a-new-sale)
- [Update a Sale](#update-a-sale)
- [Close a Sale](#close-a-sale)
- [Query your own Sales for a given policy id](#query-your-own-sales-for-a-given-policy-id)
- [Query all Sales for a given policy id](#query-all-sales-for-a-given-policy-id)
- [Purchase an NFT](#purchase-an-nft)
- [Burning receipt tokens](#burning-receipt-tokens)

---
## Installing

### Using Nix
The [Nix Package Manager](https://nixos.org/) can be installed on most Linux distributions by downloading and running the installation script:
```
curl -L https://nixos.org/nix/install > install-nix.sh
chmod +x install-nix.sh
./install-nix.sh
```
and following the directions.

#### Configuring the Binary Caches
While this step is optional, it can save several hours of build time. Therefore, it is highly recommended that you do this.
```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee -a /etc/nix/nix.conf
experimental-features = nix-command flakes
allow-import-from-derivation = true
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
EOF
```
The caches used here come from the plutus-apps contributing [doc](https://github.com/input-output-hk/plutus-apps/blob/713955dea45739de6df3c388717123cfec648914/CONTRIBUTING.adoc#how-to-get-a-shell-environment-with-tools).

You will need to restart the nix service in order to make sure that it uses the newly configured caches. A sure fire way to do this is restart your machine.

#### Building the Executable
You do not need sudo for this step. It is advised that you execute these steps as a standard user.
```
git clone https://github.com/fallen-icarus/cardano-secondary-market
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout 68c3721
nix develop # This step can take a few hours even with the caches configured
```
The last command should drop you into a nix terminal once it is finished running. Execute the following within the nix terminal.
```
cd ../cardano-secondary-market
cabal clean
cabal update
cabal build all
```

If all goes well, the `cardano-secondary-market` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-secondary-market-0.1.0.0/x/cardano-secondary-market/build/cardano-secondary-market/cardano-secondary-market`. Move the program to somewhere in your $PATH.

You can now exit the nix terminal with `exit`.

All `cardano-secondary-market` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

#### Troubleshooting
If you encounter a libsodium error, you may need to first install libsodium separately. While not inside the nix terminal, execute the following:
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh # Install GHC and cabal
ghcup install ghc 8.10.7
ghcup set ghc 8.10.7
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install
```

Once installed, you can retry the build after exporting the following variables while inside the nix terminal:
```
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
```

--- 
## Minting test tokens
An always succeeding minting policy as well as the required redeemer are included [here](scripts/mint-test-tokens/). In that directory is also the template bash script that uses them. These can be used to create as many native tokens as needed to test this lending dApp.

---
## Address Conversions
Since plutus smart contracts do not use bech32 encoded addresses while cardano-cli does, addresses will need to be converted as necessary. To make this as painless as possible, `cardano-secondary-market` is capable of doing these conversions for you. It uses [`cardano-addresses`](https://github.com/input-output-hk/cardano-addresses) under the hood.

#### Hashes to Bech32
```
cardano-secondary-market convert-address \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --staking-pubkey-hash 623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c \
  --stdout
```
If the address does not have a staking credential, those fields can be omitted. **All Bech32 addresses generated with this command will be for the Preproduction testnet.**

#### Bech32 to Hashes
```
cardano-secondary-market convert-address \
  --address addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f \
  --stdout
```

This will result in the following output when piped to `jq`:
``` JSON
{
  "network_tag": 0,
  "payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
  "payment_script_hash": null,
  "staking_pubkey_hash": null,
  "staking_script_hash": null
}
```

The `network_tag` of 0 corresponds to the Preproduction testnet (1 would be Mainnet). This address uses a spending pubkey and has no staking credential.

---
## Calculate Credential Hashes
These commands can also save to a variable by omitting the `--out-file` field.

#### Calculate the hash of a payment pubkey
```
cardano-cli address key-hash \
  --payment-verification-key-file ownerPayment.vkey \
  --out-file ownerPayment.pkh
```

#### Calculate the hash of a staking pubkey
```
cardano-cli stake-address key-hash \
  --stake-verification-key-file ownerStake.vkey \
  --out-file ownerStake.pkh
```

#### Calculate the hash of a script (payment, minting, or staking)
```
cardano-cli transaction policyid \
  --script-file script.plutus \
  --out-file script.h
```

---
## Create a new Sale
This example uses a pubkey for the owner's staking credential.

#### Export the scripts
```
cardano-secondary-market export-script market-script \
  --out-file marketplace.plutus

cardano-secondary-market export-script beacon-policy \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --out-file beacons.plutus
```
The beacon policy required is dependent on the policy id of the NFT being sold. Make sure you use the correct policy id. The transaction will fail if the wrong beacon policy is used.

#### Create the owner's marketplace address
```
cardano-cli address build \
  --payment-script-file marketplace.plutus \
  --stake-verification-key-file ownerStake.vkey \
  --testnet-magic 1 \
  --out-file market.addr
```

#### Calculate the beacon policy id
```
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

#### Create the minting redeemer
```
cardano-secondary-market beacon-redeemer \
  --mint-sale \
  --out-file mintSale.json
```

#### Create the Sale datum
You will need the hashes of the owner's payToAddress.
```
cardano-secondary-market market-datum \
  --beacon-policy-id $beaconPolicyId \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --nft-token-name 4f74686572546f6b656e0a \
  --desired-asset-is-lovelace \
  --desired-amount 10000000 \
  --payment-pubkey-hash "$(cat ownerPayment.pkh) \
  --staking-pubkey-hash "$(cat ownerStake.pkh) \
  --out-file saleTerms.json
```
This datum says the NFT is on sale for 10 ADA.

#### Create and submit the transaction
```
saleBeacon="${beaconPolicyId}.53616c65"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_with_nft_and_fee> \
  --tx-out "$(cat market.addr) + 3000000 lovelace + 1 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a + 1 ${saleBeacon}" \
  --tx-out-inline-datum-file saleTerms.json \
  --mint "1 ${saleBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file mintSale.json \
  --change-address "$(cat owner.addr)" \
  --tx-in-collateral <collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file ownerPayment.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Update a Sale
This allows the seller to change the terms of a sale. **All sales must be updated in separate transactions to guarantee that each sale will have a unique transaction hash.**

The beacon policy is not executed but is usefull for getting the beacon policy id. If you already know the beacon policy id, the beacon policy steps (exporting and calculating the policy id) can be omitted.

#### Export the scripts
```
cardano-secondary-market export-script market-script \
  --out-file marketplace.plutus

cardano-secondary-market export-script beacon-policy \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --out-file beacons.plutus
```
The beacon policy required is dependent on the policy id of the NFT being sold. Make sure you use the correct policy id. The transaction will fail if the wrong beacon policy is used.

#### Calculate the beacon policy id
```
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

#### Create the new Sale datum
```
cardano-secondary-market market-datum \
  --beacon-policy-id $beaconPolicyId \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --nft-token-name 4f74686572546f6b656e0a \
  --desired-asset-is-lovelace \
  --desired-amount 20000000 \
  --payment-pubkey-hash "$(cat ownerPayment.pkh) \
  --staking-pubkey-hash "$(cat ownerStake.pkh) \
  --out-file saleTerms.json
```

The beacon policy id, and the nft policy id and token name must remain the same. However, the rest of the fields can be changed. The transaction will fail if the you change a field that should not be changed.

#### Create the spending redeemer
```
cardano-secondary-market market-redeemer \
  --update \
  --out-file update.json
```

#### Create and submit the transaction
```
saleBeacon="${beaconPolicyId}.53616c65"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_with_fee> \
  --tx-in <sale_utxo> \
  --tx-in-script-file marketplace.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file update.json \
  --tx-out "$(cat market.addr) + 3000000 lovelace + 1 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a + 1 ${saleBeacon}" \
  --tx-out-inline-datum-file saleTerms.json \
  --required-signer-hash ownerStake.pkh \
  --change-address "$(cat owner.addr)" \
  --tx-in-collateral <collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file ownerPayment.skey \
  --signing-key-file ownerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Close a Sale

#### Export the scripts
```
cardano-secondary-market export-script market-script \
  --out-file marketplace.plutus

cardano-secondary-market export-script beacon-policy \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --out-file beacons.plutus
```
The beacon policy required is dependent on the policy id of the NFT being sold. Make sure you use the correct policy id. The transaction will fail if the wrong beacon policy is used.

#### Calculate the beacon policy id
```
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

#### Create the spending redeemer
```
cardano-secondary-market market-redeemer \
  --close \
  --out-file close.json
```

#### Create the burn redeemer
```
cardano-secondary-market beacon-redeemer \
  --burn \
  --out-file burn.json
```

#### Create and submit the transaction
```
saleBeacon="${beaconPolicyId}.53616c65"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_for_fee> \
  --tx-in <sale_utxo> \
  --tx-in-script-file marketplace.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file close.json \
  --tx-out "$(cat owner.addr) + 3000000 lovelace + 1 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "-1 ${saleBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burn.json \
  --required-signer-hash "$(cat ownerStake.pkh) \
  --change-address "$(cat owner.addr)" \
  --tx-in-collateral <collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file ownerPayment.skey \
  --signing-key-file ownerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Query your own Sales for a given policy id
```
cardano-secondary-market query own-sales \
  --address $(cat market.addr) \
  --preprod $(cat api.txt) \
  --beacon-policy-id a18b36e32e3fbb3f132dfd98f4e958539c28912d7e485768fd74c89e \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "output_index": "0",
    "sale_info": {
      "beacon_symbol": "a18b36e32e3fbb3f132dfd98f4e958539c28912d7e485768fd74c89e",
      "nft_for_sale": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "pay_to_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "pay_to_address_payment_script_hash": null,
      "pay_to_address_staking_pubkey_hash": null,
      "pay_to_address_staking_script_hash": null,
      "sale_price": {
        "amount": 10000000,
        "asset": "lovelace"
      }
    },
    "tx_hash": "2fa45e5d847911809ff557e39fd063ddeeace5a8489a44fb7156b65fa41168af",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "a18b36e32e3fbb3f132dfd98f4e958539c28912d7e485768fd74c89e.53616c65",
        "quantity": 1
      },
      {
        "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 1
      }
    ]
  }
]
```

---
### Query all Sales for a given policy id
```
cardano-secondary-market query all-sales \
  --preprod $(cat api.txt) \
  --beacon-policy-id a18b36e32e3fbb3f132dfd98f4e958539c28912d7e485768fd74c89e \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "output_index": "0",
    "sale_info": {
      "beacon_symbol": "a18b36e32e3fbb3f132dfd98f4e958539c28912d7e485768fd74c89e",
      "nft_for_sale": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
      "pay_to_address_payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
      "pay_to_address_payment_script_hash": null,
      "pay_to_address_staking_pubkey_hash": null,
      "pay_to_address_staking_script_hash": null,
      "sale_price": {
        "amount": 10000000,
        "asset": "lovelace"
      }
    },
    "tx_hash": "2fa45e5d847911809ff557e39fd063ddeeace5a8489a44fb7156b65fa41168af",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "a18b36e32e3fbb3f132dfd98f4e958539c28912d7e485768fd74c89e.53616c65",
        "quantity": 1
      },
      {
        "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 1
      }
    ]
  }
]
```

---
## Purchase an NFT

#### Export the scripts
```
cardano-secondary-market export-script market-script \
  --out-file marketplace.plutus

cardano-secondary-market export-script beacon-policy \
  --nft-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --out-file beacons.plutus
```
The beacon policy required is dependent on the policy id of the NFT being sold. Make sure you use the correct policy id. The transaction will fail if the wrong beacon policy is used.

#### Calculate the beacon policy id
```
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

#### Create the spending redeemer
```
cardano-secondary-market market-redeemer \
  --purchase \
  --out-file purchase.json
```

#### Create the minting redeemer
```
cardano-secondary-market beacon-redeemer \
  --mint-receipt \
  --out-file mintReceipt.json
```

#### Convert the address in the datum to bech32
```
paymentAddr=$(cardano-secondary-market convert-address \
  --payment-pubkey-hash fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f \
  --stdout)
```

#### Create and submit the transaction
```
targetSaleHash="2fa45e5d847911809ff557e39fd063ddeeace5a8489a44fb7156b65fa41168af"
saleBeacon="${beaconPolicyId}.53616c65"
receiptToken="${beaconPolicyId}.${targetSaleHash}"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_for_fee_and_payment> \
  --tx-in 2fa45e5d847911809ff557e39fd063ddeeace5a8489a44fb7156b65fa41168af#0 \
  --tx-in-script-file marketplace.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file purchase.json \
  --tx-out "$(cat buyer.addr) + 3000000 lovelace + 1 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out "${paymentAddr} + 13000000 lovelace + 1 ${receiptToken}" \
  --mint "-1 ${saleBeacon} + 1 ${receiptToken}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file mintReceipt.json \
  --change-address "$(cat buyer.addr)" \
  --tx-in-collateral <collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file buyer.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

You are able to buy multiple NFTs in one transaction. If you do, make sure to properly match the receiptToken with the required payment. The transaction will fail if the payment outputs are not exactly what the script expects. The payment output should always be:
```
3 ADA + purchase amount + 1 receipt token with the proper token name
```

The 3 ADA is the seller's minimum UTxO deposit that you are returning to them.

---
## Burning receipt tokens
Burning receipt tokens is always allowed with the burn redeemer. All that is needed is the beacon policy and the burn redeemer.