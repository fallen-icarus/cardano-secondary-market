# Cardano-Secondary-Market

## Abstract

This protocol shows how a secondary market can be created for any NFT. This market is most similar to an order book. While this protocol is meant for financial NFTs that use *lock and key* NFTs, this protocol can be used for any tradable NFT such as gaming NFTs or even art NFTs.

## Motivation

Most defi protocols are closed systems. They are also concentrated dApps. This protocol gives full control over all assets as well as allowing for natural secondary markets to form.

## Specification

### *Lock and Key* NFTs

Lock and key NFTs are an extension of the beacon token pattern. These tokens serve two purposes: as beacons for efficiently finding information about its counterpart, and as a pair for "unlocking" the UTxO with the lock NFT. The Key NFT can be freely traded while the Lock NFT is always kept with the actual financial UTxO. For example, in `cardano-options`, when an option contract is purchased, one lock and one key NFT are both created. They are identical. One stays with the assets at the script address (this is the lock NFT) and the other can be freely traded. To actually execute the option contract, both NFTs are required. This means that whoever holds the Key NFT can execute the option contract still located at the script address. And since the two NFTs are identical, you always know what the name of the beacon asset to look up in order to find information about the contract. This protocol was designed for easily creating secondary markets for the Key NFTs.

### A Unique Beacon Policy For Every NFT Policy ID

This protocol assumes that all Key NFTs for a given application will share a policy id. For example, all option contracts for ADA/DJED will have the same policy id and all loans created by `cardano-loans` will have another policy id. With this in mind, a separate beacon policy is created for each policy id. This way, users can easily query only the sales of the items they desire. Since gaming NFTs will also likely share a policy id, this protocol can easily work this them too.

### A Universal Market Validator

Since the beacons serve to sort the sales of different items, there is no reason to give each market its own family of addresses (this is a distributed dApp). Therefore, all users will use the same validator script no matter what asset is being sold. The validator is capable of enforcing the sales occur as they should.

### Buyers can pay directly to the seller's personal address

The protocol enforces that the designated address is paid the proper value when the sale is accepted. This means the funds are not paid to the script only to be withdrawn by the seller later. Since there is no way to designate the required datum to use with the payment, this protocol requires that the payment address uses a payment pubkey credential.

Since plutus uses a different format for addresses than `cardano-cli`, the included `cardano-secondary-market` CLI has the ability to easily convert between the two formats as needed.

### Receipt Token

Since the protocol allows paying directly to the seller's personal address, double satisfaction is possible upon payments. To prevent this, every payment output must be guaranteed to be unique. One method would be to use datums for this but parsing the datums can be costly. A cheaper method is to just check the value. To this end, a unique token is required to be included with every payment output. This is a receipt token. The token name of the receipt token is the transaction hash of the corresponding Sale UTxO being consumed. The protocol enforces that every transaction with Sale beacons can never have more than one output with a Sale beacon. This means every Sale beacon is guaranteed to be associated with a unique transaction hash. And since hashes are cryptographically guaranteed to be unique, the protocol can use this security assumption as the foundation for preventing double satisfaction of purchases.

### Composable Purchases

By using the receipt tokens, purchases can be arbitrarily composed into a single transaction with no risk of a double satisfaction. You can even compose purchases made of different NFT policy ids. For example, you can buy an option Key NFT and a loan Key NFT in the same transaction.

### The Sale Datum
``` Haskell
data MarketDatum = MarketDatum 
  { -- | The currency symbol of the beacon policy for this NFT.
    beaconSymbol :: CurrencySymbol
  
    -- | The currency symbol of the NFT must match what the minting policy expects. The minting
    -- of the Sale beacon will fail if the NFT has the wrong currency symbol.
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
```

The first version of this marketplace only allows a single asset to be specified for the `salePrice`. Given that this dApp can be composed with `cardano-swaps`, stablecoins can easily be converted between each other in the same transaction where the NFT is being purchased.

### Fee Estimations (YMMV)

| Action | Tx Fee |
|--|--|
| Create a New Sale | 0.50613 ADA |
| Update a Sale | 0.526772 ADA |
| Close a Sale | 0.666328 ADA |
| Purchase an NFT | 0.736944 |
| Burn a Receipt Token | 0.389625 ADA |

In testing, it was possible to purchase 4 NFTs under the same policy before exceeding the transaction limits. Mixing NFTs of different policies will result in less total NFTs since each policy will require the appropriate minting script to be included in the transaction as well.

### Example Sale Beacon Query
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