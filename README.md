# Cardano-Secondary-Market

## Abstract

This protocol shows how a secondary market can be created for any NFT. This market is most similar to an order book. While this protocol is meant for financial NFTs that use *lock and key* NFTs, this protocol can be used for any tradable NFT such as gaming NFTs or even art NFTs.

## Motivation

Most defi protocols are closed systems. They are also concentrated dApps. This protocol gives full control over all assets as well as allowing for natural secondary markets to form.

## Specification

### *Lock and Key* NFTs

Lock and key NFTs are an extension of the beacon token pattern. These tokens serve two purposes: as beacons for efficiently finding information about its counterpart, and as a pair for "unlocking" the UTxO with the lock NFT. The Key NFT can be freely traded while the Lock NFT is always kept with the actual financial UTxO. For example, in `cardano-options`, when an option contract is purchased, one lock and one key NFT are both created. They are identical. One stays with the assets at the script address (this is the lock NFT) and the other can be freely traded. To actually execute the option contract, both NFTs are required. This means that whoever holds the Key NFT can execute the option contract still located at the script address. And since the two NFTs are identical, you always know what the name of the beacon asset to look up in order to find information about the contract. This protocol was designed for easily creating secondary markets for Key NFTs.

### A Unique Beacon Policy For Every NFT Policy ID

This protocol assumes that all Key NFTs for a given application will share a policy id. For example, all option contracts for ADA/DJED will have the same policy id and all loans created by `cardano-loans` will have a different policy id. With this in mind, a separate secondary-market beacon policy is created for each policy id. This way, users can easily query only the sales of the items they desire. Since gaming NFTs will also likely share a policy id, this protocol can easily work for them, too.

### A Universal Market Validator

Since the beacons serve to sort the sales of different items, there is no reason to give each market its own family of addresses. Therefore, all users will use the same validator script no matter what asset is being sold. The validator is capable of enforcing the sales occur as they should.

This protocol works as a distributed dApp so all users get their own market address where they uniquely maintain spending custody (when appropriate) and delegation control.

### Buyers can pay directly to the seller's personal address

The protocol enforces that the designated address is paid the proper value when the sale is accepted. This means the funds are not paid to the script, only to be withdrawn by the seller later. Instead the market validator will only released the asset for sale if the required payment is made to the target address. 

Since there is no way to designate the required datum to use with the payment, this protocol requires that the payment address uses a payment pubkey credential.

### Receipt Token

Since the protocol allows paying directly to the seller's personal address, double satisfaction is possible upon payments. To prevent this, every payment output must be guaranteed to be unique. To this end, a unique token is required to be included with every payment output. This is a receipt token. The token name of the receipt token is the transaction hash of the corresponding Sale UTxO being consumed. The protocol enforces that every transaction with Sale beacons can never have more than one output with a Sale beacon. This means every Sale beacon is guaranteed to be associated with a unique transaction hash. And since hashes are cryptographically guaranteed to be unique, the protocol can use this security assumption as the foundation for preventing double satisfaction of purchases.

:notebook: Each protocol in the familiy (loans, options, and secondary-market) uses a different method for handling double satisfaction. This was to enable trying different possible solutions. The mature form of these protocols will likely settle on a single method that has the best trade-offs.

### Composable Purchases

By preventing double satisfaction, purchases can be arbitrarily composed into a single transaction with no risk of a double satisfaction. You can even compose purchases made of different NFT policy ids. For example, you can buy an option Key NFT and a loan Key NFT in the same transaction.

### The Sale Datum
``` Haskell
data MarketDatum = MarketDatum 
  { -- | The currency symbol of the market beacon policy for this NFT.
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
    -- can be anything, including none.
  , payToAddress :: Address
  }
```

The first version of this marketplace only allows a single asset to be specified for the `salePrice`. Given that this dApp can be composed with `cardano-swaps`, stablecoins can easily be converted between each other in the same transaction where the NFT is being purchased.

### The Marketplace Redeemer
``` Haskell
data MarketRedeemer
  = CloseSale -- ^ Owner action.
  | UpdateSale -- ^ Owner action.
  | Purchase -- ^ Buyer action.
```

### The Marketplace Beacon Redeemer
``` Haskell
data MarketBeaconRedeemer
  = MintSaleBeacon
  | MintReceiptTokens -- ^ Not a beacon. Used to guarantee uniqueness of payment outputs.
  | BurnBeacons
```

### The Marketplace Config
```
type MarketConfig = CurrencySymbol
```
The `MarketConfig` is an extra parameter to the minting policy to create a unique minting policy for every policy id, i.e. family of NFTs being sold.

### The Lifecylce

#### Creating a Sale
Creating a new sale involves minting a `Sale` beacon and storing it with the NFT for sale at the user's marketplace address with the terms in the datum.

Minting a `Sale` beacon requires the `MintSaleBeacon` redeemer and all the following must be true:
1. Only one `Sale` beacon can be minted this transaction.
2. The `Sale` beacon must have the token name "Sale".
3. No other tokens can be minted by this marketplace policy.
4. The `Sale` beacon must go to an address using the marketplace validator as the payment credential.
5. The `Sale` beacon must go to an address with a staking credential.
6. The `Sale` beacon must be stored with the proper inline `MarketDatum`:
        - beaconSymbol == this policy id
        - the currency symbol for nftOnSale == currency symbol in `MarketConfig`
        - nftOnSale == the NFT stored in the UTxO
        - the salePrice > 0
        - the payToAddress msut use a payment pubkey
7. The `Sale` beacon must be stored with the proper value:
        - 3 ADA + nft for sale + `Sale` beacon
        - no other assets

#### Closing an open Sale
Closing an open sale involves burning the `Sale` beacon and reclaiming the NFT. Only the marketplace address owner can use this redeemer. The owner can close multiple open sales in a single transaction.

To close a sale, both the `CloseSale` and `BurnBeacons` redeemers are required. The following must all be true:

1. All `Sale` beacons among the tx inputs must be burned.
2. The address' staking credential must signal approval.

#### UpdateSale
It is possible to update an open sale in place. Only the address owner can do this. To update the terms of an open sale, the `UpdateSale` redeemer must be used and all the following must be true:

1. There can only be one `Sale` beacon among the transaction inputs.
2. The `Sale` beacon must be in the UTxO being updated.
3. No `Sale` beacons can be minted this transaction.
4. The `Sale` beacon msut be re-output to this address.
5. The output must have a valid inline `MarketDatum`:
        - the beaconSymbol must be the same as the input's datum
        - the nftOnSale must be the same as the input's datum
        - the salePrice must be > 0
        - the payToAddress must use a payment pubkey
6. The output must have the proper value:
        - 3 ADA + nft for sale + `Sale` beacon
        - no other assets
7. The address' staking credential must signal approval.

The first three requirements guarantee that each sale will have a unique transaction hash which is required to compose purchases. The trade-off here is that only one sale can be updated per transaction.

#### Purchasing an NFT
Purchasing an NFT involves minting a receipt token, burning the `Sale` beacon, and making the required payment to the payToAddress in the sale datum.

The `Purchase` redeemer and `MintReceiptToken` redeemers must be used, and all the following must be true:
1. A receipt token must be minted with the token name being the transaction hash of the target sale input.
2. There must be at least one valid sale input in the transaction, demarcated by the presence of a `Sale` beacon.
3. There must be the required payment output for every sale input.
4. Every `Sale` beacon among the transaction inputs must be burned.
5. The receipt tokens for each sale input must be minted.
6. No other tokens can be minted by the marketplace beacon policy.

The marketplace minting policy does all of the necessary checks; the marketplace validator just needs to ensure the minting policy is executed by checking that the required receipt token is minted for each sale UTxO being spent. By pushing the heavy logic into the minting policy, the impact of the redundant executions is heavily minimized.

### Fee Estimations (YMMV)

| Action | Tx Fee |
|--|--|
| Create a New Sale | 0.50613 ADA |
| Update a Sale | 0.526772 ADA |
| Close a Sale | 0.666328 ADA |
| Purchase an NFT | 0.736944 |
| Burn a Receipt Token | 0.389625 ADA |

In testing, it was possible to purchase 4 NFTs under the same policy before exceeding the transaction limits. Mixing NFTs of different policies will result in less total NFTs since each policy will require the appropriate minting script to be included in the transaction as well. Using reference scripts instead of local scripts can yield better results.