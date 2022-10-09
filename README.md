# Cardano Marketplace Contract

This repository contains standard smart contracts for trade fungible and non-fungible token in the Cardano blockchain. The objetive is to create an open example contract for promoting the use and develop of the Cardano blockchain.

## Use Cases

At the moment, there are two basic use cases for the purchase of tokens fulfilled with the contract. It capables of selling NFTs and FTs. In the case of FTs, a partial purchase can be made and the rest of the token returns to the smart contract for a future sale.

The validator is constrained to one script Utxo at time. This decision was made to simplify the logic behind the validator and in order to get it more secure.

This implementation has 4 actors: seller, buyer, NFT creator and the backend operator. The seller and the buyer are part of the token transaction and they operate under a marketplace supported by a backend operator. The backend operator get a fee of each transaction and she/he can configure the royalties to pay to the NFT/FT creator.

### Use Case 1: NFT sale within a script UTXO

![Use Case 1 NFT sale within a script UTXO](doc/img/Use%20Case%201%20NFT%20sale%20within%20a%20script%20UTXO.jpg)

### Use Case 2: Partial FT sale within a script UTXO

![Use Case 2 Partial FT sale within a script UTXO](doc/img/Use%20Case%202%20Partial%20FT%20sale%20within%20a%20script%20UTXO.jpg)

### Use Case 3: Partial FT or NFT sale cancelation

The third case is to cancel the sale. The script must unsure that only the person who created the script utxo can recive the tokens inside it.

![Use Case 3 Partial FT or NFT sale cancelation](doc/img/Use%20Case%203%20Partial%20FT%20or%20NFT%20sale%20cancelation.jpg)

## Build

The project is built with Nix and Cabal. Once Nix is correctly installed and configurated in the system (Documentation [Nix configuration](https://github.com/input-output-hk/plutus/blob/master/README.adoc#how-to-build-the-projects-artifacts)), you can start a Nix shell with the command:

```
$ nix-shell 
```

It should prepare the haskell environment downloading and installing all the dependencies. Then you can build the project and run the tests:

```
$ cabal build
$ cabal test
```

## User stories and further steps

There are still some interesting features and improvements to be implemented. Theses user stories show the current and the coming development of the project:

- [x] Lock tokens into trustless script for sale
- [x] Script compatible with NFTs and FTs
- [x] Allow partial purchase in case of FTs
- [x] While only single script utxo is allowed, secure implementation to avoid double validation problem
- [x] Take into account the ADA locked into the script and return it to the seller when the script full spent
- [x] Implement royalties in the validation
- [x] Implemente a cancelation mechanism for recover the locked tokens
- [ ] Translate to Plutus V2
- [ ] Include payment in other cardano tokens via oracles
- [ ] Optimize the memory and cpu use
- [ ] Global validator logic to include multiple script Utxos and multiple buyers

