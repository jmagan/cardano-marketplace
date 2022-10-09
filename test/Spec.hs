{-
 * Created Date: Tuesday May 24th 2022
 * Author: Juan Salvador Magán Valero
 * 
 * Copyright (c) 2022 Juan Salvador Magán Valero. All rights resreved.
 * 
 */
 -}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators #-}

module Main (main, test1, extractCost) where

import           Codec.Serialise        (serialise)
import           Control.Monad          (void)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as BSL
import           Data.Default           (Default (..))
import qualified Data.Map               as Map
import           Ledger
import           Ledger.CardanoWallet   (fromSeed')
import qualified Ledger.Value           as Value
import           CardanoMarketplace
import           Plutus.Trace hiding (params)
import qualified Plutus.V1.Ledger.Ada   as Ada
import           Wallet.Emulator
import           Wallet.Emulator.Wallet (toMockWallet)
import qualified Plutus.Trace as Emulator.Extract
import qualified Ledger as Emulator.Extract

main :: IO ()
main = test1

v :: Value
v = Ada.lovelaceValueOf 1_000_000_000

vToken :: Value
vToken = Value.singleton "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e" "NAME" 40

walletPassphrase :: ByteString
walletPassphrase = BSL.toStrict $ serialise @Int 2

w1 :: Wallet
w1 =
  Wallet
    { prettyWalletName = Just "NFT Creator Wallet",
      getWalletId = getWalletId $ knownWallet 1
    }

w2 :: Wallet
w2 =
  Wallet
    { prettyWalletName = Just "Backend Wallet",
      getWalletId = getWalletId $ toMockWallet $ fromSeed' walletPassphrase
    }

w3 :: Wallet
w3 =
  Wallet
    { prettyWalletName = Just "Seller Wallet",
      getWalletId = getWalletId $ knownWallet 3
    }

w4 :: Wallet
w4 =
  Wallet
    { prettyWalletName = Just "Buyer Wallet",
      getWalletId = getWalletId $ knownWallet 4
    }



emCfg :: EmulatorConfig
emCfg =
  def
    { _initialChainState =
        Left $
          Map.fromList
            [ (w1, v),
              (w2, v),
              (w3, v <> vToken),
              (w4, v)
            ]
    }

test1 :: IO ()
test1 = runEmulatorTraceIO' def emCfg test1ET

test1ET :: EmulatorTrace ()
test1ET = do
  h3 <- activateContractWallet w3 endpoints
  h4 <- activateContractWallet w4 endpoints

  let params = ContractParams {
    unRoyaltiesPubkeyHash = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash w1,
    unBackendPubkeyHash = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash w2
  }

  let datum = ContractDatum {
    cdUnitPrice = 2_014_000,
    cdSellerPubkeyHash = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash w3,
    cdCurrencySymbol = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e",
    cdTokenName = "NAME"
  }

  let startParams = StartParams {
    spContractParams = params,
    spContractDatum = datum,
    spQuantity=10
  }

  let buyParms = BuyParams {
    gpContractParams = params,
    gpPassphrase = walletPassphrase,
    gpContractDatum = datum,
    gpQuantity = 10
  }

  callEndpoint @"start" h3 startParams
  void $ waitNSlots 10
  callEndpoint @"start" h3 startParams
  void $ waitNSlots 10
  callEndpoint @"buy" h4 buyParms
  void $ waitNSlots 1



scriptConfig :: ScriptsConfig
scriptConfig = Emulator.Extract.ScriptsConfig "output/script" cmd
  where
    cmd = Emulator.Extract.Scripts Emulator.Extract.FullyAppliedValidators


extractCost :: IO ()
extractCost = do
  (totalSize, exBudget) <- writeScriptsTo scriptConfig "Marketplace" test1ET emCfg
  putStrLn $ "Total size = " <> show totalSize
  putStrLn $ "ExBudget = " <> showExBudgetPerc exBudget

showExBudgetPerc :: ExBudget -> String
showExBudgetPerc exBudget = show exBudget <> percStr
  where
    
    getCostingIntegerExCPU (ExCPU a) = a
    getCostingIntegerExMemory (ExMemory a) = a

    percStr = "( exCPUPerc: " <> show (getCostingIntegerExCPU (exBudgetCPU exBudget) * 100 `div` 10000000000 ) <> 
      ", exMemoryPerc: " <> show (getCostingIntegerExMemory (exBudgetMemory exBudget) * 100 `div` 14000000 ) <> ")"