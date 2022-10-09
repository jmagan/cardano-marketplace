{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module NFTStoreV1 (ContractParams (..), ContractDatum (..), ContractRedeemer (..), endpoints, GrabParams (..), StartParams (..), validator, scrAddress) where

import           Control.Lens                (review)
import           Control.Monad               (void)
import qualified Control.Monad.Freer         as FR
import qualified Control.Monad.Freer.Error   as FRE
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.ByteString             as BS
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           GHC.Generics
import           Ledger
import           Ledger.Scripts
import           Ledger.CardanoWallet        (fromSeed', paymentPrivateKey)
import qualified Ledger.Constraints          as Constraints
import qualified Ledger.Constraints          as Contraints
import qualified Plutus.Script.Utils.V1.Typed.Scripts        as Scripts
import           Plutus.Contract
import           Plutus.V1.Ledger.Ada
import           Plutus.V1.Ledger.Api        (toBuiltinData)
import           Plutus.V1.Ledger.Credential
import qualified Plutus.V1.Ledger.Value      as Value
import qualified PlutusTx
import           PlutusTx.Builtins           (divideInteger)
import           PlutusTx.Prelude
import           Prelude                     (Show (..), String)
import qualified Prelude                     as P
import           Text.Printf                 (printf)
import           Wallet.Emulator.Wallet      (selectCoin, signTxWithPrivateKey)
import Ledger.Value (adaOnlyValue)
import Plutus.Script.Utils.V1.Scripts (datumHash)

data ContractParams = ContractParams
    { unBackendPubkeyHash     :: !PubKeyHash
    , unMarketOwnerPubkeyHash :: !PubKeyHash
    }
    deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''ContractParams

data ContractDatum = ContractDatum
    { cdUnitPrice        :: Integer
    , cdSellerPubkeyHash :: PubKeyHash
    , cdCurrencySymbol   :: CurrencySymbol
    , cdTokenName        :: TokenName
    }
    deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ContractDatum
PlutusTx.makeLift ''ContractDatum

data ContractRedeemer = BuyAction Integer
    | Close

PlutusTx.unstableMakeIsData ''ContractRedeemer
PlutusTx.makeLift ''ContractRedeemer

-- traceIfFalse ("Token : " <> decodeUtf8 (consByteString (numberTokenOutScript + 0x30) emptyByteString)) (numberTokenOutScript == 9)
-- Errors:
-- E1 -> Not enough funds to the backend
-- E2 -> Not enough funds to the market owner
-- E3 -> Not enough funds to the seller
-- E4 -> The validator must validate same scripts inputs
-- E5 -> All outputs must have same datum
-- E6 -> Token out of the script not equal to redeemer quantity
-- E7 -> The close of the contract mustbe call by the seller
{-# INLINEABLE mkValidator #-}
mkValidator :: ContractParams -> ContractDatum -> ContractRedeemer -> ScriptContext -> Bool
mkValidator p d (BuyAction qty) ctx =
  traceIfFalse "E1" (valueToBackend `Value.geq` (valueFromBackend <> toValue (lovelaceOf ((totalTransactionCost * 5) `divideInteger` 1_000))))
    && traceIfFalse "E2" (valueToMarketOwner `Value.geq` toValue (lovelaceOf ((totalTransactionCost * 40) `divideInteger` 1_000)))
    && traceIfFalse "E3" (valueToSeller `Value.geq` totalTransactionValue totalTransactionCost adaOutScript (unMarketOwnerPubkeyHash p) (cdSellerPubkeyHash d))
    && traceIfFalse "E4" (numberOfInputsFromThisScript == numberOfScriptsInputs)
    && traceIfFalse "E5" allOutPutsHaveSameDatum
    && traceIfFalse "E6" (tokenOutScript == qty)
   where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    totalTransactionCost = qty * cdUnitPrice d

    unPubkeyCrendetial :: Credential -> PubKeyHash
    unPubkeyCrendetial (PubKeyCredential pkh) = pkh
    unPubkeyCrendetial _                      = PubKeyHash ""

    getPubKeyHashTxInInfo :: TxInInfo -> PubKeyHash
    getPubKeyHashTxInInfo = unPubkeyCrendetial . addressCredential . txOutAddress . txInInfoResolved

    valueFromBackend :: Value
    valueFromBackend = mconcat [txOutValue $ txInInfoResolved tx | tx <- txInfoInputs info, getPubKeyHashTxInInfo tx == unBackendPubkeyHash p]

    valueToBackend :: Value
    valueToBackend = valuePaidTo info (unBackendPubkeyHash p)

    valueToMarketOwner :: Value
    valueToMarketOwner = valuePaidTo info $ unMarketOwnerPubkeyHash p

    valueToSeller :: Value
    valueToSeller = valuePaidTo info (cdSellerPubkeyHash d)

    -- /// SCRIPT OUTPUT ///
    -- Calculate all the values from and to the script

    valueFromScript :: Value
    valueFromScript = foldr fld (lovelaceValueOf 0) (txInInfoResolved <$> txInfoInputs info)
      where
        fld TxOut{txOutValue=val, txOutAddress=Address{addressCredential=ScriptCredential valHash}} acc =
          if ownHash ctx == valHash then
              acc + val
            else
              acc
        fld _ acc = acc

    valueToScript :: Value
    valueToScript = mconcat (txOutValue <$> getContinuingOutputs ctx)

    adaOutScript :: Value
    adaOutScript = adaOnlyValue (valueFromScript <> negate valueToScript)

    tokenOutScript :: Integer
    tokenOutScript = Value.valueOf (valueFromScript <> negate valueToScript) (cdCurrencySymbol d) (cdTokenName d)

    allOutPutsHaveSameDatum :: Bool
    allOutPutsHaveSameDatum = all checkDatums $ txOutDatumHash <$> getContinuingOutputs ctx
      where
        checkDatums :: Maybe DatumHash -> Bool
        checkDatums mdh = mdh == Just (snd $ ownHashes ctx)

    -- //// CHECK SCRIPT INPUTS ////
    -- The inputs must be from the same validator and must have the same Datum

    numberOfScriptsInputs :: Integer
    numberOfScriptsInputs = foldr sumScripts 0 (txInInfoResolved <$> txInfoInputs info)
      where
        sumScripts tx cont = cont + case addressCredential $ txOutAddress tx of
           PubKeyCredential _ -> 0
           ScriptCredential _ -> 1

    numberOfInputsFromThisScript :: Integer
    numberOfInputsFromThisScript = foldr sumScripts 0 (txInInfoResolved <$> txInfoInputs info)
      where
        sumScripts tx cont = cont + case tx of
          TxOut{txOutAddress=Address{addressCredential=ScriptCredential valHash}, txOutDatumHash=Just datHash} ->
            case findOwnInput ctx of
              Just TxInInfo{
                txInInfoResolved=TxOut{txOutAddress=Address{addressCredential=ScriptCredential vh},
                txOutDatumHash=Just dh}
              } -> if dh == datHash && vh == valHash then 1 else 0
              _ -> 0
          _ -> 0
mkValidator _ d Close ctx = traceIfFalse "E7" $ txSignedBy (scriptContextTxInfo ctx) (cdSellerPubkeyHash d)


-- Calculate minumum value to the seller
totalTransactionValue :: Integer -> Value -> PubKeyHash -> PubKeyHash -> Value
totalTransactionValue totalTransactionCost adaOutScript marketOwnerPubKeyHash sellerPubKeyHash = lovelaceValueOf totalTransactionCost
  <> negate (toValue (lovelaceOf ((totalTransactionCost * 5) `divideInteger` 1_000)))
  <> adaOutScript
  <> if marketOwnerPubKeyHash == sellerPubKeyHash then
        lovelaceValueOf 0
    else
        negate $ toValue (lovelaceOf ((totalTransactionCost * 40) `divideInteger` 1_000))

data NFTStoreV1

instance Scripts.ValidatorTypes NFTStoreV1 where
  type DatumType NFTStoreV1 = ContractDatum
  type RedeemerType NFTStoreV1 = ContractRedeemer

typedValidator :: ContractParams -> Scripts.TypedValidator NFTStoreV1
typedValidator p =
  Scripts.mkTypedValidator @NFTStoreV1
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @ContractDatum @ContractRedeemer

validator :: ContractParams -> Validator
validator = Scripts.validatorScript . typedValidator

validatorHash' :: ContractParams -> ValidatorHash
validatorHash' = Scripts.validatorHash . typedValidator

scrAddress :: ContractParams -> Address
scrAddress = scriptAddress . validator

type ContractSchema =
  Endpoint "start" StartParams
    .\/ Endpoint "grab" GrabParams

data StartParams = StartParams
    { spContractParams :: ContractParams
    , spContractDatum  :: ContractDatum
    , spQuantity       :: Integer
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data GrabParams = GrabParams
    { gpContractParams :: ContractParams
    , gpContractDatum  :: ContractDatum
    , gpPassphrase     :: BS.ByteString
    , gpQuantity       :: Integer
    }
    deriving (Show, Generic, ToJSON, FromJSON)

start :: AsContractError e => StartParams -> Contract w s e ()
start sp = do
  let v = toValue minAdaTxOut <> Value.singleton (cdCurrencySymbol $ spContractDatum sp) (cdTokenName $ spContractDatum sp) (spQuantity sp)
      tx = Constraints.mustPayToOtherScript (validatorHash' $ spContractParams sp) (Datum $ toBuiltinData $ spContractDatum sp) v
  ledgerTx <- submitTx tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Address: " ++ show (scrAddress $ spContractParams sp)
  logInfo @String $ printf "Transaction executed, give to the script"

grab :: AsContractError e  => GrabParams -> Contract w s e ()
grab gp = do

  -- Get UTXOs from backend for cover the min ada in the fee
  utxosBackend <-
    utxosAt $
      Address
        { addressCredential = PubKeyCredential (unBackendPubkeyHash $ gpContractParams gp),
          addressStakingCredential = Nothing
        }
  let backendUTXO = head $ [x | x@(TxOutRef {}, PublicKeyChainIndexTxOut {}) <- Map.toList utxosBackend]

  -- Get all the UTXOs from the script filtering by datum. Then, to make selection
  -- for covering the buy order
  allUtxos <- findDatumUTXOs  (gpContractParams gp) (gpContractDatum gp)
  let utxoValues = (\utxo -> (utxo, _ciTxOutValue $ snd utxo )) <$> Map.toList allUtxos
      v = Value.singleton (cdCurrencySymbol $ gpContractDatum gp) (cdTokenName $ gpContractDatum gp) (gpQuantity gp)

  -- Make selection from the Script UTXOs to make the sell
      selection = FR.run $ FRE.runError $ selectCoin utxoValues v
  utxos <- case selection of
        Left  err        ->  throwError $ review _WalletContractError err
        Right selection' -> return $ Map.fromList $ fst <$> fst selection'

  let orefs = fst <$> Map.toList utxos
      txOuts = snd <$> Map.toList utxos

  logInfo @String $ "Address: " ++ show (scrAddress $ gpContractParams gp) ++ " Utxos founds: " ++ show orefs
  logInfo @String $ "Backend UTXO: " ++ show backendUTXO
  let lookups = 
        Constraints.unspentOutputs utxos
        P.<> Constraints.otherScript (validator $ gpContractParams gp)
        P.<> Contraints.unspentOutputs utxosBackend

      totalTransactionCost = cdUnitPrice (gpContractDatum gp) * gpQuantity gp

      totalValueFromScript = mconcat (_ciTxOutValue <$> txOuts)
      totalTokenFromScript = Value.valueOf totalValueFromScript (cdCurrencySymbol $ gpContractDatum gp) (cdTokenName $ gpContractDatum gp)
      valueToScript = if totalTokenFromScript > gpQuantity gp then
                          toValue minAdaTxOut <> Value.singleton (cdCurrencySymbol $ gpContractDatum gp) (cdTokenName $ gpContractDatum gp) (totalTokenFromScript - gpQuantity gp)
                      else mempty

      valueToSeller = totalTransactionValue totalTransactionCost (adaOnlyValue $ totalValueFromScript <> negate valueToScript) (cdSellerPubkeyHash $ gpContractDatum gp) (unMarketOwnerPubkeyHash $ gpContractParams gp)
      valueToMarketOwner = lovelaceValueOf if (totalTransactionCost * 40 `P.div` 1_000) < getLovelace minAdaTxOut then getLovelace minAdaTxOut else totalTransactionCost * 40 `P.div` 1_000

      valueToBackend = lovelaceValueOf (totalTransactionCost * 5 `P.div` 1_000) <> extractValueFromUTXO (snd backendUTXO)

      tx = mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ toBuiltinData $ BuyAction (gpQuantity gp)) | oref <- orefs]
          <> (if valueToScript == mempty then mempty else Constraints.mustPayToOtherScript (validatorHash' $ gpContractParams gp) (Datum $ toBuiltinData $ gpContractDatum gp) valueToScript)
          <> Contraints.mustPayToPubKey (PaymentPubKeyHash $ cdSellerPubkeyHash $ gpContractDatum gp) valueToSeller
          <> Contraints.mustPayToPubKey (PaymentPubKeyHash $ unBackendPubkeyHash $ gpContractParams gp) valueToBackend
          <> Contraints.mustPayToPubKey (PaymentPubKeyHash $ unMarketOwnerPubkeyHash $ gpContractParams gp) valueToMarketOwner
          <> Contraints.mustSpendPubKeyOutput (fst backendUTXO)
          <> Contraints.mustBeSignedBy (PaymentPubKeyHash $ unBackendPubkeyHash $ gpContractParams gp)

  unBalancedTx <- mkTxConstraints @NFTStoreV1 lookups tx
  balancedTx <- balanceTx unBalancedTx
  let signedTx = signTxMockWallet balancedTx (paymentPrivateKey $ fromSeed' $ NFTStoreV1.gpPassphrase gp) (PaymentPubKeyHash $ unBackendPubkeyHash $ gpContractParams gp)

  case signedTx of
    Left wae -> logInfo @String $ show wae
    Right stx -> do
      ledgerTx <- submitBalancedTx stx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "collected gifts"

endpoints :: Contract () ContractSchema Text ()
endpoints = awaitPromise (start' `select` grab') >> endpoints
  where
    start' = endpoint @"start" start
    grab' = endpoint @"grab" grab

extractValueFromUTXO :: ChainIndexTxOut -> Value
extractValueFromUTXO utxo = case utxo of
  PublicKeyChainIndexTxOut _ val -> val
  ScriptChainIndexTxOut {}       -> lovelaceValueOf 0

signTxMockWallet :: CardanoTx -> PaymentPrivateKey -> PaymentPubKeyHash -> Either WalletAPIError CardanoTx
signTxMockWallet balancedTx privateKey pph = FR.run $ FRE.runError $ signTxWithPrivateKey privateKey balancedTx pph

findDatumUTXOs :: AsContractError e => ContractParams -> ContractDatum -> Contract w s e (Map.Map TxOutRef ChainIndexTxOut)
findDatumUTXOs params datum = do
  utxos <- utxosAt $ scrAddress params
  let utxosList = filter f $ Map.toList utxos
  return $ Map.fromList utxosList
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f utxo = case utxo of
      (_, scriptOutput@ScriptChainIndexTxOut{} ) ->
        case _ciTxOutDatum scriptOutput of
          Left hash -> hash == datumHash (Datum $ toBuiltinData datum)
          Right dat -> dat == Datum (toBuiltinData datum)
      _ -> False
