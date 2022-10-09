{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Utils.Deploy
    ( writeJSON
    , writeValidator
    , writeUnit
    , writeContractValidator
    , bech32toAddress
    , toPaymentHash
    , cardaniaWhiteFCDatum
    , buyOneRedeemer
    , scriptAddress
    , closeRedeemer, scriptAddressCredential, stakeCredential, scriptFullAddress) where

import           Cardano.Api
import           Cardano.Api.Shelley   
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text             as Text
import qualified Ledger
import           Ledger.Value          (TokenName (TokenName))
import           NFTStoreV1
import           PlutusTx              (Data (..))
import qualified PlutusTx
import Plutus.V1.Ledger.Api (Validator, unValidatorScript)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)


dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . unValidatorScript

writeUnit :: IO ()
writeUnit = writeJSON "output/testnet/unit.json" ()

writeContractValidator :: String -> String -> IO (Either (FileError ()) ())
writeContractValidator backendPKH marketOwnerPKH= writeValidator "output/contract.plutus" $ validator $ ContractParams
    {
        unBackendPubkeyHash= Ledger.PubKeyHash $ stringToBuiltinByteString backendPKH,
        unMarketOwnerPubkeyHash=Ledger.PubKeyHash $ stringToBuiltinByteString marketOwnerPKH
    }

-- unBackendPubkeyHash="84fab74abaff1d265aaf2110cd8185015a19aef93ca271cda261fd32",
-- unMarketOwnerPubkeyHash="8518c178c2a8220e5d3c49068b844b5681b556f2581204bb7dec8917"

bech32toAddress :: String -> Maybe AddressAny
bech32toAddress s = deserialiseAddress AsAddressAny $ Text.pack s

toPaymentHash :: AddressAny -> String
toPaymentHash (AddressShelley (ShelleyAddress _ paymentKey _)) = show paymentKey
toPaymentHash _ = "byron address"


cardaniaWhiteFCDatum :: ContractDatum
cardaniaWhiteFCDatum = ContractDatum {
    cdUnitPrice = 10000000,
    cdSellerPubkeyHash = "457029452ddb8b30492c202168783de82489cb1b26ebce761a714264",
    cdCurrencySymbol = "641593ca39c5cbd3eb314533841d53e61ebf6ee7a0ec7c391652f31e",
    cdTokenName = TokenName "CardaniaFounderWhite"
  }

buyOneRedeemer :: ContractRedeemer
buyOneRedeemer =  BuyAction 1

closeRedeemer :: ContractRedeemer
closeRedeemer = Close

scriptAddress :: Ledger.Address
scriptAddress = scrAddress $ ContractParams {
        unBackendPubkeyHash="84fab74abaff1d265aaf2110cd8185015a19aef93ca271cda261fd32",
        unMarketOwnerPubkeyHash="8518c178c2a8220e5d3c49068b844b5681b556f2581204bb7dec8917"
    }

-- Script Address: 5b1d8c80b4f34cc88fb17706db6872711e9b3bd3717e7a4a4e1771a6

scriptAddressCredential :: PaymentCredential
scriptAddressCredential = PaymentCredentialByScript "5b1d8c80b4f34cc88fb17706db6872711e9b3bd3717e7a4a4e1771a6"

stakeCredential :: StakeAddressReference
stakeCredential = StakeAddressByValue $ StakeCredentialByScript "8e3d0922ae4c28fb22506887d6e5e6f7197e21a5760ae56caa7743d6"

scriptFullAddress :: Text.Text
scriptFullAddress = serialiseToBech32 $  makeShelleyAddress Mainnet scriptAddressCredential stakeCredential





        