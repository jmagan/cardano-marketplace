{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Utils.Deploy
    ( writeJSON
    , writeValidator
    , writeUnit
    , writeContractValidator
    , scriptAddress
    , scriptAddressCredential
    , contractParams) where

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise            (serialise)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as BC8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as SBS
import           Data.Either                (fromRight)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Ledger
import           CardanoMarketplace
import           Plutus.V1.Ledger.Api       (Validator, unValidatorScript)
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins.Internal as Builtin

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

contractParams :: String -> String -> ContractParams
contractParams backendPKH royaltiesPKH = ContractParams
    {
        unBackendPubkeyHash= Ledger.PubKeyHash $ Builtin.BuiltinByteString $ fromRight  ""  (B16.decode $ BC8.pack backendPKH),
        unRoyaltiesPubkeyHash=Ledger.PubKeyHash $ Builtin.BuiltinByteString $ fromRight  ""  (B16.decode $ BC8.pack royaltiesPKH) 
    }

writeContractValidator :: String -> String -> IO (Either (FileError ()) ())
writeContractValidator backendPKH royaltiesPKH = writeValidator "output/contract.plutus" $ validator $ contractParams backendPKH royaltiesPKH


scriptAddress :: String -> String -> Ledger.Address
scriptAddress backendPKH royaltiesPKH = scrAddress $ contractParams backendPKH royaltiesPKH

scriptAddressCredential :: String -> PaymentCredential
scriptAddressCredential str = case deserialisedCbor str of
        Just sh -> PaymentCredentialByScript sh
        Nothing -> error "Error"
    where
        deserialisedCbor = deserialiseFromRawBytesHex AsScriptHash . Text.encodeUtf8 . Text.pack





