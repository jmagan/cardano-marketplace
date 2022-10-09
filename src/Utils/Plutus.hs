{-# LANGUAGE OverloadedStrings #-}

module Utils.Plutus (showIntegerBuiltinString) where

import           PlutusTx.Builtins (addInteger, divideInteger,
                                    greaterThanEqualsInteger, lessThanInteger,
                                    modInteger, multiplyInteger)
import           PlutusTx.Prelude  as PP
import qualified Prelude           hiding (($))

{-# INLINEABLE showIntegerBuiltinString #-}
showIntegerBuiltinString :: Integer -> BuiltinString
showIntegerBuiltinString i
  | i `greaterThanEqualsInteger` 10     = appendString (showIntegerBuiltinString (i `divideInteger` 10))
                                                       (decodeUtf8 $ consByteString ((i `modInteger` 10)  `addInteger` 0x30) emptyByteString)
  | i `lessThanInteger` 0               = appendString "-" $ showIntegerBuiltinString $ PP.negate i `multiplyInteger` i
  | otherwise                           = decodeUtf8 $ consByteString ((i `modInteger` 10)  `addInteger` 0x30) emptyByteString


