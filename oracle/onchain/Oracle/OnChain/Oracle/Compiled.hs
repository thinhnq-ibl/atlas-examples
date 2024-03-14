{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Oracle.OnChain.Oracle.Compiled
    ( oracleValidator
    ) where

import           Oracle.OnChain.Oracle (mkWrappedValidator)
import           PlutusCore.Version    (plcVersion100)
import           PlutusLedgerApi.V2    (TokenName)
import           PlutusLedgerApi.V3    (BuiltinData, CurrencySymbol, PubKeyHash)
import qualified PlutusTx

-- | Generates validator given parameter.
oracleValidator ::  CurrencySymbol -> TokenName -> PubKeyHash -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
oracleValidator cs tn pubkey =
    $$(PlutusTx.compile [|| mkWrappedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 cs
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 tn
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 pubkey
