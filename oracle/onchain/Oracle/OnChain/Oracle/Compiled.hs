{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Oracle.OnChain.Oracle.Compiled
    ( nftPolicy
    ) where

import PlutusLedgerApi.V3 ( BuiltinData, PubKeyHash, CurrencySymbol )
import qualified PlutusTx
import PlutusLedgerApi.V2 (TokenName)
import PlutusCore.Version (plcVersion100)
import Oracle.OnChain.Oracle (mkWrappedValidator)

-- | Generates validator given parameter.
nftPolicy ::  CurrencySymbol -> TokenName -> PubKeyHash -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
nftPolicy cs tn pubkey =
    $$(PlutusTx.compile [|| mkWrappedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 cs
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 tn
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 pubkey