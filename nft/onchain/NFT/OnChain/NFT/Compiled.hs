{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module NFT.OnChain.NFT.Compiled
    ( nftPolicy
    ) where

import PlutusLedgerApi.V3 ( BuiltinData, TxOutRef )
import qualified PlutusTx
import PlutusLedgerApi.V2 (TokenName)
import PlutusCore.Version (plcVersion100)
import NFT.OnChain.NFT (mkWrappedNFTPolicy)

-- | Generates validator given parameter.
nftPolicy ::  TxOutRef -> TokenName -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
nftPolicy oref tn =
    $$(PlutusTx.compile [|| mkWrappedNFTPolicy||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 oref
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 tn