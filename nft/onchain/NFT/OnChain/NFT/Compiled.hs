{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module NFT.OnChain.NFT.Compiled
    ( nftPolicy
    ) where

import PlutusLedgerApi.V3 ( TxId, BuiltinData )
import qualified PlutusTx
import PlutusLedgerApi.V2 (TokenName)
import PlutusCore.Version (plcVersion100)
import NFT.OnChain.NFT (mkWrappedNFTPolicy)

-- | Generates validator given parameter.
nftPolicy ::  TxId -> Integer -> TokenName -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
nftPolicy txid ix tn =
    $$(PlutusTx.compile [|| mkWrappedNFTPolicy||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 txid
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 ix
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 tn