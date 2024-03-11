{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module NFT.OnChain.NFT.Compiled
    ( nftPolicy
    ) where

import qualified PlutusTx

import           NFT.OnChain.NFT (mkWrappedNFTPolicy)
import           PlutusLedgerApi.V1         (TokenName)  
import           PlutusLedgerApi.V2       (BuiltinData, UnsafeFromData (unsafeFromBuiltinData))
import           PlutusCore.Version      (plcVersion100)

-- | Generates validator given parameter.
nftPolicy ::  TokenName -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
nftPolicy tn = 
    $$(PlutusTx.compile [|| mkWrappedNFTPolicy||]) 
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 tn