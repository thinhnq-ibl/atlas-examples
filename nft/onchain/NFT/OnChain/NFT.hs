{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module NFT.OnChain.NFT (mkWrappedNFTPolicy) where


import           PlutusLedgerApi.V1.Value     (flattenValue)
import           PlutusLedgerApi.V2       (BuiltinData,
                                             ScriptContext (scriptContextTxInfo),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             )
import           PlutusLedgerApi.V1         (TokenName)
import qualified PlutusTx
import           PlutusTx.Prelude           (Bool (True), Eq ((==)), any,
                                             traceIfFalse, ($), (&&), (.), check)

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy ::  TokenName -> () ->  PlutusLedgerApi.V2.ScriptContext -> Bool
mkNFTPolicy tn _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info ::  PlutusLedgerApi.V2.TxInfo
    info =  scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO =  True --any (\i ->  txInInfoOutRef i == oref) $  txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = any (\(_,tn'',i) -> i == 1 && tn'' == tn) . flattenValue $ txInfoMint info

-- nftPolicy ::  MintingPolicy
-- nftPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkNFTPolicy ||])


{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: TokenName -> PlutusLedgerApi.V2.BuiltinData -> PlutusLedgerApi.V2.BuiltinData -> ()
mkWrappedNFTPolicy tn _ ctx = check $ mkNFTPolicy tn () (PlutusTx.unsafeFromBuiltinData ctx)
  -- where
    -- oref ::  TxOutRef
    -- oref =  TxOutRef
    --     ( PlutusTx.unsafeFromBuiltinData tid)
    --     (PlutusTx.unsafeFromBuiltinData ix)

    -- tn ::  TokenName
    -- tn = PlutusTx.unsafeFromBuiltinData tn'


