{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-} 

module NFT.OnChain.NFT (mkWrappedNFTPolicy) where


import qualified Data.ByteString.Char8      as BS8
import           PlutusLedgerApi.V1.Value     (flattenValue)
import           PlutusLedgerApi.V2       (BuiltinData, CurrencySymbol,
                                             ScriptContext (scriptContextTxInfo),
                                             TxId (TxId, getTxId),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             )   
import           PlutusLedgerApi.V1         (TokenName)    
import           PlutusLedgerApi.V1      (TxOutRef)                                    
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False, True), Eq ((==)), any,
                                             traceIfFalse, ($), (&&), (.), check)
import           Prelude                    (Show (show), String)

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy ::  TokenName -> () ->  ScriptContext -> Bool
mkNFTPolicy tn _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info ::  TxInfo
    info =  scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO =  True --any (\i ->  txInInfoOutRef i == oref) $  txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = any (\(_,tn'',i) -> i == 1 && tn'' == tn) . flattenValue $ (txInfoMint info) 

-- nftPolicy ::  MintingPolicy
-- nftPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkNFTPolicy ||])


{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: TokenName -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy tn _ ctx = check $ mkNFTPolicy tn () (PlutusTx.unsafeFromBuiltinData ctx)
  where
    -- oref ::  TxOutRef
    -- oref =  TxOutRef
    --     ( PlutusTx.unsafeFromBuiltinData tid)
    --     (PlutusTx.unsafeFromBuiltinData ix)

    -- tn ::  TokenName
    -- tn = PlutusTx.unsafeFromBuiltinData tn'


