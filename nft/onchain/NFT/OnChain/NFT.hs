{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module NFT.OnChain.NFT (mkWrappedNFTPolicy) where
import           PlutusLedgerApi.V1.Value
import           PlutusLedgerApi.V3       (ScriptContext (..), TxId,
                                           TxInInfo (..), TxInfo (..),
                                           TxOutRef (..), unsafeFromBuiltinData)
import           PlutusTx.Prelude         (Bool, BuiltinData, Eq ((==)),
                                           Integer, any, check, traceIfFalse,
                                           ($), (&&), (.))


{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy oref tn _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info =  scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO =  any (\i ->  txInInfoOutRef i == oref) $  txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = any (\(_,tn'',i) -> i == 1 && tn'' == tn) . flattenValue $ txInfoMint info

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: TxOutRef -> TokenName -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy oref tn _ ctx = check $ mkNFTPolicy oref tn () (unsafeFromBuiltinData ctx)


