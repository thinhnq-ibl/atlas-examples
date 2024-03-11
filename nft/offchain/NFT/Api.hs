module NFT.Api
    ( mintNFTToken
    ) where

import           GeniusYield.TxBuilder
import           GeniusYield.Types

import           NFT.Script        (nftValidator)

mintNFTToken ::
  -- | TokenName generated with combination of IPFS hash
  GYTokenName ->
  GYTxSkeleton 'PlutusV2
mintNFTToken tn =
  let nftPolicy = nftValidator tn

  in mustMint (GYMintScript nftPolicy) unitRedeemer tn 1

-- vestingAddress :: GYTxQueryMonad m => GYPubKeyHash -> m GYAddress
-- vestingAddress = scriptAddress . vestingValidator

-- placeVesting :: GYTxQueryMonad m => GYPubKeyHash -> GYTime -> GYValue -> m (GYTxSkeleton 'PlutusV2)
-- placeVesting beneficiary deadline value = do
--     addr <- vestingAddress beneficiary
--     return $ mustHaveOutput $ GYTxOut
--         { gyTxOutAddress = addr
--         , gyTxOutValue   = value
--         , gyTxOutDatum   = Just (datumFromPlutusData $ timeToPlutus deadline, GYTxOutUseInlineDatum)
--         , gyTxOutRefS    = Nothing
--         }

-- availableVestings :: GYTxQueryMonad m => GYPubKeyHash -> m [(GYTxOutRef, GYTime)]
-- availableVestings beneficiary = do
--     slot   <- slotOfCurrentBlock
--     now    <- slotToBeginTime slot
--     addr   <- vestingAddress beneficiary
--     utxos  <- utxosAtAddress addr Nothing
--     utxos' <- utxosDatums utxos
--     return
--         [ (oref, deadline')
--         | (oref, (_, _, deadline)) <- Map.toList utxos'
--         , let deadline' = timeFromPlutus deadline
--         , now > deadline'
--         ]

-- retrieveVestings :: GYTxQueryMonad m => GYPubKeyHash -> [(GYTxOutRef, GYTime)] -> m (GYTxSkeleton 'PlutusV2)
-- retrieveVestings beneficiary orefs = do
--     slot <- slotOfCurrentBlock
--     return $ isInvalidBefore slot <> mustBeSignedBy beneficiary <> mconcat
--         [ mustHaveInput GYTxIn
--             { gyTxInTxOutRef = oref
--             , gyTxInWitness  = GYTxInWitnessScript
--                 (GYInScript $ vestingValidator beneficiary)
--                 (datumFromPlutusData $ timeToPlutus deadline) unitRedeemer
--             }
--         | (oref, deadline) <- orefs
--         ]
