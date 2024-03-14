module Oracle.Api
    ( setupOracle
    ) where

import           GeniusYield.TxBuilder
import           GeniusYield.Types

import           Data.Maybe            (fromJust)
import           Oracle.OnChain.Oracle (OracleDatum (..))
import           Oracle.Script         (oracleScriptValidator)

setupOracle ::
  -- | TokenName generated with combination of IPFS hash
  GYNetworkId ->
  GYMintingPolicyId ->
  GYTokenName ->
  GYAddress ->
  GYTxOutRef ->
  GYTxSkeleton 'PlutusV2
setupOracle nid cs tn minter utxo = do
  let pkh = fromJust $ addressToPubKeyHash minter
      orcVld = oracleScriptValidator cs tn pkh
      oracleAddress = addressFromValidator nid orcVld
      output = GYTxOut oracleAddress nftValue (Just (datumFromPlutusData datum, GYTxOutUseInlineDatum)) Nothing
      datum = OracleDatum {
        rate = 2000000
      }
      nftValue = valueFromLovelace $ toInteger (2000000 :: Integer)
      input = GYTxIn {
        gyTxInTxOutRef = utxo,
        gyTxInWitness = GYTxInWitnessKey
      }

  mustHaveInput input
        <> mustHaveOutput output
