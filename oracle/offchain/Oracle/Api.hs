module Oracle.Api
    ( setupOracle,
    updateOracle,
    oracleAddress,
    deleteOracle
    ) where

import           GeniusYield.TxBuilder
import           GeniusYield.Types

import           Data.Maybe            (fromJust)
import           Oracle.OnChain.Oracle (OracleDatum (..),
                                        OracleRedeemer (Delete, Update))
import           Oracle.Script         (oracleScriptValidator)

setupOracle ::
  -- | TokenName generated with combination of IPFS hash
  GYNetworkId ->
  GYMintingPolicyId ->
  GYTokenName ->
  GYAddress ->
  GYTxOutRef ->
  Integer ->
  GYTxSkeleton 'PlutusV2
setupOracle nid cs tn minter utxo rate = do
  let pkh = fromJust $ addressToPubKeyHash minter
      orcVld = oracleScriptValidator cs tn pkh
      oracleAddr = addressFromValidator nid orcVld
      output = GYTxOut oracleAddr nftValue (Just (datumFromPlutusData datum, GYTxOutUseInlineDatum)) Nothing
      -- lovelace
      datum = OracleDatum {
        rate = rate
      }
      nftValue =  valueSingleton (GYToken cs tn) 1
      input = GYTxIn {
        gyTxInTxOutRef = utxo,
        gyTxInWitness = GYTxInWitnessKey
      }

  mustHaveInput input
        <> mustHaveOutput output

updateOracle ::
  -- | TokenName generated with combination of IPFS hash
  GYNetworkId ->
  GYMintingPolicyId ->
  GYTokenName ->
  GYAddress ->
  GYTxOutRef ->
  Integer ->
  Integer ->
  GYTxSkeleton 'PlutusV2
updateOracle nid cs tn minter utxo rate oldRate= do
  let pkh = fromJust $ addressToPubKeyHash minter
      orcVld = oracleScriptValidator cs tn pkh
      oracleAddr = addressFromValidator nid orcVld
      output = GYTxOut oracleAddr nftValue (Just (datumFromPlutusData datum, GYTxOutUseInlineDatum)) Nothing
      -- lovelace
      datum = OracleDatum {
        rate = rate
      }
      -- lovelace
      datumOld = OracleDatum {
        rate = oldRate
      }

      redeemer = Update

      nftValue =  valueSingleton (GYToken cs tn) 1
      input = GYTxIn {
        gyTxInTxOutRef = utxo
        , gyTxInWitness  = GYTxInWitnessScript
                (GYInScript $ oracleScriptValidator cs tn pkh)
                (datumFromPlutusData datumOld) (redeemerFromPlutusData redeemer)
      }

  mustBeSignedBy (fromJust $ addressToPubKeyHash minter)
    <> mustHaveOutput output
    <> mustHaveInput input

oracleAddress :: GYNetworkId -> GYMintingPolicyId -> GYTokenName -> GYAddress -> GYAddress
oracleAddress nid cs tn minter = do
  let pkh = fromJust $ addressToPubKeyHash minter
      orcVld = oracleScriptValidator cs tn pkh
      oracleAddress' = addressFromValidator nid orcVld
  oracleAddress'


deleteOracle ::
  GYMintingPolicyId ->
  GYTokenName ->
  GYAddress ->
  GYTxOutRef ->
  Integer ->
  GYTxSkeleton 'PlutusV2
deleteOracle cs tn minter utxo oldRate = do
  let pkh = fromJust $ addressToPubKeyHash minter
      output = GYTxOut minter nftValue Nothing Nothing
      -- lovelace
      datumOld = OracleDatum {
        rate = oldRate
      }

      redeemer = Delete

      nftValue =  valueSingleton (GYToken cs tn) 1
      input = GYTxIn {
        gyTxInTxOutRef = utxo
        , gyTxInWitness  = GYTxInWitnessScript
                (GYInScript $ oracleScriptValidator cs tn pkh)
                (datumFromPlutusData datumOld) (redeemerFromPlutusData redeemer)
      }

  mustBeSignedBy (fromJust $ addressToPubKeyHash minter)
    <> mustHaveOutput output
    <> mustHaveInput input
