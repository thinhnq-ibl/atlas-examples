module NFT.Api
    ( mintNFTToken
    ) where

import           GeniusYield.TxBuilder
import           GeniusYield.Types

import           NFT.Script            (nftValidator)

mintNFTToken ::
  -- | TokenName generated with combination of IPFS hash
  GYTokenName ->
  GYAddress ->
  GYTxOutRef ->
  GYTxSkeleton 'PlutusV2
mintNFTToken tn minter utxo = do
  let nftPolicy = nftValidator utxo tn
      nftMintingPolicyId = mintingPolicyId nftPolicy
      output = GYTxOut minter nftValue Nothing Nothing
      nftValue = valueSingleton (GYToken nftMintingPolicyId tn) 1
      input = GYTxIn {
        gyTxInTxOutRef = utxo,
        gyTxInWitness = GYTxInWitnessKey
      }

  mustHaveInput input
        <> mustMint (GYMintScript nftPolicy) unitRedeemer tn 1
        <> mustHaveOutput output
