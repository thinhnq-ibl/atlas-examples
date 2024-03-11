module NFT.Api
    ( mintNFTToken
    ) where

import           GeniusYield.TxBuilder
import           GeniusYield.Types

import           NFT.Script        (nftValidator)

mintNFTToken ::
  GYTxQueryMonad m =>
  -- | TokenName generated with combination of IPFS hash
  GYTokenName ->
  m (GYTxSkeleton 'PlutusV2)
mintNFTToken tn = do
  let nftPolicy = nftValidator tn
  return $ mustMint (GYMintScript nftPolicy) unitRedeemer tn 1