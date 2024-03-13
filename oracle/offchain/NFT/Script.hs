module NFT.Script
    ( nftValidator
    ) where

import           GeniusYield.Types
import qualified NFT.OnChain.NFT.Compiled as OnChain

nftValidator :: GYTxOutRef -> GYTokenName -> GYMintingPolicy 'PlutusV2
nftValidator oref tn = mintingPolicyFromPlutus $ OnChain.nftPolicy (txOutRefToPlutus oref) (tokenNameToPlutus tn)
