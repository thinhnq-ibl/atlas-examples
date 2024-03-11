module NFT.Script
    ( nftValidator
    ) where

import           GeniusYield.Types
import qualified NFT.OnChain.NFT.Compiled as OnChain

nftValidator :: GYTokenName -> GYMintingPolicy 'PlutusV2
nftValidator = mintingPolicyFromPlutus . OnChain.nftPolicy . tokenNameToPlutus
