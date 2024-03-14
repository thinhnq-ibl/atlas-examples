module Oracle.Script
    ( oracleValidator
    ) where

import           GeniusYield.Types
import qualified Oracle.OnChain.Oracle.Compiled as OnChain

oracleValidator :: GYTxOutRef -> GYTokenName -> GYMintingPolicy 'PlutusV2
oracelValidator oref tn = mintingPolicyFromPlutus $ OnChain.oracleValidator (txOutRefToPlutus oref) (tokenNameToPlutus tn)
