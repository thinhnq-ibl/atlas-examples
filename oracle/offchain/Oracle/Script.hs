module Oracle.Script
    ( oracleScriptValidator
    ) where

import           GeniusYield.Types
import qualified Oracle.OnChain.Oracle.Compiled as OnChain

oracleScriptValidator :: GYMintingPolicyId -> GYTokenName -> GYPubKeyHash -> GYValidator 'PlutusV2
oracleScriptValidator cs tn pkh= validatorFromPlutus $ OnChain.oracleValidator (mintingPolicyIdCurrencySymbol cs) (tokenNameToPlutus tn) (pubKeyHashToPlutus pkh)
