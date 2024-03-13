{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Oracle.OnChain.Oracle (mkWrappedValidator) where
import PlutusTx.Prelude (Integer, Bool, Maybe (..), (&&), traceError, ($), Eq ((==)), isJust, check)
import qualified PlutusTx
import PlutusLedgerApi.V3
    ( TxOut,
      TxInfo,
      PubKeyHash,
      OutputDatum(..),
      Datum(..),
      ScriptContext,
      BuiltinData,
      txOutDatum,
      ScriptContext(..),
      TxInInfo(..),
      TxOut(..),
      CurrencySymbol,
      TokenName )
import PlutusLedgerApi.V1.Value
    ( AssetClass, assetClassValueOf, AssetClass(..) )
import Prelude (Show)
import PlutusLedgerApi.V2.Contexts(findDatum, findOwnInput, txSignedBy, getContinuingOutputs)
import PlutusTx.Trace (traceIfFalse)
import PlutusLedgerApi.V2 (UnsafeFromData(..))

-- Oracle Datum
newtype OracleDatum = OracleDatum {rate :: Integer}

PlutusTx.unstableMakeIsData ''OracleDatum
---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------
{-# INLINABLE parseOracleDatum #-}
parseOracleDatum :: TxOut -> TxInfo -> Maybe OracleDatum
parseOracleDatum o info =
  case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
    OutputDatumHash dh -> do
      Datum d <- findDatum dh info
      PlutusTx.fromBuiltinData d

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------
data OracleParams =
  OracleParams
    { oNFT      :: AssetClass
    , oOperator :: PubKeyHash
    }
  deriving (Show)

PlutusTx.makeLift ''OracleParams

data OracleRedeemer
  = Update
  | Delete
  deriving (Prelude.Show)

PlutusTx.unstableMakeIsData ''OracleRedeemer



{-# INLINABLE mkValidator #-}
mkValidator ::
     OracleParams -> OracleDatum -> OracleRedeemer -> ScriptContext -> Bool
mkValidator oracle _ r ctx =
  case r of
    Update ->
      traceIfFalse "token missing from input" inputHasToken &&
      traceIfFalse "token missing from output" outputHasToken &&
      traceIfFalse "operator signature missing" checkOperatorSignature &&
      traceIfFalse "invalid output datum" validOutputDatum
    Delete -> traceIfFalse "operator signature missing" checkOperatorSignature
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    -- | Check that the 'oracle' is signed by the 'oOperator'.
    checkOperatorSignature :: Bool
    checkOperatorSignature = txSignedBy info $ oOperator oracle
    -- | Find the oracle input.
    ownInput :: TxOut
    ownInput =
      case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i
    -- Check that the oracle input contains the NFT.
    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oNFT oracle) == 1
    -- | Find the oracle output.
    ownOutput :: TxOut
    ownOutput =
      case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"
    -- Check that the oracle output contains the NFT.
    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oNFT oracle) == 1
    -- Check that the oracle output contains a valid datum.
    validOutputDatum :: Bool
    validOutputDatum = isJust $ parseOracleDatum ownOutput info


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator ::
     CurrencySymbol
  -> TokenName
  -> PubKeyHash
  -> BuiltinData
  -> BuiltinData
  -> BuiltinData
  -> ()
mkWrappedValidator cs tn pkh datum redeemer ctx= check $ mkValidator op (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) (unsafeFromBuiltinData ctx)
  where
    op =
      OracleParams
        { oNFT = AssetClass ( cs, tn)
        , oOperator = pkh
        }
