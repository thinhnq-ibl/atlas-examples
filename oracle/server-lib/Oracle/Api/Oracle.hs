module Oracle.Api.Oracle where

import qualified Data.Swagger          as Swagger
import           GeniusYield.GYConfig
import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Oracle.Api
import           Oracle.Api.Context
import           Servant

-- | Input parameters for init oracle.
data InitOracleParams = InitOracleParams {
    oref :: GYTxOutRef,
    cs   :: GYMintingPolicyId,
    tn   :: GYTokenName,
    rate :: !Integer
  }
  deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data UpdateOracleParams = UpdateOracleParams {
    orefu    :: GYTxOutRef,
    csu      :: GYMintingPolicyId,
    tnu      :: GYTokenName,
    rateu    :: !Integer,
    oldrateu :: !Integer
  }
  deriving (Show, Generic, FromJSON, Swagger.ToSchema)

data DeleteOracleParams = DeleteOracleParams {
    orefd    :: GYTxOutRef,
    csd      :: GYMintingPolicyId,
    tnd      :: GYTokenName,
    oldrated :: !Integer
  }
  deriving (Show, Generic, FromJSON, Swagger.ToSchema)

-- | Type for our Servant API.
type OracleApi =
       "init"
    :> ReqBody '[JSON] InitOracleParams
    :> Post    '[JSON] GYTxId
  :<|> "update"
    :> ReqBody '[JSON] UpdateOracleParams
    :> Post    '[JSON] GYTxId
  :<|> "delete"
    :> ReqBody '[JSON] DeleteOracleParams
    :> Post    '[JSON] GYTxId

-- | Serving our API.
handleOracleApi :: Ctx -> ServerT OracleApi IO
handleOracleApi ctx = handleInit ctx
            :<|> handleUpdate ctx
            :<|> handleDelete ctx

-- | Handle for init oracle.
handleInit :: Ctx -> InitOracleParams -> IO GYTxId
handleInit ctx InitOracleParams{..} = do
  let nid    = cfgNetworkId $ ctxCoreCfg ctx
      sender = ctxSender ctx
      skey = ctxSkey ctx
      providers = ctxProviders ctx
  txBody <- runGYTxMonadNode nid providers [sender] sender Nothing (return $ setupOracle nid cs tn sender oref rate)
  gySubmitTx providers $ signGYTxBody txBody [skey]

-- | Handle for update oracle.
handleUpdate :: Ctx -> UpdateOracleParams -> IO GYTxId
handleUpdate ctx UpdateOracleParams{..} = do
  let nid    = cfgNetworkId $ ctxCoreCfg ctx
      sender = ctxSender ctx
      skey = ctxSkey ctx
      providers = ctxProviders ctx
  txBody <- runGYTxMonadNode nid providers [sender] sender Nothing (return $ updateOracle nid csu tnu sender orefu oldrateu rateu)
  gySubmitTx providers $ signGYTxBody txBody [skey]

-- | Handle for update oracle.
handleDelete :: Ctx -> DeleteOracleParams -> IO GYTxId
handleDelete ctx DeleteOracleParams{..} = do
  let nid    = cfgNetworkId $ ctxCoreCfg ctx
      sender = ctxSender ctx
      skey = ctxSkey ctx
      providers = ctxProviders ctx
  txBody <- runGYTxMonadNode nid providers [sender] sender Nothing (return $ deleteOracle csd tnd sender orefd oldrated)
  gySubmitTx providers $ signGYTxBody txBody [skey]
