module Oracle.Api.Api where

import           Oracle.Api.Oracle   (OracleApi, handleOracleApi)
-- import           Oracle.Api.Tx       (TxAPI, handleTx)
import           Data.Swagger
import           GeniusYield.Imports
import           Servant
import           Servant.Swagger

-- | Type for our Servant API.
type Api =
  "oracle" :>  OracleApi
  --       "tx"  :> TxAPI
  -- :<|>  

appApi :: Proxy Api
appApi = Proxy

apiSwagger  :: Swagger
apiSwagger  = toSwagger appApi

apiServer :: Ctx -> ServerT Api IO
apiServer = handleOracleApi

  --      handleTx ctx
  -- :<|> 