module Oracle.Api.Api where

import           Data.Swagger
import           GeniusYield.Imports
import           Oracle.Api.Context
import           Oracle.Api.Oracle   (OracleApi, handleOracleApi)
import           Servant
import           Servant.Swagger

-- | Type for our Servant API.
type Api =
  "oracle" :>  OracleApi

appApi :: Proxy Api
appApi = Proxy

apiSwagger  :: Swagger
apiSwagger  = toSwagger appApi

apiServer :: Ctx -> ServerT Api IO
apiServer = handleOracleApi
