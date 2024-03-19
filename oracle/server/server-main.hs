import           Control.Exception           (try)
import           Control.Monad.Trans.Except
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant
import           System.Environment          (getArgs)

import           GeniusYield.GYConfig

import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy.Char8  as BL8
import           GeniusYield.Types
import           Oracle.Api.Api
import           Oracle.Api.Context
import           Prelude                     hiding (read)

-- | Getting path for our core configuration and the beneficiary.
parseArgs :: IO (FilePath, FilePath)
parseArgs = do
    args <- getArgs
    case args of
        [coreCfgFile, skeyFile] -> return (coreCfgFile, skeyFile)
        _invalidArgument                                 -> fail
            "Error: wrong arguments, needed the configuration file, the sender skey file, the beneficiary address \n"

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)

  putStrLn "parsing Config ..."
  (coreCfgPath, skeyFile) <- parseArgs
  coreCfg <- coreConfigIO coreCfgPath  -- Parsing our core configuration.
  skey    <- readPaymentSigningKey skeyFile
  let nid    = cfgNetworkId coreCfg
      sender = addressFromPubKeyHash nid $ pubKeyHash $ paymentVerificationKey skey

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server"  $ \providers -> do
    let port = 8081
        ctx = Ctx coreCfg providers skey sender
    putStrLn $ "Starting server at \n " <> "http://localhost:" <> show port
    run port $ app ctx


app :: Ctx -> Application
app ctx = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $ serve appApi $ hoistServer appApi (Handler . ExceptT . try)  $ apiServer ctx
