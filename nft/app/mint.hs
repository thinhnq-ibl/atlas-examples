import           Data.Text             (pack)
import           GeniusYield.GYConfig
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           System.Environment    (getArgs)
import           Text.Printf           (printf)
import           NFT.Api

-- | Getting path for our core configuration and the beneficiary.
parseArgs :: IO (FilePath, FilePath, GYTokenName)
parseArgs = do
    args <- getArgs
    case args of
        [coreCfgFile, skeyFile, tn] -> do
            let tokenName = tokenNameFromHex $ pack tn
            case tokenName of
                Right tn' -> return (coreCfgFile, skeyFile, tn')
                Left _ -> fail ("error %s" :: String)
        _invalidArgument                                 -> fail
            "Error: wrong arguments, needed the configuration file, the sender skey file, the beneficiary address, the deadline and the amount\n"

main :: IO ()
main = do
    (coreCfgFile, skeyFile, tn) <- parseArgs
    -- printf "configuration file: %s\nsender skey file: %s\n token name: %s\ndeadline: %s\namount: %d\n" coreCfgFile skeyFile tn
    coreCfg <- coreConfigIO coreCfgFile
    skey    <- readPaymentSigningKey skeyFile
    let nid    = cfgNetworkId coreCfg
        sender = addressFromPubKeyHash nid $ pubKeyHash $ paymentVerificationKey skey
    withCfgProviders coreCfg "place-vesting" $ \providers -> do
        txBody <- runGYTxMonadNode nid providers [sender] sender Nothing $ mintNFTToken tn
        tid    <- gySubmitTx providers $ signGYTxBody txBody [skey]
        printf "submitted tx: %s\n" tid

