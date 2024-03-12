{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.Text             (pack)
import           GeniusYield.GYConfig
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           System.Environment    (getArgs)
import           Text.Printf           (printf)
import           NFT.Api
import Control.Arrow (Arrow(first))
import qualified Data.Maybe

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
            "Error: wrong arguments, needed the configuration file, the sender skey file, the beneficiary address \n"

main :: IO ()
main = do
    (coreCfgFile, skeyFile, tn) <- parseArgs
    coreCfg <- coreConfigIO coreCfgFile
    skey    <- readPaymentSigningKey skeyFile
    let nid    = cfgNetworkId coreCfg
        sender = addressFromPubKeyHash nid $ pubKeyHash $ paymentVerificationKey skey
    printf "sender %s \n" sender
    withCfgProviders coreCfg "place-vesting" $ \providers -> do
        utxos <- gyQueryUtxosAtAddresses providers [sender]
        utxoM <- randomTxOutRef utxos
        case utxoM of
            Just utxo -> do
                    txBody <- runGYTxMonadNode nid providers [sender] sender Nothing (return $ mintNFTToken tn sender (fst utxo))
                    tid    <- gySubmitTx providers $ signGYTxBody txBody [skey]
                    printf "submitted tx: %s\n" tid
            Nothing -> 
                    printf "No utxo /n"

