{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import           Data.Text             (pack)
import           GeniusYield.GYConfig
import           GeniusYield.Imports   (IsString (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types     (GYMintingPolicyId,
                                        GYProviders (gySubmitTx), GYTokenName,
                                        GYTxOutRef, GYUTxO (utxoRef),
                                        addressFromPubKeyHash,
                                        gyQueryUtxosAtAddresses,
                                        paymentVerificationKey, pubKeyHash,
                                        readPaymentSigningKey, signGYTxBody,
                                        tokenNameFromHex, utxosLookup)
import           Oracle.Api
import           System.Environment    (getArgs)
import           Text.Printf           (printf)

-- | Getting path for our core configuration and the beneficiary.
parseArgs :: IO (FilePath, FilePath, GYTxOutRef, GYMintingPolicyId, GYTokenName, Integer)
parseArgs = do
    args <- getArgs
    case args of
        [coreCfgFile, skeyFile, oref, cs, tn, rate] -> do
            let tokenName = tokenNameFromHex $ pack tn
                cs' = fromString cs
                rate' = read rate :: Integer
            case tokenName of
                Right tn' -> return (coreCfgFile, skeyFile, fromString oref, cs', tn', rate')
                Left _ -> fail ("error %s" :: String)
        _invalidArgument                                 -> fail
            "Error: wrong arguments, needed the configuration file, the sender skey file, the beneficiary address \n"

main :: IO ()
main = do
    (coreCfgFile, skeyFile, oref, cs, tn, rate) <- parseArgs
    coreCfg <- coreConfigIO coreCfgFile
    skey    <- readPaymentSigningKey skeyFile
    let nid    = cfgNetworkId coreCfg
        sender = addressFromPubKeyHash nid $ pubKeyHash $ paymentVerificationKey skey
    printf "sender %s \n" sender
    withCfgProviders coreCfg "oracle" $ \providers -> do
        utxos <- gyQueryUtxosAtAddresses providers [sender]
        let utxoM = utxosLookup oref utxos
        case utxoM of
            Just utxo -> do
                    txBody <- runGYTxMonadNode nid providers [sender] sender Nothing (return $ setupOracle nid cs tn sender (utxoRef utxo) rate)
                    tid    <- gySubmitTx providers $ signGYTxBody txBody [skey]
                    printf "submitted tx: %s\n" tid
            Nothing ->
                    printf "No utxo /n"

