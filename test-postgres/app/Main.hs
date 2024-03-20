{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Database.PostgreSQL.Simple
import           Lib
import           Text.Printf                (printf)
import qualified Data.Text as Text

main :: IO ()
main = do
    someFunc
    let conString = "postgresql://postgres:changeme@192.168.1.3:5432/oracle";
    conn <- connectPostgreSQL conString
    xs <- query_ conn "select id, usd, vnd from price"
    forM_ xs $ \(id, usd, vnd) ->
        putStrLn $ Text.unpack usd ++ " is " ++ show (vnd :: Int)
