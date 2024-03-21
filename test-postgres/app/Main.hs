{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Database.PostgreSQL.Simple
import           Lib
import Control.Monad
import Data.Int

main :: IO ()
main = do
    someFunc
    let conString = "postgresql://postgres:changeme@192.168.1.3:5432/oracle";
    conn <- connectPostgreSQL conString
    xs :: [(Int64, Double, Int64)] <- query_ conn "select id, usd, vnd from price"
    forM_ xs $ \(id, usd, vnd) ->
        putStrLn $ show id ++ " " ++ show usd ++ " " ++ show vnd 
