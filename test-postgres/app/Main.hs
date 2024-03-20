{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Database.PostgreSQL.Simple
import           Lib
import           Text.Printf                (printf)

main :: IO ()
main = do
    someFunc
    let conString = "postgresql://postgres:changeme@192.168.1.3:5432/postgres";
    conn <- connectPostgreSQL conString
    [Only i] <- query_ conn "select 2 + 2"
    printf i
