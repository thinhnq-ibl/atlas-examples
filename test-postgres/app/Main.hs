{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main (main) where

import           Database.PostgreSQL.Simple
import           Lib
import Control.Monad
import GHC.Generics
import Prelude hiding (id)

data Oracle = Oracle {
    id :: Int,
    usd :: Double,
    vnd :: Int
    }
  deriving (Show, Generic)           -- Derive Generic instance,
  deriving anyclass (ToRow, FromRow) -- used for these instances

main :: IO ()
main = do
    someFunc
    let conString = "postgresql://postgres:changeme@192.168.1.3:5432/oracle";
    conn <- connectPostgreSQL conString
    let newOracle = Oracle {
        id = 3,
        usd = 5.5,
        vnd = 150000
    }
    insert1 <-
        execute
            conn
            "insert into price (id, usd, vnd) values (?, ?, ?)"
            newOracle
    print insert1
    xs :: [Oracle] <- query_ conn "select id, usd, vnd from price"
    forM_ xs $ \x ->
        print x
