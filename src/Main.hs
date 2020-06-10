{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import Control.Exception (bracket)
import qualified Hasql.Connection as Connection
import qualified Hasql.Pool as Pool
import qualified Server

main :: IO ()
main = bracket (Pool.acquire poolSettings) Pool.release (Server.run port)
  where
    port         = 8888
    poolSettings = (5, 1.0, pgSettings)
    pgSettings =
      Connection.settings "localhost" 5432 "postgres" "example" "postgres"
