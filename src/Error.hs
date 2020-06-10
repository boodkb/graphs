module Error
  ( dbError
  , nodeNotFound
  )
where

import Data.ByteString.Lazy.Char8 as BS
import Data.Int (Int64)
import Servant
import Hasql.Pool (UsageError)

nodeNotFound :: Int64 -> ServerError
nodeNotFound nodeId = err404 { errBody = BS.pack msg }
  where
    msg = "Node with id " <> show nodeId <> " was not found"

dbError :: UsageError -> ServerError
dbError err = err500
