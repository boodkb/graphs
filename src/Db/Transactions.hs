module Db.Transactions
  ( getNeighbours
  , linkNodes
  )
where


import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Db.Statements as S
import Hasql.Session (Session)
import Hasql.Transaction (statement)
import Hasql.Transaction.Sessions


-- | Gets all neighbours of a node
-- | Returns Nothing if the node did not exist.
-- | Otherwise returns a vector of @(nodeId, label)@ pairs.
getNeighbours :: Int64 -> Session (Maybe (Vector (Int64, Text)))
getNeighbours nodeId = transaction RepeatableRead Read $ do
    nodeExists <- statement nodeId S.nodeExists
    if nodeExists
      then Just <$> statement nodeId S.getNeighbours
      else pure Nothing

-- | Links two existing nodes of the graph
-- | Returns @Left nodeId@ if at least one of the specified nodes does not exist.
-- | Otherwise returns @Right ()@
linkNodes :: Int64 -> Int64 -> Session (Either Int64 ())
linkNodes nodeFrom nodeTo = transaction Serializable Write $ runExceptT $ do
    nodeFromExists <- lift $ statement nodeFrom S.nodeExists
    unless nodeFromExists $ throwE nodeFrom
    nodeToExists <- lift $ statement nodeTo S.nodeExists
    unless nodeToExists $ throwE nodeTo
    edgeExists <- lift $ statement (nodeFrom, nodeTo) S.edgeExists
    unless edgeExists $ lift $ statement (nodeFrom, nodeTo) S.insertEdge

