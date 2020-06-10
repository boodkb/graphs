module Db.Sessions
  ( getAllNodes
  , deleteNode
  , createNode
  , renameNode
  )
where



import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Db.Statements as S
import Hasql.Session

-- | Gets all nodes of the graph
-- | The result is a vector of @(nodeId, label)@ pairs
getAllNodes :: Session (Vector (Int64, Text))
getAllNodes = statement () S.getAllNodes

-- | Deletes a node of the graph
-- | Returns False if the node did not exist.
-- | Otherwise returns True.
deleteNode :: Int64 -> Session Bool
deleteNode nodeId = statement nodeId S.deleteNode

-- | Creates a new node
-- | Returns id of the created node
createNode :: Text -> Session Int64
createNode label = statement label S.insertNode

-- | Updates the label of a node
-- | Returns False if the node does not exist.
-- | Otherwise returns True.
renameNode :: Int64 -> Text -> Session Bool
renameNode nodeId label = statement (nodeId, label) S.renameNode
