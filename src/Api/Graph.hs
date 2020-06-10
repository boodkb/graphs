{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
module Api.Graph
  ( Routes(..)
  , proxy
  , Node(..)
  , NodeLabel(..)
  , Created(..)
  , Nodes(..)
  )
where

import Data.Aeson
import Data.Int (Int64)
import Data.Swagger (ToSchema)
import Data.Text
import Data.Vector
import Servant
import Servant.API.Generic


-- | Represents a JSON object with a list of nodes
data Nodes = Nodes
    { nodes :: Vector Node
    } deriving (Generic, Show, ToJSON, ToSchema)

data Node = Node
  { id :: Int64
  , label :: Text
  } deriving (Generic, Show, ToJSON, ToSchema)

-- | Represents a JSON object with a node label
newtype NodeLabel = NodeLabel
  { label :: Text
  } deriving (Generic, Show, FromJSON, ToSchema)

-- | Represents a JSON object with a node id
newtype Created = Created
  { id :: Int64
  } deriving (Generic, Show, ToJSON, ToSchema)

type CreateNode
  = Summary "Create a new node"
  :> "graph"
  :> "node"
  :> ReqBody' '[Description "An object with a label"] '[JSON] NodeLabel
  :> Put '[JSON] Created

type DeleteNode
  = Summary "Delete an existing node"
  :> "graph"
  :> "node"
  :> Capture' '[Description "Id of the node to delete"] "id" Int64
  :> DeleteNoContent '[JSON] NoContent

type RenameNode
  = Summary "Rename an existing node"
  :> "graph"
  :> "node"
  :> Capture' '[Description "Id of the node to rename "] "id" Int64
  :> "label"
  :> ReqBody' '[Description "An object with a new label"] '[JSON] NodeLabel
  :> PutNoContent '[JSON] NoContent

type GetAllNodes
  = Summary "Get all nodes"
  :> "graph"
  :> "node"
  :> Get '[JSON] Nodes

type GetNeighbours
  = Summary "Get all neighbours of the specified node"
  :> "graph"
  :> "node"
  :> Capture' '[Description "Id of the node whose neighbours to find"] "id" Int64
  :> "neighbours"
  :> Get '[JSON] Nodes

type LinkNodes
  = Summary "Link two specified nodes"
  :> "graph"
  :> "link"
  :> Capture' '[Description "Id of the 'from' node"] "id_from" Int64
  :> Capture' '[Description "Id of the 'to' node"] "id_to" Int64
  :> PutNoContent '[JSON] NoContent

data Routes route = Routes
  { createNode :: route :- CreateNode
  , deleteNode :: route :- DeleteNode
  , renameNode :: route :- RenameNode
  , getAllNodes :: route :- GetAllNodes
  , getNeighbours :: route :- GetNeighbours
  , linkNodes :: route :- LinkNodes
  } deriving (Generic)

proxy :: Proxy (ToServantApi Routes)
proxy = genericApi (Proxy @Routes)

