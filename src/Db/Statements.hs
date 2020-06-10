{-# LANGUAGE OverloadedStrings #-}
module Db.Statements
  ( getAllNodes
  , getNeighbours
  , insertNode
  , renameNode
  , deleteNode
  , nodeExists
  , insertEdge
  , edgeExists
  )
where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text
import Data.Vector
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Hasql.Statement (Statement(..))

-- | A statement for getting all nodes of the graph
getAllNodes :: Statement () (Vector (Int64, Text))
getAllNodes = Statement sql E.noParams decoder True
  where
    sql     = "select id, label from graphs.node"
    decoder = D.rowVector nodeDecoder

-- | A statement for getting all neighbours of the node
getNeighbours :: Statement Int64 (Vector (Int64, Text))
getNeighbours = Statement sql encoder decoder True
  where
    sql
      = "select * from graphs.node \
        \where id in \
        \(select id_from from graphs.edge \
        \where id_to = $1 \
        \union all \
        \select id_to from graphs.edge \
        \where id_from = $1 \
        \)"
    encoder = E.param (E.nonNullable E.int8)
    decoder = D.rowVector nodeDecoder

-- | A statement for inserting a new node
insertNode :: Statement Text Int64
insertNode = Statement sql encoder decoder True
  where
    sql     = "insert into graphs.node (label) values ($1) returning id"
    encoder = E.param (E.nonNullable E.text)
    decoder = D.singleRow (D.column (D.nonNullable D.int8))

-- | A statement for updating the label of an exiting node
renameNode :: Statement (Int64, Text) Bool
renameNode = Statement sql encoder decoder True
  where
    sql = "update graphs.node set label = $2 where id = $1"
    encoder =
      (fst >$< E.param (E.nonNullable E.int8))
        <> (snd >$< E.param (E.nonNullable E.text))
    decoder = (fmap (> 0) D.rowsAffected)

-- | A statement for deleting of an exiting node
deleteNode :: Statement Int64 Bool
deleteNode = Statement sql encoder decoder True
  where
    sql     = "delete from graphs.node where id = $1"
    encoder = E.param (E.nonNullable E.int8)
    decoder = (fmap (> 0) D.rowsAffected)

-- | A statement for checking existense of a node
nodeExists :: Statement Int64 Bool
nodeExists = Statement sql encoder decoder True
  where
    sql     = "select from graphs.node where id = $1"
    encoder = E.param (E.nonNullable E.int8)
    decoder = isJust <$> D.rowMaybe (pure ())

-- | A statement for checking existense of an edge
edgeExists :: Statement (Int64, Int64) Bool
edgeExists = Statement sql encoder decoder True
  where
    sql     = "select from graphs.edge \
              \where (id_from = $1 and id_to = $2) \
              \or (id_from = $2 and id_to = $1)"
    encoder =
      (fst >$< E.param (E.nonNullable E.int8))
        <> (snd >$< E.param (E.nonNullable E.int8))
    decoder = isJust <$> D.rowMaybe (pure ())

-- | A statement for inserting a new edge
insertEdge :: Statement (Int64, Int64) ()
insertEdge = Statement sql encoder D.noResult True
  where
    sql = "insert into graphs.edge values ($1, $2)"
    encoder =
      (fst >$< E.param (E.nonNullable E.int8))
        <> (snd >$< E.param (E.nonNullable E.int8))

nodeDecoder :: D.Row (Int64, Text)
nodeDecoder =
  (,)
    <$> (D.column . D.nonNullable) D.int8
    <*> (D.column . D.nonNullable) D.text
