{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Server.Graph
  ( server
  )
where

import qualified Api.Graph as Api
import Control.Monad.IO.Class (liftIO)
import qualified Db
import Data.Int (Int64)
import Error (dbError, nodeNotFound)
import Hasql.Pool (Pool, use)
import Servant
import Servant.Server.Generic (AsServer)


server :: Pool -> Api.Routes AsServer
server pool = Api.Routes { .. }
  where
    createNode :: Api.NodeLabel -> Handler Api.Created
    createNode (Api.NodeLabel label) = do
      mbRes <- liftIO $ use pool (Db.createNode label)
      case mbRes of
        Left  err    -> throwError (dbError err)
        Right nodeId -> pure $ Api.Created nodeId

    deleteNode :: Int64 -> Handler NoContent
    deleteNode nodeId = do
      mbRes <- liftIO $ use pool (Db.deleteNode nodeId)
      case mbRes of
        Left  err   -> throwError (dbError err)
        Right False -> throwError (nodeNotFound nodeId)
        Right True  -> pure NoContent

    renameNode :: Int64 -> Api.NodeLabel -> Handler NoContent
    renameNode nodeId (Api.NodeLabel label) = do
      mbRes <- liftIO $ use pool (Db.renameNode nodeId label)
      case mbRes of
        Left  err   -> throwError (dbError err)
        Right False -> throwError (nodeNotFound nodeId)
        Right True  -> pure NoContent

    linkNodes :: Int64 -> Int64 -> Handler NoContent
    linkNodes nodeFrom nodeTo = do
      mbRes <- liftIO $ use pool (Db.linkNodes nodeFrom nodeTo)
      case mbRes of
        Left  err       -> throwError (dbError err)
        Right (Left nodeId) -> throwError (nodeNotFound nodeId)
        Right (Right _) -> pure NoContent

    getAllNodes :: Handler Api.Nodes
    getAllNodes = do
      mbRes <- liftIO $ use pool Db.getAllNodes
      case mbRes of
        Left  err   -> throwError (dbError err)
        Right nodes -> pure $ Api.Nodes (uncurry Api.Node <$> nodes)

    getNeighbours :: Int64 -> Handler Api.Nodes
    getNeighbours nodeId = do
      mbRes <- liftIO $ use pool (Db.getNeighbours nodeId)
      case mbRes of
        Left  err          -> throwError (dbError err)
        Right Nothing      -> throwError (nodeNotFound nodeId)
        Right (Just nodes) -> pure $ Api.Nodes (uncurry Api.Node <$> nodes)

