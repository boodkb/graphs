module Server
  ( run
  )
where

import qualified Api
import Hasql.Pool (Pool)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API.Generic
import Servant.Swagger.UI (swaggerSchemaUIServer)
import Servant.Server.Generic (AsServer, genericServe)
import qualified Server.Graph

server :: Pool -> Api.Routes AsServer
server pool = Api.Routes (swaggerSchemaUIServer Api.swagger)
                         (toServant (Server.Graph.server pool))

run :: Int -> Pool -> IO ()
run port pool = do
    Api.writeSwaggerFile
    Warp.run port $ genericServe (server pool)

