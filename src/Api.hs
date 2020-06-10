{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Api
  ( Routes(..)
  , swagger
  , writeSwaggerFile
  )
where

import qualified Api.Graph
import Control.Lens
import Data.Aeson (encodeFile)
import Data.Swagger (Swagger, info , title)
import Servant.API.Generic
import Servant.Swagger.UI (SwaggerSchemaUI)
import Servant.Swagger (toSwagger)


data Routes route = Routes
  { _swaggerUi :: route :- SwaggerSchemaUI "swagger-ui" "swagger.json"
  , _graphApi :: route :- ToServantApi Api.Graph.Routes
  } deriving (Generic)

swagger :: Swagger
swagger = toSwagger Api.Graph.proxy
  & info . title .~ "Graph API"

writeSwaggerFile :: IO ()
writeSwaggerFile = encodeFile "swagger.json" swagger
