{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AWSLambda.Events.APIGateway
import           Control.Lens
import qualified Data.HashMap.Strict         as HashMap
import           Data.Semigroup
import           Data.Text                   (Text)
import           Servant (serve)


import           Lib (itemApi, server)
import           ServantShim (makeHandler)

main :: IO ()
main = apiGatewayMain itemEndpoint

hello :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
hello request = do
  putStrLn "This should go to logs"
  case HashMap.lookup "name" (request ^. agprqPathParameters) of
    Just name -> return $ responseOK & responseBody ?~ "Hello, " <> name
    Nothing -> return responseNotFound


itemEndpoint = makeHandler $ serve itemApi server
