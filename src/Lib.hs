{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import           Database
import           Database.Beam
import           Database.Beam.MySQL
import           Database.MySQL.Base

-- * api

type ItemApi =
  "item" :> "test" :> Get '[JSON] [Item]

itemApi :: Proxy ItemApi
itemApi = Proxy

server :: Server ItemApi
server = getItems

getItems :: Handler [Item]
getItems = liftIO allItems


allItems :: IO [Item]
allItems = do
  conn <- connect connectIn
  runBeamMySQL conn . runSelectReturningList . select
    $ all_
        (_shoppingCartItems shoppingCartDb)
