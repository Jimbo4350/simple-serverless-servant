{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database where

import Data.Aeson

import Database.Beam
import Database.Beam.MySQL
import Database.MySQL.Base

import Data.Text (Text)


data ItemT f
    = Item
    { _itemId :: Columnar f Int }
    deriving Generic

type Item = ItemT Identity
type ItemId = PrimaryKey ItemT Identity

deriving instance Show Item
deriving instance Eq Item
deriving instance ToJSON Item

instance Beamable ItemT

instance Table ItemT where
    data PrimaryKey ItemT f = ItemId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = ItemId . _itemId


data ShoppingCartDb f = ShoppingCartDb { _shoppingCartItems :: f (TableEntity ItemT) }
                        deriving (Generic, Database MySQL)

shoppingCartDb :: DatabaseSettings MySQL ShoppingCartDb
shoppingCartDb = defaultDbSettings



-- CONNECTION --

connectIn :: ConnectInfo
connectIn = ConnectInfo
               { connectHost = "xxxxx.rds.amazonaws.com"
               , connectPort = 3306
               , connectUser = ""
               , connectPassword = ""
               , connectDatabase = "itemsdb"
               , connectOptions = []
               , connectPath = ""
               , connectSSL = Nothing
               }
