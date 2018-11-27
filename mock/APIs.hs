{-# LANGUAGE DeriveAnyClass #-}

module APIs where

import           Data.Text
import           GHC.Generics
import           Servant.API
import           Servant.Foreign     (Foreign, GenerateList, HasForeign,
                                      HasForeignType, Req, listFromAPI, typeFor,
                                      _reqReturnType)
import           Typescript

data User = User
    {name    :: Text
    ,age     :: Int
    ,isAdmin :: Bool
    ,hasMI   :: Maybe Text
    } deriving (Generic, TypescriptType)


type SimpleAPI = "user" :> Get '[JSON] [User]
              :<|> "user" :> Capture "userId" Int :> Get '[JSON] User
