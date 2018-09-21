{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Data.Maybe
import           Data.Proxy          (Proxy (Proxy))
import           Data.Text
import           GenericMappings
import           GenericMappings
import           GHC.Generics
import           Servant.API
import           Servant.Foreign     (Foreign, GenerateList, HasForeign,
                                      HasForeignType, Req, listFromAPI, typeFor,
                                      _reqReturnType)
import           Typescript.Generate
import           Typescript.Types


data LangTypescript

instance (TypescriptType a) => HasForeignType LangTypescript TSType a where
  typeFor _ _ _ = fromMaybe (TSPrimitiveType TSNumber) $ toTypescriptType (Proxy :: Proxy a)

servantToTS ::
          ( HasForeign LangTypescript TSType api
          , GenerateList TSType (Foreign TSType api))
         => Proxy api
         -> [Req TSType]
servantToTS =
    listFromAPI (Proxy :: Proxy LangTypescript) (Proxy :: Proxy TSType)

data User = User
    {name    :: Text
    ,age     :: Int
    ,isAdmin :: Bool
    } deriving (Generic, TypescriptType)

type API = "users" :> Get '[JSON] User

main :: IO ()
main = do
  let (asTS :: [Req TSType]) = servantToTS (Proxy :: Proxy API)
      allTypes = (fromMaybe (TSPrimitiveType TSString) <$> _reqReturnType) <$> asTS
  putStrLn $ show $ toTypescript <$> allTypes
