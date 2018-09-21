{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Data.Maybe
import           Data.Proxy       (Proxy (Proxy))
import           GenericMappings
import           Servant.Foreign  (Foreign, GenerateList, HasForeign,
                                   HasForeignType, Req, listFromAPI, typeFor)
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

main :: IO ()
main = do
  putStrLn "Init"
