{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           ServantTS.Convert
import           Data.Maybe      (fromMaybe)
import           Data.Proxy
import           APIs
import Data.Text (Text)
import qualified Data.Text as T
import ServantTS.Output.TSFunctions
import           Servant.API
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor,
                                  _reqReturnType)
import           Typescript
import Data.Text.Prettyprint.Doc
import ServantTS.Output.Docs
import ServantTS.Output.RequestFlavors.Fetch (Fetch)
import Dhall hiding (sequence)


data Example =
  Example
    {decFile  :: Text
    ,funcFile :: Text
    } deriving (Generic, ToDhall)

instance FromDhall Example

main :: IO ()
main = do
  f <- (input auto "./mkdocs/README.md.template") :: IO (Example -> Text)
  writeFile "../README.md" $ T.unpack (f config)
  print "FINISHED WRITING FILE"
  print $ f config
 where
  asTS         = servantToReqTS (Proxy :: Proxy FpTs) (Proxy :: Proxy SimpleAPI)
  reqToTSFunction = defaultReqToTSFunction (Proxy @Fetch)
  config = Example
       {decFile = T.pack $ show (apiToTypeDeclarationDoc asTS)
       ,funcFile = T.pack $ show (apiToFunctionDoc asTS reqToTSFunction)
       }


