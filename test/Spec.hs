module Spec where

import           Convert
import           Data.Maybe      (fromMaybe)
import           Data.Proxy
import           APIs
import           Servant.API
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor,
                                  _reqReturnType)
import           Test.Hspec
import           Typescript



servantTSSpec :: Spec
servantTSSpec = do
  describe "base" $ do
    it "should test basic" $ do
      let (asTS :: [Req TSType]) = servantToTS (Proxy :: Proxy SimpleAPI)
          allTypes = sequence $ _reqReturnType <$> asTS
      case allTypes of
        Nothing -> error "Could not match api"
        Just types -> do
          putStrLn $ show $ toTypescript <$> types
          putStrLn $ show types
          types `shouldBe`
            [TSInterface "User"
              [TSField (FieldName "name") (TSPrimitiveType TSString)
              ,TSField (FieldName "age") (TSPrimitiveType TSNumber)
              ,TSField (FieldName "isAdmin") (TSPrimitiveType TSBoolean)
              ]
            ]

