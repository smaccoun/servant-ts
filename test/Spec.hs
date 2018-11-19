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
      let (asTS :: [Req (TSIntermediate Vanilla)]) = servantToReqTS (Proxy :: Proxy SimpleAPI)
          allTypes :: [TSIntermediate Vanilla]
          allTypes = fromMaybe [] $ sequence $ _reqReturnType <$> asTS
          allDeclarations = fmap (declaration . toForeignType) allTypes
          answer = ["interface User { \n  name : string\n  age : number\n  isAdmin : boolean\n}"]
      allDeclarations `shouldBe` answer
