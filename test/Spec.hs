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
      let
        asTS =
          servantToReqTS (Proxy :: Proxy FpTs) (Proxy :: Proxy SimpleAPI)
        allTypes        = fromMaybe [] $ sequence $ _reqReturnType <$> asTS
        allDeclarations = fmap (declaration . toForeignType) allTypes
        answer =
          [ "interface User { \n  name : string\n  age : number\n  isAdmin : boolean\n}"
          ]
      allDeclarations `shouldBe` answer
