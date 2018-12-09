module Spec where

import           Convert
import           Control.Lens
import           Data.Maybe      (fromMaybe)
import           Data.Proxy
import           APIs
import Data.Text
import Output.TSFunctions
import           Servant.API
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor,
                                  _reqReturnType)
import           Test.Hspec
import           Typescript
import qualified Output.RequestFlavors.Fetch as Fetch

servantTSSpec :: Spec
servantTSSpec = describe "base" $ do
  it "should test basic" $ do
    let
      asTS = servantToReqTS (Proxy :: Proxy FpTs) (Proxy :: Proxy SimpleAPI)
      allTypes = fromMaybe [] $ sequence $ _reqReturnType <$> asTS
      allDeclarations = fmap (declaration . toForeignType) allTypes
      answer =
        [ "Array<User>"
        , "interface User { \n  name : string\n  age : number\n  isAdmin : boolean\n  hasMI : Option<string>\n}"
        ]
    allDeclarations `shouldBe` answer
  it "should print out correct functions" $ do
    let asTS = servantToReqTS (Proxy :: Proxy FpTs) (Proxy :: Proxy SimpleAPI)
        allFunctions = fmap (printTSFunction . Fetch.reqToTSFunction) asTS
    allFunctions `shouldBe` answer
 where
  answer =
    [ "function getUser(): Promise<Array<User>> {\n  return fetch(withRemoteBaseUrl(\\`user\\`))\n}"
    , "function getUserByUserId(userId : number): Promise<User> {\n  return fetch(withRemoteBaseUrl(\\`user/${userId}\\`))\n}"
    ]

