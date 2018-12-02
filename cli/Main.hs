module Main where

import           Convert
import           Data.Maybe      (fromMaybe)
import           Data.Proxy
import           APIs
import Data.Text
import Output.TSFunctions
import           Servant.API
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor,
                                  _reqReturnType)
import           Typescript
import Data.Text.Prettyprint.Doc
import Output.FunctionDoc


main :: IO ()
main = do
  print $ apiToFunctionDoc asTS
  print $ pretty allDeclarations
 where
  asTS = servantToReqTS (Proxy :: Proxy FpTs) (Proxy :: Proxy SimpleAPI)
  allFunctions =
    Data.Text.intercalate "\n\n" $ fmap (printTSFunction . reqToTSFunction) asTS
  allTypes = fromMaybe [] $ sequence $ _reqReturnType <$> asTS
  allDeclarations =
    Data.Text.intercalate "\n\n" $ fmap (declaration . toForeignType) allTypes

