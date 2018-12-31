module ServantTS.Output.FunctionDoc where

import           ServantTS.Convert
import           Data.Maybe      (fromMaybe)
import           Data.Proxy
import Data.Text
import ServantTS.Output.TSFunctions
import           Servant.API
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor,
                                  _reqReturnType)
import           Typescript
import Data.Text.Prettyprint.Doc

apiToFunctionDoc
  :: (IsForeignType (TSIntermediate flavor))
  => [Req (TSIntermediate flavor)]
  -> (Req (TSIntermediate flavor) -> TSFunctionConfig)
  -> Doc ann
apiToFunctionDoc apiReqs reqToTSFunction' =
  mkFunctionDoc $ fmap reqToTSFunction' apiReqs

mkFunctionDoc :: [TSFunctionConfig] -> Doc ann
mkFunctionDoc tsFunctions = vsep $ fmap (pretty . printTSFunction) tsFunctions
