module Output.RequestFlavors.Fetch where

import Output.TSFunctions
import           Typescript
import           Servant.Foreign
import Data.Text
import Control.Lens

mkBody
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> Text
mkBody req = "  return fetch(\\`" <> getReqUrl req <> "\\`)"

reqToTSFunction
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> TSFunctionConfig
reqToTSFunction req = TSFunctionConfig
  { _tsFuncName   = reqToTSFunctionName req
  , _tsArgs       = reqToTSFunctionArgs req
  , _tsReturnType = maybe "void"
                          (asPromise . refName . toForeignType)
                          (req ^. reqReturnType)
  , _body         = mkBody req
  }
