module Output.RequestFlavors.Fetch where

import Output.TSFunctions
import           Typescript
import           Servant.Foreign
import Data.Proxy
import Data.Text
import Control.Lens
import Output.RequestFlavors.Class

data Fetch

instance TSRequestMethod Fetch where
  printTSReqMethod = "fetch"

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
  , _body         = mkDefaultBody req (Proxy @Fetch) withDefaultUrlFunc
  }

