module ServantTS.Output.RequestFlavors.Fetch where

import ServantTS.Output.TSFunctions
import           Typescript
import           Servant.Foreign
import Data.Proxy
import Data.Text
import Control.Lens
import ServantTS.Output.RequestFlavors.Class

data Fetch

instance TSRequestMethod Fetch where
  type HasReturnType Fetch = Promise
  printTSReqMethod = "fetch"

reqToTSFunction
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> TSFunctionConfig
reqToTSFunction req = TSFunctionConfig
  { _tsFuncName   = reqToTSFunctionName req
  , _tsArgs       = reqToTSFunctionArgs req
  , _tsReturnType = maybe "void"
                          ((getTSReqMethodReturnType (Proxy @Fetch)))
                          (req ^. reqReturnType)
  , _body         = mkDefaultBody req (Proxy @Fetch) withDefaultUrlFunc
  }


getTSReqMethodReturnType :: forall t ft . (TSRequestMethod t, TSReturnType (HasReturnType t), IsForeignType ft) => Proxy t -> ft -> Text
getTSReqMethodReturnType _ ft =
  (printReturnType @(HasReturnType t)) ft
