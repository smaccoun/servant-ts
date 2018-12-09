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



