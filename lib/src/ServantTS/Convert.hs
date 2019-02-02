module ServantTS.Convert where

import           Control.Lens
import           Data.Maybe
import           Data.Proxy                         (Proxy (Proxy))
import           Data.Text
import qualified Data.Text                          as T
import           GHC.Generics
import           Servant.Foreign
import           ServantTS.Output.TSFunctions
import           Typescript
import           Typescript.Internal.Output.Foreign.Class
import           Typescript.Internal.Intermediate.Generic (TypescriptType(..))


data LangTypescript
type TSVanilla = TSIntermediate Vanilla

instance (TypescriptType a) => HasForeignType LangTypescript (TSIntermediate flavor) a where
  typeFor _ _ _ = toTSIntermediate (Proxy :: Proxy a)

servantToReqTS
  :: ( HasForeign LangTypescript (TSIntermediate flavor) api
     , GenerateList
         (TSIntermediate flavor)
         (Foreign (TSIntermediate flavor) api)
     )
  => Proxy flavor
  -> Proxy api
  -> [Req (TSIntermediate flavor)]
servantToReqTS _ = listFromAPI (Proxy :: Proxy LangTypescript)
                               (Proxy :: Proxy (TSIntermediate flavor))
