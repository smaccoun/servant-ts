module Convert where

import           Control.Lens
import           Data.Maybe
import           Data.Proxy                         (Proxy (Proxy))
import           Data.Text
import qualified Data.Text                          as T
import           GHC.Generics
import           Servant.Foreign
import           TSFunctions
import           Typescript
import           Typescript.Internal.Output.Foreign.Class
import           Typescript.Internal.Intermediate.Generic (TypescriptType(..))


data LangTypescript
type TSVanilla = TSIntermediate Vanilla

instance (TypescriptType a) => HasForeignType LangTypescript TSVanilla a where
  typeFor _ _ _ = toTSIntermediate (Proxy :: Proxy a)

servantToTSVanilla ::
          ( HasForeign LangTypescript TSVanilla api
          , GenerateList TSVanilla (Foreign TSVanilla api))
         => (Req TSVanilla -> Text)
         -> Proxy api
         -> [Text]
servantToTSVanilla genTS proxyApi =
    genTS <$> servantToReqTS proxyApi

servantToReqTS ::
          ( HasForeign LangTypescript TSVanilla api
          , GenerateList TSVanilla (Foreign TSVanilla api))
         => Proxy api
         -> [Req TSVanilla]
servantToReqTS =
  listFromAPI (Proxy :: Proxy LangTypescript) (Proxy :: Proxy TSVanilla)
