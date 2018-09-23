module Convert where

import           Data.Maybe
import           Data.Proxy      (Proxy (Proxy))
import           Data.Text
import           GHC.Generics
import           Servant.Foreign (Foreign, GenerateList, HasForeign,
                                  HasForeignType, Req, listFromAPI, typeFor,
                                  _reqReturnType)
import           Typescript


data LangTypescript

instance (TypescriptType a) => HasForeignType LangTypescript TSType a where
  typeFor _ _ _ = fromMaybe TSAny $ toTypescriptType (Proxy :: Proxy a)

servantToTS ::
          ( HasForeign LangTypescript TSType api
          , GenerateList TSType (Foreign TSType api))
         => Proxy api
         -> [Req TSType]
servantToTS =
    listFromAPI (Proxy :: Proxy LangTypescript) (Proxy :: Proxy TSType)
