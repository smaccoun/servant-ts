module Convert where

import           Control.Lens
import           Data.Maybe
import           Data.Proxy      (Proxy (Proxy))
import           Data.Text
import qualified Data.Text       as T
import           GHC.Generics
import           Servant.Foreign
import           TSFunctions
import           Typescript


data LangTypescript

instance (TypescriptType a) => HasForeignType LangTypescript TSType a where
  typeFor _ _ _ = fromMaybe TSAny $ toTypescriptType (Proxy :: Proxy a)

servantToTS ::
          ( HasForeign LangTypescript TSType api
          , GenerateList TSType (Foreign TSType api))
         => (Req TSType -> Text)
         -> Proxy api
         -> [Text]
servantToTS genTS proxyApi =
    genTS <$> servantToReqTS proxyApi

servantToReqTS ::
          ( HasForeign LangTypescript TSType api
          , GenerateList TSType (Foreign TSType api))
         => Proxy api
         -> [Req TSType]
servantToReqTS =
  listFromAPI (Proxy :: Proxy LangTypescript) (Proxy :: Proxy TSType)

getFunctions :: [Req TSType] -> [Text]
getFunctions reqs =
  getFunction <$> reqs

getFunction :: Req TSType -> Text
getFunction req =
  printTSFunction cfg
  where
    cfg =
      TSFunctionConfig
        {_tsFuncName   = frmtFunctionName . _reqFuncName $ req
        ,_tsArgs       = []
        ,_tsReturnType = (getTypeName . fromMaybe TSAny . _reqReturnType) req
        ,_body = ""
        }


frmtFunctionName :: FunctionName -> Text
frmtFunctionName (FunctionName parts)=
  T.intercalate "" parts

getTypeName :: TSType -> Text
getTypeName type' =
  case type' of
    TSInterface iname _ -> iname
    TSCollectionType col ->
      case col of
        TSArray t -> "Array<" <> getTypeName t <> ">"
    TSPrimitiveType prim ->
      case prim of
        TSNumber -> "number"
        TSString -> "string"
        TSBoolean -> "boolean"
    TSAny -> "any"
    TSOption _ -> "TODO" --TODO

