module ServantTS.Output.TSFunctions where

import           Control.Lens
import Data.Text.Strict.Lens
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import           Data.Proxy
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Debug.Trace
import           GHC.Generics
import           Servant.API
import Data.Text.Prettyprint.Doc
import           Servant.Foreign
import           Servant.Foreign.Inflections (camelCase)
import           Typescript
import ServantTS.Output.RequestFlavors.Class

data TSFunctionConfig =
  TSFunctionConfig
    {_tsFuncName   :: Text
    ,_tsArgs       :: [TSArg]
    ,_tsReturnType :: Text
    ,_body         :: TSFunctionBody
    }

data TSTypedVar = TSTypedVar {varName :: Text, varType :: Text} deriving (Show)

printTSTypedVar (TSTypedVar name' type') = name' <> " : " <> type'

newtype TSArg = TSArg {getTypedVar :: TSTypedVar} deriving (Show)

newtype TSFunctionBody = TSFunctionBody {getTSFunctionBody :: [Text]}

printArgs :: [TSArg] -> Text
printArgs tsArgs' =
  T.intercalate "," $ printTSTypedVar . getTypedVar <$> tsArgs'

printTSFunction :: TSFunctionConfig -> Text
printTSFunction (TSFunctionConfig tsFuncName' tsArgs' tsReturnType' body') =
  T.pack $ show $ vsep [pretty funcDeclareLine, printBody, pretty ("}" :: Text)]
 where
  funcDeclareLine =
    "function "
      <> tsFuncName'
      <> "("
      <> printArgs tsArgs'
      <> "): "
      <> tsReturnType'
      <> " {"
  printBody = indent 2 $ vsep (pretty <$> getTSFunctionBody body')


reqToTSFunctionArgs
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> [TSArg]
reqToTSFunctionArgs req =
  map (reqArgToTSArg . captureArg) . filter isCapture $ req ^. reqUrl . path

getReqUrl
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> Text
getReqUrl req = T.intercalate "/" (fmap handleSegment $ req ^. reqUrl . path)
 where
  handleSegment
    :: (IsForeignType (TSIntermediate flavor))
    => Segment (TSIntermediate flavor)
    -> Text
  handleSegment seg = case unSegment seg of
    Static (PathSegment ps) -> ps
    Cap arg -> "${" <> (varName . getTypedVar . reqArgToTSArg) arg <> "}"


reqArgToTSArg
  :: (IsForeignType (TSIntermediate flavor))
  => Arg (TSIntermediate flavor)
  -> TSArg
reqArgToTSArg (Arg argName' argType') = TSArg $ TSTypedVar
  { varName = unPathSegment argName'
  , varType = refName . toForeignType $ argType'
  }

reqToTSFunctionName
  :: (IsForeignType (TSIntermediate flavor))
  => Req (TSIntermediate flavor)
  -> Text
reqToTSFunctionName req = camelCase $ req ^. reqFuncName

withDefaultUrlFunc :: Text -> Text
withDefaultUrlFunc t = "withRemoteBaseUrl(`" <> t <> "`)"


type TSBaseUrlMethod = Text -> Text

mkDefaultBody
  :: forall flavor trm
   . (IsForeignType (TSIntermediate flavor), TSRequestMethod trm)
  => Req (TSIntermediate flavor)
  -> Proxy trm
  -> TSBaseUrlMethod
  -> TSFunctionBody
mkDefaultBody req tsReqMethod tsBaseUrlFunc = TSFunctionBody
  [ "return "
    <> printTSReqMethod @trm
    <> "("
    <> (tsBaseUrlFunc $ getReqUrl req)
    <> ")"
  ]

getTSReqMethodReturnType
  :: forall t ft
   . (TSRequestMethod t, TSReturnType (HasReturnType t), IsForeignType ft)
  => Proxy t
  -> ft
  -> Text
getTSReqMethodReturnType _ ft = (printReturnType @(HasReturnType t)) ft


defaultReqToTSFunction
  :: (IsForeignType (TSIntermediate flavor), TSRequestMethod tsReqMethod)
  => Proxy tsReqMethod
  -> Req (TSIntermediate flavor)
  -> TSFunctionConfig
defaultReqToTSFunction pTSReqMethod req = TSFunctionConfig
  { _tsFuncName   = reqToTSFunctionName req
  , _tsArgs       = reqToTSFunctionArgs req
  , _tsReturnType = maybe "void"
                          ((getTSReqMethodReturnType pTSReqMethod))
                          (req ^. reqReturnType)
  , _body         = mkDefaultBody req pTSReqMethod withDefaultUrlFunc
  }
