module ServantTS.Output.TSFunctions where

import           Control.Lens
import           Data.Monoid     ((<>))
import           Data.Proxy
import           Data.Text       (Text)
import qualified Data.Text       as T
import Data.Text.Prettyprint.Doc
import           Servant.Foreign
import           Servant.Foreign.Inflections (camelCase)
import           Typescript
import ServantTS.Output.RequestFlavors.Class

{- Represents the essential parts to describe a Typescript function -}
data TSFunctionConfig =
  TSFunctionConfig
    {_tsFuncName   :: Text
    ,_tsArgs       :: [TSArg]
    ,_tsReturnType :: Text
    ,_body         :: TSFunctionBody
    }

{- The main function to use when wanting to print out all the desired network request calls -}
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


newtype TSFunctionBody = TSFunctionBody {getTSFunctionBody :: Lines}
type Lines = [Text]

{-|TSArg is an argument in a typescript function, consisting of a name and a type
  e.g. :  function getUserId(userId: number) {return userId}
  has _tsArgs = [TSArg (TSTypedVar {varName = "userId", varType: "number"})]
-}
newtype TSArg = TSArg {getTypedVar :: TSTypedVar} deriving (Show)
data TSTypedVar = TSTypedVar {varName :: Text, varType :: Text} deriving (Show)



{-| The below functions are generally considered for internal use only, but the module exposes them in case of edge case customization
-}
printArgs :: [TSArg] -> Text
printArgs tsArgs' =
  T.intercalate "," $ printTSTypedVar . getTypedVar <$> tsArgs'

printTSTypedVar :: TSTypedVar -> Text
printTSTypedVar (TSTypedVar name' type') = name' <> " : " <> type'

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
mkDefaultBody req _ tsBaseUrlFunc = TSFunctionBody
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
