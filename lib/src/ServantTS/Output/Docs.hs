module ServantTS.Output.Docs where

import           Data.Maybe                   ( fromMaybe )
import           Data.Proxy
import           Data.Text

import qualified Data.Text.IO                 as TIO
import           Data.Text.Prettyprint.Doc

import           Servant.API
import           Servant.Foreign
                 ( Foreign, GenerateList, HasForeign, HasForeignType, Req
                 , _reqReturnType, listFromAPI, typeFor )

import           ServantTS.Convert

import           ServantTS.Output.TSFunctions

import           Typescript

data OutputFileNames = OutputFileNames { decFileLoc  :: FilePath
                                       , funcFileLoc :: FilePath
                                       }

apiToTSDocs :: (IsForeignType (TSIntermediate flavor))
            => [Req (TSIntermediate flavor)]
            -> (Req (TSIntermediate flavor) -> TSFunctionConfig)
            -> OutputFileNames
            -> IO ()
apiToTSDocs apiReqs reqToTSFunction' (OutputFileNames typeDecLoc'
                                                      funcFileLoc') = do
    writeFile funcFileLoc' $ show $ apiToFunctionDoc apiReqs reqToTSFunction'
    writeFile typeDecLoc' $ show $ apiToTypeDeclarationDoc apiReqs

{-| Utility methods for writing the function file
-}
apiToFunctionDoc :: (IsForeignType (TSIntermediate flavor))
                 => [Req (TSIntermediate flavor)]
                 -> (Req (TSIntermediate flavor) -> TSFunctionConfig)
                 -> Doc ann
apiToFunctionDoc apiReqs reqToTSFunction' =
    mkFunctionDoc $ fmap reqToTSFunction' apiReqs

{-| Utility methods for writing the TYPE DECLARATION file
-}
mkFunctionDoc :: [TSFunctionConfig]
              -> Doc ann
mkFunctionDoc tsFunctions = vsep $ fmap (pretty . printTSFunction) tsFunctions

allTypes :: [Req a]
         -> [a]
allTypes asTS = fromMaybe [] $ sequence $ _reqReturnType <$> asTS

apiToTypeDeclarationDoc :: IsForeignType t
                        => [Req t]
                        -> Doc ann
apiToTypeDeclarationDoc asTS = vsep $
    fmap (pretty . declaration . toForeignType) (allTypes asTS)
