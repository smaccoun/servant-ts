{-# LANGUAGE AllowAmbiguousTypes #-}
module ServantTS.Output.RequestFlavors.Class where

import Data.Text
import Typescript

class (TSReturnType (HasReturnType trm)) => TSRequestMethod trm where
  type HasReturnType trm :: *
  printTSReqMethod :: Text

class TSReturnType rt where
  printReturnType :: (IsForeignType t) => t -> Text

data Promise

instance TSReturnType Promise where
  printReturnType t = "Promise<" <> (refName . toForeignType) t <> ">"

