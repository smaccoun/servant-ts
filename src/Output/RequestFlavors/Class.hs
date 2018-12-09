{-# LANGUAGE AllowAmbiguousTypes #-}
module Output.RequestFlavors.Class where

import Data.Text

class TSRequestMethod trm where
  printTSReqMethod :: Text
