module Main where

import           Convert
import           Data.Maybe
import           Data.Proxy      (Proxy (Proxy))
import           Data.Text
import           Generate
import           GenericMappings
import           GenericMappings
import           GHC.Generics
import           Types
import           Typescript


main :: IO ()
main = do
  let (asTS :: [Req TSType]) = servantToTS (Proxy :: Proxy API)
      allTypes = (fromMaybe (TSPrimitiveType TSString) <$> _reqReturnType) <$> asTS
  putStrLn $ show $ toTypescript <$> allTypes
