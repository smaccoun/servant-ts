#!/usr/bin/env stack
-- stack --resolver lts-13.5 script
{-# LANGUAGE OverloadedStrings #-}                           --

import Turtle
import qualified Control.Foldl as Fold
import Control.Monad
import Data.Monoid ((<>))

main = do
  allHSFiles <- getAllHSFiles
  results <- forM allHSFiles $ \fp -> do
    let Right f = toText fp
    Just r <- fold (shell ("floskell " <> f) empty) Fold.head
    return (f, r)
  print "------------------------"
  print "RESULTS! "
  print "------------------------"
  print "------------------------"
  print "     Successfully transformed "
  print $ filter toBool results
  print "------------------------"
  print "     But FAILED to transfor "
  print $ filter (not . toBool) results
  print "------------------------"
  where
    toBool (_, ExitSuccess ) = True
    toBool (_, (ExitFailure _)) = False

getAllHSFiles = do
  allFiles <- fold (lstree "./src" <|> lstree "./test") Fold.list
  return $ filter (flip hasExtension "hs") allFiles
