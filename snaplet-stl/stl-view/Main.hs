{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Snaplet.STL.View

main :: IO ()
main = serveSnaplet defaultConfig stlAppInit

