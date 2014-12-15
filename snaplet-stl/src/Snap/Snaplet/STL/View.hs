{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.STL.View where

 
import           Control.Lens.TH
import           Data.IORef
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap
import           Snap.Snaplet.Heist
import           Data.STL.Topology

data STLApp = STLApp { 
    }


stlAppInit :: SnapletInit STLApp STLApp
stlAppInit = makeSnaplet "stlview" "STL Viewing Snaplet" Nothing $ do
               addRoutes [ ("/", writeText "Load/View STL files")
                         , ("/load", writeText "Load STL files") 
                         ]
               return STLApp 
