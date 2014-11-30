{-# LANGUAGE OverloadedStrings #-}
module Data.STL.Parser
    (
    readSTL
    ) where

import Data.STL.TextParser
import Data.STL.Topology

readSTL :: (Fractional a, Ord a) => a -> FilePath -> IO (Either String (Solid a))
readSTL = readTextSTL
