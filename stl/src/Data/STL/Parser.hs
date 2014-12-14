{-# LANGUAGE OverloadedStrings #-}
{- | 
   Module : Parser
   
   STL is a very popular format for representing tessellated solid model. STL file
   format is simple. It simply enlists facets in the solid model. Each facet has a 
   triplet of points, and optionally a normal. Since the point data is duplicated 
   by facets sharing it, STL format is verbose, and there can be errors exporting
   facets. Typically the tessellated data is less accurate and is represented 
   using single precision numbers.

   The STL file can be represnted either using a text format or using a binary 
   format. The binary format is used to represent large data. 
-}

module Data.STL.Parser
    (
    readSTL
    ) where

import Data.STL.TextParser
import Data.STL.Topology

readSTL :: (Fractional a, Ord a) => a -> FilePath -> IO (Either String (Solid a))
readSTL = readTextSTL
