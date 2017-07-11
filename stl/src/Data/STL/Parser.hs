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

   The text format has following structure:

   >  solid <solid name - optional>
   >  facet <normal>
   >    outer loop
   >       point x1 y1 z1
   >       point x2 y2 z2
   >       point x3 y3 z3
   >    endloop
   >    ...
   >  endsolid
-}

module Data.STL.Parser
    (
     -- * STL Reader
     readSTL
    ) where


import Data.STL.TextParser
import System.IO

-- | Read the file in chunks 
readChunks :: Handle -> IO [ByteString]
readChunks h | hIsEOF h = []
readChunks h = hGetSome h 4096 : readChunks h

-- | Feed the parser
parseSolid :: [ByteString] -> Result (Solid a)
parseSolid [] = Fail [] "No input"
parseSolid (b:bs) =
  let result = parse solid b
  in parseSolid' bs result

  where
    parseSolid [] r = r
    parseSolid (b:bs) r = undefined


-- | Read STL file, and returs a solid. The solid contains an indexed set of points, and
-- a list of faces. This function can be slow, as compared to the stream based STL
readSTL :: Fractional a => FilePath -> IO (Either String (Solid a))
readSTL = withFile ReadMode $ \h -> do
  isAtEnd <- hIsEOF
  if isAtEnd 


