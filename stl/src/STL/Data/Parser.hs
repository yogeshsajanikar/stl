{-# LANGUAGE OverloadedStrings #-}
module STL.Data.Parser
    (
    ) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Text.IO
import STL.Data.Topology
import Control.Applicative
import Data.Char
import System.IO (withFile, IOMode(..))

whiteSpace = skipWhile (\c -> c == ' ' || c == '\t')


-- | STL follows a very simple format.
-- solid name 
-- facet normal ni nj nk
--     outer loop
--         vertex v1x v1y v1z
--         vertex v2x v2y v2z
--         vertex v3x v3y v3z
--     endloop
-- endfacet
-- endsolid

-- | Parse coordinates for either a vector or a point
coordinates s p = do 
            string s 
            x <- coordinate
            y <- coordinate
            z <- coordinate
            return $ p x y z
        where
                coordinate = skipSpace *> rational

-- | Normal is a unit vector (not checking unitness)
normal space = coordinates "normal" (createVector space)

-- | Parse 'facet' and normal that follows from it.
beginFacet solid = do
           skipSpace *> string "facet"
           skipSpace
           v <- normal $ getSpace solid
           return v

beginOuterLoop = skipSpace *> string "outer" *> skipSpace *> string "loop"
endOuterLoop = skipSpace *> string "endloop"

endFacet = skipSpace *> string "endfacet"


point solid = skipSpace *> coordinates "vertex" (createPoint solid)

facet solid = do 
      n <- beginFacet solid 
      beginOuterLoop
      v1 <- point solid
      v2 <- point solid
      v3 <- point solid 
      endOuterLoop
      endFacet
      return $ (True, addFace solid v1 v2 v3 n)

endSolid solid = skipSpace *> string "endsolid" *> pure (False, solid)
      
facets solid = do
       (continue, solid') <- (facet solid) <|> (endSolid solid)
       if continue then 
          facets solid'
       else 
          return solid'

solidName = do
          whiteSpace
          option "default" $ takeWhile1 (not . isSpace)

beginSolid space = do
           skipSpace
           string "solid"
           solidName
           takeTill isEndOfLine
           endOfLine
           return $ createSolid space


stl tolerance = do
    solid  <- beginSolid $ createSpace tolerance
    solid' <- facets solid
    return solid'


parseSTL tolerance path = withFile path ReadMode (\handle -> do
         contents <- hGetContents handle
         case parse (stl tolerance) contents of 
              Done _ r             -> return r
              Partial _            -> error "Failed with partial results"
              Fail i context err -> error (T.unpack (T.take 50 i)))
