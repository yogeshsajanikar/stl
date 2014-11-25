{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.STL.TextParser where


import Control.Applicative
import Data.Attoparsec.Text.Lazy as Al
import Data.Text as T hiding(foldl)
import Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as Tl
import Data.STL.Topology
import Control.Monad
import System.IO

-- |
skipLine :: Parser ()
skipLine = const () <$> takeTill isEndOfLine <* endOfLine

-- | If no solid name is given, we assume name of the solid as 
-- "default". We ignore the solid name
solidName :: Parser Text
solidName = T.pack <$> ( skipWhile isHorizontalSpace *> many1 letter ) 
                   <?> "Parse solid name"

-- | Match beginning of the solid data, and create a solid with given tolerance
beginSolid :: Parser Text
beginSolid = (skipSpace *> string "solid" *> optionalSolidName <* skipLine)
           <?> "Parse solid and its name"
       where
            optionalSolidName = option "default" solidName

endSolid :: Parser ()
endSolid = const () <$> (skipSpace <* string "endsolid")
                    <?> "Parse end of the solid"


coordinate :: (Fractional a) => Parser a
coordinate = (skipWhile isHorizontalSpace) *> rational

coordinates :: Fractional a => Text -> (a -> a -> a -> b) -> Parser b 
coordinates s f =  f  <$> (skipSpace *> string s *> coordinate)
                      <*> coordinate
                      <*> coordinate 
                      <?> "Parsing triad of coordinates"

vertexPoint :: Fractional a => Solid a -> Parser (Point a)
vertexPoint s = coordinates "vertex" (createPoint s)

beginFacet :: Fractional a => Solid a -> Parser (Vector a)
beginFacet s = skipSpace *> string "facet" *> (coordinates "normal" $ createVec s) <* skipLine

endFacet :: Parser ()
endFacet = const () <$> (skipSpace *> string "endfacet" <* skipLine)

facet :: (Fractional a, Ord a) => Solid a -> Parser (Solid a)
facet s = addFace s <$> (beginFacet s)
                    <*> (skipSpace *> string "outer loop" *> (vertexPoint s) <* skipLine)
                    <*> ( (vertexPoint s) <* skipLine)
                    <*> ( (vertexPoint s) <* skipLine)
                    <*  (skipSpace *> string "endloop" <* endFacet )
                    <?> "Parse facet"
  
facets :: (Ord a, Fractional a) => Solid a -> Parser (Solid a)
facets s = let facet' t = (False,) <$> (facet t)
           in do
                (decision,s') <- option (True,s) (facet' s)
                if decision then
                   return s'
                else 
                   facets s'
               


solid :: (Ord a, Fractional a) => a -> Parser (Solid a)
solid t = beginSolid *> (facets solid) <* endSolid 
      where 
            solid = createSolid $ createSpace t

parseSolid tolerance path = TIO.readFile path >>= return . eitherResult . (parse $ solid tolerance)


-- -- parseSolid2 tolerance path = withFile path ReadMode $ parseSolidStrict tolerance

-- -- parseSolidStrict tolerance path 
