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

-- | If no solid name is given, we assume name of the solid as 
-- "default". We ignore the solid name
solidName :: Parser Text
solidName = do
  skipWhile isHorizontalSpace                     <?> "skip whitespace before solid name"
  option "default" (fmap T.pack (many1 letter)    <?> "solid name parser")

-- | Match start of the solid
beginSolid' :: Parser Text
beginSolid' = do
  skipSpace                           <?> "Neglect white space before 'solid'"
  string "solid"                      <?> "Parse keyword 'solid'"
  name <- solidName                   <?> "parse name of solid, optional"
  takeTill isEndOfLine                <?> "Neglect till end of the line"
  endOfLine                           <?> "match end of line"
  return name

-- | Match beginning of the solid data, and create a solid with given tolerance
beginSolid :: a -> Parser (Solid a)
beginSolid tolerance =
  (\_ -> createSolid space) <$> beginSolid'
  where
    space = createSpace tolerance


coordinate :: (Fractional a) => Parser a
coordinate = skipWhile isHorizontalSpace *> rational

coordinates :: Fractional a => Text -> Parser (a, a, a)
coordinates s = do
  skipSpace
  string s
  x <- coordinate
  y <- coordinate
  z <- coordinate
  return (x, y, z)

triad :: Parser (a, a, a) -> (a -> a -> a -> b) -> Parser b
triad p f =
  (\(x, y, z) -> f x y z) <$> p

vertexPoint :: Fractional a => Solid a -> Parser (Point a)
vertexPoint s = triad (coordinates "vertex") (createPoint s)
  
beginFacet :: Fractional a => Parser (a, a, a)
beginFacet = do
  skipSpace
  string "facet"
  coordinates "normal"

facetContent :: Fractional a => Parser [(a, a, a)]
facetContent = Al.count 3 $ coordinates "vertex"

endFacet :: Parser ()
endFacet = skipSpace *> string "endfacet" *> pure ()

facet :: (Fractional a, Ord a) => Solid a -> Parser (Solid a)
facet s = do
  n          <- triad beginFacet (createVec s)  <?> "Begin facet with normal"
  skipSpace *> string "outer loop"              <?> "Outer loop start"
  [v1,v2,v3] <- Al.count 3 (vertexPoint s)      <?> "facet vertices"
  skipSpace *> string "endloop"                 <?> "Outer loop ends"
  endFacet   <- endFacet                        <?> "End facet"
  return $ addFace s v1 v2 v3 n 
  
parseFacet :: (Fractional a, Ord a) => Solid a -> Tl.Text -> Result (Solid a)
parseFacet = parse . facet 

parseFacets :: (Fractional a, Ord a) => Solid a -> Tl.Text -> Result (Solid a)
parseFacets s i = case parseFacet s i of 
                       Done i' s'    -> parseFacet s' i'
                       Fail i' _ _   -> Done i' s

endSolid :: Parser ()
endSolid = skipSpace *> string "endsolid" *> pure ()

-- parseBeginSolid :: a -> Tl.Text -> Either String (Solid a)
-- parseBeginSolid tolerance i = case parse (beginSolid tolerance) i of
--                                    Done i' 

facets :: (Ord a, Fractional a) => Solid a -> Parser (Solid a)
facets s = do
       (t, s') <- option (True, s) $ do { s'' <- facet s; return (False, s'') }
       if t then 
          return s'
       else 
          facets s'

solid :: (Ord a, Fractional a) => a -> Parser (Solid a)
solid t = do
      s  <- beginSolid t
      s' <- option s (facets s) <?> "Parsing facets"
      endSolid
      return s'


parseSolid tolerance path = withFile path ReadMode (\handle -> do 
                                                        i <- TIO.hGetContents handle
                                                        return $ eitherResult $ parse (solid tolerance) i )


parseSolid2 tolerance path = withFile path ReadMode $ parseSolidStrict tolerance

parseSolidStrict tolerance path 
