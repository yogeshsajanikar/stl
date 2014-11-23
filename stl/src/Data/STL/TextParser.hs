{-# LANGUAGE OverloadedStrings #-}
module Data.STL.TextParser where


import Control.Applicative
import Data.Attoparsec.Text.Lazy as Al
import Data.Text
import Data.STL.Topology

-- | If no solid name is given, we assume name of the solid as 
-- "default". We ignore the solid name
solidName :: Parser Text
solidName = do
  skipWhile isHorizontalSpace
  option "default" $ takeWhile1 (not . isHorizontalSpace)

-- | Match start of the solid
beginSolid' :: Parser Text
beginSolid' = do
  skipSpace
  string "solid"
  name <- solidName
  takeTill isEndOfLine
  endOfLine
  return name

beginSolid :: a -> Parser (Solid a)
beginSolid tolerance =
  (\_ -> createSolid space) <$> beginSolid'
  where
    space = createSpace tolerance

coordinate :: (Fractional a) => Parser a
coordinate = skipWhile isHorizontalSpace *> rational

coordinates :: Fractional a => Text -> Parser (a, a, a)
coordinates s = do
  string s
  x <- coordinate
  y <- coordinate
  z <- coordinate
  return $ (x, y, z)

triad :: Parser (a, a, a) -> (a -> a -> a -> b) -> Parser b
triad p f =
  (\(x, y, z) -> f x y z) <$> p

beginFacet :: Fractional a => Parser (a, a, a)
beginFacet = do
  skipSpace
  string "facet"
  coordinates "normal"

facetContent :: Fractional a => Parser [(a, a, a)]
facetContent = Al.count 3 $ coordinates "vertex"
