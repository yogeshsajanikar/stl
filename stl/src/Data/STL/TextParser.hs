{-# LANGUAGE OverloadedStrings #-}
module Data.STL.TextParser where


import Control.Applicative
import Data.Attoparsec.Text.Lazy
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

coordinate = skipWhile isHorizontalSpace *> rational

coordinates s = do
  string s
  x <- coordinate
  y <- coordinate
  z <- coordinate
  return $ (x, y, z)


