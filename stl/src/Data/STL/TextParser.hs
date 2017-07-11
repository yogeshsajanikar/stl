{-# LANGUAGE OverloadedStrings #-}
module Data.STL.TextParser where


import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as Al
import           Data.ByteString.Char8 as T
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString.Lazy as Tl
import           Data.Char (isAlphaNum)
import qualified Data.List as L
import Data.STL.Geometry
--import           Data.STL.Topology

-- | White space parser
white = satisfy (isHorizontalSpace . BS.c2w)

-- | Parse a coordinate
coordinate :: Fractional a => Parser a
coordinate = realToFrac <$> double

-- |
coordinates :: (Fractional a, Fractional b, Fractional c) => (a -> b -> c -> d) -> Parser d
coordinates f = f <$> coordinate <*> (some white *> coordinate) <*> (some white *> coordinate)

vertex :: Fractional a => Parser (Point a)
vertex = string "vertex" *> coordinates Point

normal :: Fractional a => Parser (Vector a)
normal = string "normal" *> coordinates Vector

loop :: Fractional a => Parser (Point a, Point a, Point a)
loop = let vs = (,,) <$> vertex <*> (skipSpace *> vertex) <*> (skipSpace *> vertex)
       in beginLoop *> skipSpace *> vs <* skipSpace <* endLoop

  where
    beginLoop :: Parser ()
    beginLoop = string "outer" *> some white *> string "loop" *> pure ()

    endLoop :: Parser ()
    endLoop = string "endloop" *> pure ()
    

facet :: Fractional a => Parser (Facet a)
facet = let fc = (\n (x,y,z) -> Facet n x y z) <$> normal <*> (skipSpace *> loop)
        in beginFacet *> skipSpace *> fc <* skipSpace <* endFacet <?> "facet"

  where
    beginFacet = string "facet"
    endFacet = string "endfacet"

facets :: Fractional a => Parser [Facet a]
facets = many facet


solid :: Fractional a => Parser (Solid a)
solid = Solid <$> (beginSolid *> some white *> solidname)
              <*> (skipSpace *> facets <* skipSpace <* endSolid)
  where
    beginSolid = skipSpace *> string "solid"
    endSolid = skipSpace *> string "endsolid"
    solidname = option "default" $ T.pack <$> (some $ satisfy isAlphaNum)
