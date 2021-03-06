{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{- |
-}
module Data.STL.TextParser where


import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as T
import qualified Data.ByteString.Lazy as Tl
import qualified Data.List  as L
import qualified Data.Attoparsec.ByteString.Lazy as Al
import Data.STL.Topology
import Control.Monad
import Data.Char(isAlphaNum)
import qualified Data.ByteString.Internal as BS (c2w, w2c)

-- | Parse coordinate tripliet.
coordinates :: (Fractional a) => ByteString -> (a -> a -> a -> b) -> Parser b
coordinates s f = do
  skipSpace
  string s
  !x <- coordinate
  !y <- coordinate
  !z <- coordinate
  return $! f x y z
  where
    coordinate = skipWhile (isHorizontalSpace . BS.c2w) *> fmap realToFrac double

-- | Parse a facet. The facet comprises of a normal, and three vertices
facet  :: Fractional a => Solid a -> Parser (RawFacet a)
facet s = RawFacet <$> beginFacet s
          <*> (skipSpace     <* string "outer loop" *> vertexPoint s)
          <*> vertexPoint s
          <*> vertexPoint s
          <*  (skipSpace <* string "endloop" <* endFacet )
          <?> "facet"
  where
    beginFacet s  = skipSpace <* string "facet" *> coordinates "normal" (createVec s)
    endFacet      = const () <$> (skipSpace <* string "endfacet")
    vertexPoint s = coordinates "vertex" (createPoint s)


-- | Parse all the facets
facets :: (Ord a, Fractional a) => Solid a -> Parser (Solid a)
facets s = L.foldl' merge s <$> many' (facet s) <?> "facets"
  where
    merge solid  (RawFacet !n !p1 !p2 !p3) = addFace solid n p1 p2 p3

-- | Parse STL solid. Solid name is not used
solid :: (Ord a, Fractional a) => a -> Parser (Solid a)
solid t = beginSolid *> facets solid <* endSolid
      where
            solid      = createSolid $ createSpace t
            solidName  = option "default" (skipWhile (isHorizontalSpace . BS.c2w) *> fmap T.pack (many1 $ satisfy isAlphaNum) )
            endSolid   = skipSpace <* "endsolid"
            beginSolid = skipSpace <* "solid" *> solidName

rawFacets :: Fractional a => a -> Parser [RawFacet a]
rawFacets tolerance = beginSolid *> many' (facet solid) <* endSolid
      where
            solid      = createSolid $ createSpace tolerance
            solidName  = option "default" (skipWhile (isHorizontalSpace . BS.c2w) *> fmap T.pack (many1 $ satisfy isAlphaNum) )
            endSolid   = skipSpace <* "endsolid" <?> "end solid"
            beginSolid = skipSpace <* "solid" *> solidName <?> "start solid"

eitherResultForce :: Al.Result r -> Either String r
eitherResultForce (Al.Done _ r) = Right $! r
eitherResultForce (Al.Fail i' ctx msg) = Left $ L.concat ctx ++ " : " ++ msg -- ++ "(remaining inp : " ++ i'

-- | Just reads an STL file and return a list of tuple (n,v1,v2,v3)
-- where n is normal, and v1,v2,v3 are vertices of a facet
readTextSTLRaw :: Fractional a => a -> FilePath -> IO (Either String [RawFacet a])
readTextSTLRaw tolerance path = do
  i <- Tl.readFile path
  let r = eitherResultForce $! Al.parse (rawFacets tolerance) i
  return $! r

-- | Read text STL file. STL extensions for color etc. are not supported in this version.
readTextSTL :: (Fractional a, Ord a) => a -> FilePath -> IO (Either String (Solid a))
readTextSTL tolerance path = liftM (Al.eitherResult . Al.parse (solid tolerance)) (Tl.readFile path)

maybeFacet :: Fractional a => Solid a -> Parser (Maybe (RawFacet a))
maybeFacet s = option Nothing (Just `fmap` facet s)


solidNameI  = option "default" (skipWhile (isHorizontalSpace . BS.c2w) *> fmap T.pack (many1 $ satisfy isAlphaNum) )
endSolidI   = skipSpace <* "endsolid" <?> "end solid"
beginSolidI = skipSpace <* "solid" *> solidNameI <?> "start solid"
