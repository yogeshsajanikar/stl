{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Data.STL.TextParser where


import Control.Applicative
import Data.Attoparsec.Text
import Data.Text as T
import qualified Data.List  as L
import qualified Data.Attoparsec.Text.Lazy as Al
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.IO as SIO
--import qualified Data.Text.Lazy as Tl
import Data.STL.Topology
import Control.Monad
import System.IO
import Control.DeepSeq

-- |
skipLine :: Parser ()
skipLine = const () <$> takeTill isEndOfLine <* endOfLine
{-# INLINE skipLine #-}

-- | If no solid name is given, we assume name of the solid as 
-- "default". We ignore the solid name
solidName :: Parser Text
solidName = T.pack <$> (skipWhile isHorizontalSpace *> many1 letter ) 
                   <?> "Parse solid name"

-- | Match beginning of the solid data, and create a solid with given tolerance
beginSolid :: Parser Text
beginSolid = (skipSpace <* "solid" *> optionalSolidName )
           <?> "Parse solid and its name"
       where
            optionalSolidName = option "default" solidName

endSolid :: Parser ()
endSolid = const () <$> (skipSpace <* "endsolid")
                    <?> "Parse end of the solid"


coordinate :: (Fractional a) => Parser a
coordinate = realToFrac <$> (skipWhile isHorizontalSpace *> double)
{-# INLINE coordinate #-}

coordinates :: (Fractional a) => Text -> (a -> a -> a -> b) -> Parser b 
coordinates s f = do
  skipSpace
  string s
  !x <- coordinate
  !y <- coordinate
  !z <- coordinate
  return $! f x y z

vertexPoint :: (Fractional a) => Solid a -> Parser (Point a)
vertexPoint s = coordinates "vertex" (createPoint s)

beginFacet :: Fractional a => Solid a -> Parser (Vector a)
beginFacet s = skipSpace <* string "facet" *> coordinates "normal" (createVec s)
{-# INLINE beginFacet #-}

endFacet :: Parser ()
endFacet = const () <$> (skipSpace <* string "endfacet")
{-# INLINE endFacet #-}

facet :: (Fractional a, Ord a) => Solid a -> Parser (Solid a)
facet s = addFace s <$> beginFacet s
                    <*> (skipSpace     <* string "outer loop" *> vertexPoint s <* skipLine)
                    <*> (vertexPoint s <* skipLine)
                    <*> (vertexPoint s <* skipLine)
                    <*  (skipSpace <* string "endloop" <* endFacet )
                    <?> "Parse facet"

facet' s = (,,,)    <$> beginFacet s
                    <*> (skipSpace     <* string "outer loop" *> vertexPoint s)
                    <*> (vertexPoint s)
                    <*> (vertexPoint s)
                    <*  (skipSpace <* string "endloop" <* endFacet )
                    <?> "Parse facet"

facets :: (Ord a, Fractional a) => Solid a -> Parser (Solid a)
facets s = L.foldl' merge s <$> many' (facet' s)
  where
    merge solid (!n,!p1,!p2,!p3) = addFace solid n p1 p2 p3 

               
solid :: (Ord a, Fractional a) => a -> Parser (Solid a)
solid t = beginSolid *> (facets solid) <* endSolid 
      where 
            solid = createSolid $ createSpace t

parseSolid tolerance path = TIO.readFile path >>= return . Al.eitherResult . (Al.parse $ solid tolerance)

