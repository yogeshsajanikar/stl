{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.STL.TextParser where


import Control.Applicative
import Data.Attoparsec.Text
import Data.Text as T
import qualified Data.List  as L
import qualified Data.Attoparsec.Text.Lazy as Al
import qualified Data.Text.Lazy.IO as TIO
--import qualified Data.Text.Lazy as Tl
import Data.STL.Topology
import Control.Monad
import System.IO

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
beginSolid = (skipSpace <* string "solid" *> optionalSolidName <* skipLine)
           <?> "Parse solid and its name"
       where
            optionalSolidName = option "default" solidName

endSolid :: Parser ()
endSolid = const () <$> (skipSpace <* string "endsolid")
                    <?> "Parse end of the solid"


coordinate :: (Fractional a) => Parser a
coordinate = (skipWhile isHorizontalSpace) *> rational
{-# INLINE coordinate #-}

coordinates :: Fractional a => Text -> (a -> a -> a -> b) -> Parser b 
coordinates s f =  f  <$> (skipSpace <* string s *> coordinate)
                      <*> coordinate
                      <*> coordinate
                      <?> "Parsing triad of coordinates"
{-# INLINE coordinates #-}                      

vertexPoint :: Fractional a => Solid a -> Parser (Point a)
vertexPoint s = coordinates "vertex" (createPoint s)

beginFacet :: Fractional a => Solid a -> Parser (Vector a)
beginFacet s = skipSpace <* string "facet" *> coordinates "normal" (createVec s) <* skipLine
{-# INLINE beginFacet #-}

endFacet :: Parser ()
endFacet = const () <$> (skipSpace <* string "endfacet" <* skipLine)
{-# INLINE endFacet #-}

facet :: (Fractional a, Ord a) => Solid a -> Parser (Solid a)
facet s = addFace s <$> beginFacet s
                    <*> (skipSpace     <* string "outer loop" *> vertexPoint s <* skipLine)
                    <*> (vertexPoint s <* skipLine)
                    <*> (vertexPoint s <* skipLine)
                    <*  (skipSpace <* string "endloop" <* endFacet )
                    <?> "Parse facet"

facet' s = (,,,)    <$> beginFacet s
                    <*> (skipSpace     <* string "outer loop" *> vertexPoint s <* skipLine)
                    <*> (vertexPoint s <* skipLine)
                    <*> (vertexPoint s <* skipLine)
                    <*  (skipSpace <* string "endloop" <* endFacet )
                    <?> "Parse facet"

facets :: (Ord a, Fractional a) => Solid a -> Parser (Solid a)
facets s = L.foldl' merge s <$> many' (facet' s)
  where
    merge solid (n,p1,p2,p3) = addFace solid n p1 p2 p3 

-- facets s = let facet' t = (False,) <$> (facet t)
--            in do
--                 (decision,s') <- option (True,s) (facet' s)
--                 if decision then
--                    return s'
--                 else 
--                    facets s'
               
solid :: (Ord a, Fractional a) => a -> Parser (Solid a)
solid t = beginSolid *> (facets solid) <* endSolid 
      where 
            solid = createSolid $ createSpace t

-- parseSolid tolerance path = TIO.readFile path >>= return . Al.eitherResult . (Al.parse $ solid tolerance)

parseBeginSolid = Al.parse beginSolid
parseEndSolid = Al.parse endSolid

parseFacet s = Al.parse (facet s)

parseFacets s i = case parseFacet s i of
                   Al.Done i' s'    -> parseFacets s' i'
                   Al.Fail i' _ _   -> Al.Done i' s
                   


parseInPackets solid j = case parseBeginSolid j of
                          Al.Done i _  -> case parseFacets solid i of
                                           Al.Done i' s' -> case parseEndSolid i' of
                                                             Al.Done i'' _   -> Al.Done i'' s'
                                                             Al.Fail i'' _ _ -> Al.Fail i'' [] "Failed"
                                           Al.Fail i' _ _ -> Al.Fail i' [] "Failed"
                          Al.Fail i' _ _ -> Al.Fail i' [] "Failed"
  

parseSolid tolerance path = let solid = createSolid $ createSpace tolerance
                            in TIO.readFile path >>= return . Al.eitherResult . (parseInPackets solid)


-- -- parseSolid2 tolerance path = withFile path ReadMode $ parseSolidStrict tolerance

-- -- parseSolidStrict tolerance path 
