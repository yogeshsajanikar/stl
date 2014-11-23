{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Data.STL.Parser
    (
      parseSTL,
      parseSTL2
    ) where

import Data.Attoparsec.Text.Lazy
import qualified Data.Text as T
import qualified Data.Text.Lazy (Text(..))
import Data.Text.Lazy.IO as TIO
import Data.STL.Topology
import Control.Applicative
import Data.Char
--import System.IO (withFile, IOMode(..))
import Data.List(foldl')

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
      return $ (v1, v2, v3, n)

endSolid :: Solid a -> Parser (Solid a)
endSolid solid = skipSpace *> string "endsolid" *> pure solid

-- | parse as many facets 
facets' solid = many' $ facet solid

-- | fold the facets into the solid 
facets solid = do
  fs <- facets' solid
  return $ foldl' addFacet solid fs
  where
    addFacet s (v1, v2, v3, n) = addFace s v1 v2 v3 n

-- | A name of the solid is optional. The default name of the solid 
-- is "default", in case it is not specified. 
solidName = do
          whiteSpace
          option "default" $ takeWhile1 (not . isSpace)

-- | Match the beginning of the solid 
beginSolid :: Space a -> Parser (Solid a)
beginSolid space = do
           skipSpace
           string "solid"
           solidName
           takeTill isEndOfLine
           endOfLine
           return $ createSolid space

-- | STL parser
stl :: (Ord a, Num a, Fractional a) => a -> Parser (Solid a)
stl tolerance = do
    solid  <- beginSolid $ createSpace tolerance
    solid' <- facets solid
    endSolid solid 
    return solid'

-- | Parse STL with given tolerance and path
parseSTL :: (Ord a, Num a, Fractional a) => a -> FilePath -> IO (Either String (Solid a))
parseSTL tolerance path = let processSTL contents = 
                                --contents <- hGetContents handle
                                case parse (stl tolerance) contents of 
                                 Done _ r             -> Right r
                                 Fail i context err   -> Left $ "ERROR parsing STL file. " ++ err

                          in do
                            text <- TIO.readFile path
                            return $ processSTL text




parseSTL2 :: (Ord a, Num a, Fractional a) => a -> FilePath -> IO (Either String (Solid a))
parseSTL2 tolerance path = do
  i <- TIO.readFile path 
  return $ processSTL2 space i 
    where space = createSpace tolerance

--processSTL2 :: Space a -> Handle -> IO (Either String (Solid a))
processSTL2 sp contents = 
  case parse (beginSolid sp) contents of
   Done i r     -> Right $ processFacets i r
   Fail _ _ err -> Left err 



--processFacets :: (Ord a, Num a, Fractional a) => Text -> Solid a -> Solid a
processFacets i !solid =
  let facet' = do
        (v1, v2, v3, n) <- facet solid <?> "facet parse"
        return $ (False, addFace solid v1 v2 v3 n)
      endSolid' = endSolid solid *> pure (True, solid) <?> "end parse"

  in case parse (facet' ) i of
      Done _  (True,  solid') -> solid'
      Done i' (False, solid') -> processFacets i' solid'
      Fail _ ctx err          -> solid --error $ concat ctx ++ " " ++ err


