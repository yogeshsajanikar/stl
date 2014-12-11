{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Data.STL.TextEnumerator where

import Data.STL.TextParser 
import Data.Attoparsec.Enumerator
import Data.Enumerator
import qualified Data.Enumerator.Binary as ET
import Data.ByteString
import Data.STL.Topology

facetsI :: (Monad m, Fractional a) => Solid a -> [RawFacet a] -> Iteratee ByteString m [RawFacet a]
facetsI s fs = do 
        !f <- iterParser (maybeFacet s)
        case f of
             Nothing -> return fs
             Just f' -> facetsI s (f':fs)


iterateSTL :: Monad m => Iteratee ByteString m [RawFacet Float]
iterateSTL = do
           iterParser beginSolidI 
           fs <- facetsI solid []
           iterParser endSolidI
           return fs
      where
            solid = createSolid $ createSpace (0.001 :: Float)


readSTL :: FilePath -> IO [RawFacet Float]
readSTL path = run_ (ET.enumFile path $$ iterateSTL)
        
facetsi :: (Monad m, Fractional a) => Solid a -> Iteratee ByteString m (Maybe (RawFacet a))
facetsi s = do
  f <- faceti
  case f of 
    Nothing -> return f
    Just f' -> do
              return f 
              facetsi s
    where
      faceti = iterParser $ maybeFacet s
               
solidi :: (Monad m, Fractional a) => a -> Iteratee ByteString m (Maybe (RawFacet a))
solidi t = do
  solidi 
  f <- facetsi solid 
  endsolidi
  return f
         where solidi = iterParser beginSolidI
               endsolidi = iterParser endSolidI
               solid = createSolid $ createSpace t
solide :: (Monad m, Fractional a) => a -> Enumeratee ByteString (Maybe (RawFacet a)) m b
solide t (Continue _) = undefined
solide _ step         = return step
