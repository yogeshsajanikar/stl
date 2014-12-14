{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Data.STL.TextEnumerator where

import Data.STL.TextParser 
import Data.Attoparsec.Enumerator
import Data.Enumerator as E
import qualified Data.Enumerator.Binary as ET
import qualified Data.Enumerator.List as EL
import Data.ByteString
import Data.STL.Topology
import Control.Monad.Trans

    
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

justIsolate :: Monad m => Enumeratee (Maybe a) (Maybe a) m b
justIsolate = EL.isolateWhile check
    where check (Just _) = True
          check _        = False



sequenceFacet :: (Fractional a, Monad m) => Solid a
              -> Enumeratee ByteString (Maybe (RawFacet a)) m b
sequenceFacet = E.sequence . iterParser . maybeFacet

readSTL' :: (Fractional a, Monad m) => a
         -> FilePath
         -> Iteratee ByteString m (RawFacet a)
readSTL' t path = run_ $ undefined

