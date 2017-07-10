{-# LANGUAGE OverloadedStrings #-}
module Data.STL.TextEnumerator where

import Data.STL.TextParser
import Data.Attoparsec.Enumerator
import Data.Enumerator as E
import qualified Data.Enumerator.Binary as ET
import qualified Data.Enumerator.List as EL
import Data.ByteString
import Data.STL.Topology
import Control.Monad.Trans
import Data.Maybe

-- | Stream STL file as an iteratee of @RawFacet@. It uses the parser for raw facets.
-- Supplied iteratee is fed facet stream. The user can supply own enumerator stream to
-- feed to the streamer.
streamSTL :: (Monad m, Fractional a) =>
             a
          -> Iteratee (RawFacet a) m b
          -> Iteratee ByteString m b
streamSTL t it = iterParser beginSolidI >> (sequenceFacet solid =$= justIsolate =$= EL.map fromJust =$ it)
    where
      solid         = createSolid $ createSpace t
      sequenceFacet = E.sequence . iterParser . maybeFacet
      justIsolate   = EL.isolateWhile (\m -> case m of { Just _ -> True; Nothing -> False } )




stlIterate :: (Monad m, Fractional a) => a -> Iteratee ByteString m [RawFacet a]
stlIterate t = do
  begin
  facets []
    where
      solid = createSolid $ createSpace t
      begin = iterParser beginSolidI
      facet = iterParser $ maybeFacet solid
      facets fs = do
                 mf <- facet
                 case mf of
                   Nothing -> return fs
                   Just f  -> facets (f:fs)
      


