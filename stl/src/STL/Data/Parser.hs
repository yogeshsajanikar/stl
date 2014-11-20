{-# LANGUAGE OverloadedStrings #-}
module STL.Data.Parser
    (
    ) where

import Data.Attoparsec.Text 
import Data.Text as T
import STL.Data.Topology
import Control.Applicative
import Data.Char


defaultSolid = createSolid $ createSpace 0.001

-- | STL follows a very simple format.
-- solid name 
-- facet normal ni nj nk
--     outer loop
--         vertex v1x v1y v1z
--         vertex v2x v2y v2z
--         vertex v3x v3y v3z
--     endloop
-- end face
-- endsolid

beginSolid = string "solid"   *>
             blankOrSolidName *>
             pure defaultSolid
  where
    solidName = skipSpace    *>
                many1 letter *>
                return ()
                
    blankOrSolidName = skipSpace <|> solidName


beginFacet = string "facet" *> skipSpace *>
             string "normal"
