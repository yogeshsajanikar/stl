module STL.Data.Parser
    (
    ) where

import Data.Attoparsec.Text 
import Data.Text as T
import STL.Data.Topology
import Control.Applicative


solidP = string $ T.pack "solid"






