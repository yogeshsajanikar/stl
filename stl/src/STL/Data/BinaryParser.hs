module STL.Data.BinaryParser where

import Data.Attoparsec.Text 
import qualified Data.Text as T
import Data.Text.IO
import STL.Data.Topology
import Control.Applicative
import Data.Char
import System.IO (withFile, IOMode(..))

