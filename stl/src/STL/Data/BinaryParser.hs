module STL.Data.BinaryParser where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Text as T
import Data.Text.IO
import STL.Data.Topology
import Control.Applicative
import Data.Char
import System.IO (withFile, IOMode(..))


-- | If the text parser fails 



