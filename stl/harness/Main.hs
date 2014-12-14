module Main where

import System.Environment
import Data.STL.Topology
import Data.STL.Parser
import Control.Monad
import Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

main :: IO Int
main = do
  let tolerance = 0.001 :: Float
  
  (path:_) <- getArgs
  putStrLn $ "Parsing STL file: " ++ path
  putStrLn $ "Default tolerance " ++ show tolerance

  run_ $ EB.enumFile path $$ streamSTL tolerance (E.printChunks False)
  -- s <- readSTL tolerance path
  -- putStrLn "Parsing complete"
  -- case s of
  --   Left e    -> putStrLn $ show e
  --   Right s'  -> putStrLn $ "Num facets : " ++ show (numFacets s')
  -- forM_ fs (putStrLn . show)
  return 0


