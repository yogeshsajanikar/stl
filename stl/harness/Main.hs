module Main where

import Data.STL.Parser
import Data.STL.Topology
import System.Environment

main :: IO Int
main = do
  let tolerance = 0.001 :: Float
  (path:_) <- getArgs
  putStrLn $ "Parsing STL file: " ++ path
  putStrLn $ "Default tolerance " ++ show tolerance
  s <- parseSolid tolerance path
  putStrLn "Parsing complete"
  case s of
   Left error -> putStrLn error
   Right s    -> do
     putStrLn $ "Num points : " ++ show (numPoints s)
     putStrLn $ "Num facets : " ++ show (numFacets s)
  return 0


