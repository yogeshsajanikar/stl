module Main where

import Data.STL.TextParser
import Data.STL.Topology
import System.Environment
import Control.Monad
import Data.STL.TextEnumerator

main :: IO Int
main = do
  let tolerance = 0.001 :: Float
  
  (path:_) <- getArgs
  putStrLn $ "Parsing STL file: " ++ path
  putStrLn $ "Default tolerance " ++ show tolerance

  -- fs <- readSTL path
  -- putStrLn $ "Num facets : " ++ show (length fs)

  s <- readTextSTLRaw tolerance path
  putStrLn "Parsing complete"
  case s of
   Left error -> putStrLn error
   Right s    -> do
     forM_ s (putStrLn . show)
     putStrLn $ "Num facets : " ++ show (length s)
  return 0


