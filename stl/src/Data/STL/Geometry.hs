{-# LANGUAGE BangPatterns #-}
module Data.STL.Geometry where

import Data.ByteString.Char8

-- | Point in space
data Point a = Point !a !a !a deriving Show

-- | Vector in space
data Vector a = Vector !a !a !a deriving Show

-- | Facet
data Facet a = Facet !(Vector a) !(Point a) !(Point a) !(Point a)

-- | Solid has a name and
data Solid a = Solid ByteString [Facet a]
