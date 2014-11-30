{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Data.STL.Topology (
  -- Space 
  Space,
  createSpace,
  ---
  Vector,
  createVector,
  ---
  Point,
  Solid,
  getSpace,
  createSolid,
  createPoint,
  createVec,
  addFace,
  --
  numPoints,
  numFacets
  )
       where

import Prelude hiding (lookup)
import Data.Map.Strict
import Control.Monad.State
import Control.DeepSeq

-- | A space represents a container of points and triangles with 
-- a tolerance
data Space a = Space a deriving (Show)

-- | Create space with a given tolerance
createSpace :: a -> Space a 
createSpace = Space

-- | Point represents a vertex of the triangle
data Point a = Point (Space a) !a !a !a 
     deriving Show

instance NFData (Point a)

-- | Vector for a given class
data Vector a = Vector !a !a !a deriving Show

instance NFData (Vector a)

-- | Normal
createVector :: Space a -> a -> a -> a -> Vector a
createVector _ =  Vector 

-- | Check whether two scalars are within tolerance
isEqual :: (Ord a, Num a) => a -> a -> a -> Bool
isEqual t p q = let delta = p - q
                in delta < t && (-delta) < t
{-# INLINE isEqual #-}

-- | Compare two scalars, using \isEqual
compareScalar :: (Ord a, Num a) => a -> a -> a -> Ordering
compareScalar t p q
  | p `cmp` q = EQ
  | p < q     = LT
  | otherwise = GT
  where
    cmp = isEqual t
{-# INLINE compareScalar #-}

-- | Order created by two successive comparisons
order :: Ordering -> Ordering -> Ordering
order EQ second = second
order first _   = first
{-# INLINE order #-}

-- | Two points are equal if their coordinates are within the given tolerance
instance (Num a, Ord a) => Eq (Point a) where
  (Point (Space t1) x1 y1 z1 ) == (Point (Space t2) x2 y2 z2) = xEq && yEq && zEq
    where
      minTol = min t1 t2 -- Work with minimum of two tolerances
      isEqual' = isEqual minTol
      xEq = isEqual' x1 x2
      yEq = isEqual' y1 y2
      zEq = isEqual' z1 z2

-- | Compare two points, starting with first coordinate, checking for 
-- equality and then comparing the numerical value.
instance (Num a, Ord a) => Ord (Point a) where

  (Point (Space t1) x1 y1 z1) `compare` (Point (Space t2) x2 y2 z2) =
     let minTol = min t1 t2
         cmp = compareScalar minTol
     in (x1 `cmp` x2) `order` (y1 `cmp` y2) `order` (z1 `cmp` z2)

-- | Map of point, and its index. Ideally we should be using R*Tree for better indexing. 
type PointMap a = Map (Point a) Int

-- | insert point into the map
insertPoint :: (Ord a, Num a) => Point a -> PointMap a -> (Int, PointMap a)
insertPoint !p !pmap = index (insertLookupWithKey combine p j pmap)
  where
    j = size pmap + 1
    combine _ _ old = old
    index (Just i', pmap') = (i', pmap')
    index (Nothing, pmap') = (j,  pmap')

-- | A face contains three vertices and a normal. The face stores a index of the point.
data Face a = Face { firstV  :: !Int
                   , secondV :: !Int
                   , thirdV  :: !Int
                   , normal  :: Vector a
                   } deriving Show                              

-- | Solid contains set of points and faces. 
data Solid a = Solid { baseSpace :: Space a, points :: PointMap a, faces :: ![ Face a ] }
             deriving Show

-- | Create a solid for given space
createSolid :: Space a -> Solid a
createSolid spc = Solid spc empty []

-- | Get the underlying space for the solid
getSpace :: Solid a -> Space a
getSpace = baseSpace

-- | Create a point in the given space
createPoint :: Solid a -> a -> a -> a -> Point a
createPoint !s !x !y !z = Point (baseSpace s) x y z

createVec :: Solid a -> a -> a -> a -> Vector a
createVec s = createVector (baseSpace s)

-- | Add point to the solid 
addPoint :: (MonadState (Solid a) m, Ord a, Num a) => Point a -> m Int
addPoint !p = do 
         solid <- get 
         let pnts = points solid
             (index, newpoints) = pnts `seq` insertPoint p pnts
         put $ newpoints `seq` solid { points = newpoints }
         return index


-- | Add a face to a solid
addFace' :: (MonadState (Solid a) m, Ord a, Num a) 
        => Vector a -> Point a -> Point a -> Point a -> m (Solid a)
addFace' !n !p1 !p2 !p3 =
  do 
        !i1 <- addPoint p1
        !i2 <- addPoint p2
        !i3 <- addPoint p3
        solid <- get
        let f = Face { firstV = i1, secondV = i2, thirdV = i3, normal = n }
            newfaces = f : faces solid 
        put $! solid { faces = newfaces }
        get

-- | Add a face to the solid 
addFace :: (Ord a, Num a) =>
           Solid a ->
           Vector a ->
           Point a -> Point a -> Point a ->
           Solid a
addFace solid n p1 p2 p3 = execState (addFace' n p1 p2 p3) solid

-- Statistical data about the solid

numPoints :: Solid a -> Int
numPoints = size . points

numFacets :: Solid a -> Int
numFacets = length . faces


