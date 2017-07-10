{-# LANGUAGE FlexibleContexts, BangPatterns #-}
{-|
  Module: Topology

  This module supports functionality to create primitives for STL
  model. The most basic primitives are @Point@, and @Vector@. The
  primitives are created in a base space @Space@. The space is
  characterized by a tolerance. The tolerance is used to check
  coincidence for points and vectors.
-}
module Data.STL.Topology (
  -- * Primitive Types
  Space,
  Vector,
  Point,
  Solid,
  RawFacet(..),
  Face,
  -- * Primitive Creation
  createSpace,
  createVector,
  createVec,
  createPoint,
  createSolid,

  -- ** Facet Creation
  addFace,

  -- * Statistical data about the solid
  numPoints,
  numFacets
  )
       where

import Prelude hiding (lookup)
import Data.Map.Strict
import Control.Monad.State
import Control.DeepSeq
import Data.Typeable

-- | A space represents a container of points and triangles with
-- a given tolerance
data Space a = Space a deriving (Show)

-- | Create space with a given tolerance
createSpace :: Fractional a => a -> Space a
createSpace = Space

-- | Point represents a 3D point in space.
data Point a = Point (Space a) !a !a !a
     deriving (Show, Typeable)

instance NFData (Point a)

-- | A 3D vector. (Note: The vector does not keep a reference to the space, as
-- it represents a free vector).
data Vector a = Vector !a !a !a deriving Show

instance NFData (Vector a)


-- | RawFacet contains a normal and three points. RawFacets are currently
-- streamed through STL module APIs.
data RawFacet a = RawFacet (Vector a) (Point a) (Point a) (Point a)
                  deriving Show

-- | Create a vector in a given space
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

-- | A face contains three vertices and a normal. The face stores indices
-- to point.
data Face a = Face { firstV  :: !Int
                   , secondV :: !Int
                   , thirdV  :: !Int
                   , normal  :: Vector a
                   } deriving Show

-- | Solid contains set of points and faces. The solid stores a map
-- of points against its index.
--
-- Note: A point map, in its current avatar, is not efficient implementation. Ideally
-- we should be using spatial tree, which would be soon included in near future.
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

-- | Add a face to a solid. For STL the facet is a triangle, and a vector. The
-- vector represents the normal to the triangle, and may be used to denote the
-- orientation of the triangle. However, the normal can be found by using vertices
-- of triangles.
--
-- Some applications use the normal (which can point to a direction other than
-- the triangles' normal) for rendering.
--
-- Here the normal information is not processed in this version.
addFace :: (Ord a, Num a) =>
           Solid a ->
           Vector a ->
           Point a -> Point a -> Point a ->
           Solid a
addFace solid n p1 p2 p3 = execState (addFace' n p1 p2 p3) solid

-- | Number of points in a solid
numPoints :: Solid a -> Int
numPoints = size . points

-- | Number of faces in the solid
numFacets :: Solid a -> Int
numFacets = length . faces



