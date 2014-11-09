module STL.Data.Topology (
  -- Space 
  Space,
  createSpace,
  -- Point 
  Point,
  createPoint,
  -- Vector
  Vector,
  -- Solid
  Solid,
  createSolid
  )
       where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (foldr)

-- | A space represents a container of points and triangles with 
-- a tolerance
data Space a = Space { tol :: a } deriving (Show)

-- | Create space with a given tolerance
createSpace :: a -> Space a 
createSpace = Space

-- | Point represents a vertex of the triangle
data Point a = Point { space :: Space a 
                     , x :: a
                     , y :: a
                     , z :: a
                     }
             deriving Show

-- | Create point in a given space
createPoint :: Space a -> a -> a -> a -> Point a
createPoint = Point

-- | Vector for a given class
data Vector a = Vector { dx :: a, dy :: a, dz :: a } deriving Show


-- | Check whether two scalars are within tolerance
isEqual :: (Ord a, Num a) => a -> a -> a -> Bool
isEqual t p q = delta < t && (-delta) < t
  where
    delta = p - q


compareScalar :: (Ord a, Num a) => a -> a -> a -> Ordering
compareScalar t p q
  | p `cmp` q = EQ
  | p < q     = LT
  | otherwise = GT
  where
    cmp = isEqual t

-- | Two points are equal if their coordinates are within the given tolerance
instance (Num a, Ord a) => Eq (Point a) where
  (Point (Space t1) x1 y1 z1 ) == (Point (Space t2) x2 y2 z2) = xEq && yEq && zEq
    where
      minTol = min t1 t2
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
         xLe = x1 `cmp` x2
         yLe = y1 `cmp` y2
         zLe = z1 `cmp` z2
     in
      case xLe of
       LT  -> xLe
       GT  -> xLe
       EQ  -> case yLe of
               LT -> yLe
               GT -> yLe
               EQ -> zLe


type PointMap a = Map (Point a) Int

insertPoint :: (Ord a, Num a) => Point a -> PointMap a -> (Int, PointMap a)
insertPoint p pmap = case lookup p pmap of
                      Just i   -> (i, pmap)
                      Nothing  -> (j, insert p j pmap)
  where
    j = size pmap + 1

data Face a = Face { fv :: Int
                   , sv :: Int
                   , tv :: Int
                   , n  :: Vector a
                   } deriving Show                              

  
data Solid a = Solid { baseSpace :: Space a, points :: PointMap a, faces :: [ Face a ] }                  

-- | Create a solid for given space
createSolid :: Space a -> Solid a
createSolid spc = Solid spc empty []

-- insertFace :: Point a -> Point a -> Point a -> Solid a
-- insertFace p q r s =
--   let points  = [p, q, r]
--       indices = foldr (\pt mp -> 

