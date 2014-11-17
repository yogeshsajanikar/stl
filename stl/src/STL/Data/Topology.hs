module STL.Data.Topology (
  -- Space 
  Space,
  createSpace,
  ---
  Vector,
  createVector,
  ---
  Point,
  Solid,
  createSolid,
  createPoint,
  )
       where

import Prelude hiding (lookup)
import Data.Map.Strict hiding (foldr)

-- | A space represents a container of points and triangles with 
-- a tolerance
data Space a = Space a deriving (Show)

-- | Create space with a given tolerance
createSpace :: a -> Space a 
createSpace = Space

-- | Point represents a vertex of the triangle
data Point a = Point (Space a) a a a 
     deriving Show

-- | Vector for a given class
data Vector a = Vector a a a deriving Show

-- | Normal
createVector :: a -> a -> a -> Vector a
createVector = Vector 

-- | Check whether two scalars are within tolerance
isEqual :: (Ord a, Num a) => a -> a -> a -> Bool
isEqual t p q = delta < t && (-delta) < t
  where
    delta = p - q

-- | Compare two scalars, using \isEqual
compareScalar :: (Ord a, Num a) => a -> a -> a -> Ordering
compareScalar t p q
  | p `cmp` q = EQ
  | p < q     = LT
  | otherwise = GT
  where
    cmp = isEqual t

fromOrdering :: Ordering -> Maybe Ordering
fromOrdering EQ = Nothing
fromOrdering t  = Just t

compareScalar' t p q  = fromOrdering $ compareScalar t p q

toOrdering :: Maybe Ordering -> Ordering
toOrdering Nothing   = EQ
toOrdering (Just x)  = x
 

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
         cmp = compareScalar' minTol
     in toOrdering $ do
                      x1 `cmp` x2
                      y1 `cmp` y2
                      z1 `cmp` z2

-- | Map of point, and its index. Ideally we should be using R*Tree for better indexing. 
type PointMap a = Map (Point a) Int

-- | insert point into the map
insertPoint :: (Ord a, Num a) => Point a -> PointMap a -> (Int, PointMap a)
insertPoint p pmap = case lookup p pmap of
                      Just i   -> (i, pmap)
                      Nothing  -> (j, insert p j pmap)
  where
    j = size pmap + 1

-- | A face contains three vertices and a normal. The face stores a index of the point.
data Face a = Face { firstV  :: Int
                   , secondV :: Int
                   , thirdV  :: Int
                   , normal  :: Vector a
                   } deriving Show                              

-- | Solid contains set of points and faces. 
data Solid a = Solid { baseSpace :: Space a, points :: PointMap a, faces :: [ Face a ] }                  

-- | Create a solid for given space
createSolid :: Space a -> Solid a
createSolid spc = Solid spc empty []

-- | Create a point in the given space
createPoint :: Solid a -> a -> a -> a -> Point a
createPoint s = Point (baseSpace s)

-- insertFace :: Point a -> Point a -> Point a -> Solid a
-- insertFace p q r s =
--   let points  = [p, q, r]
--       indices = foldr (\pt mp -> 

