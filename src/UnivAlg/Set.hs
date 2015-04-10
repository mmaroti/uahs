{-# LANGUAGE Rank2Types #-}

module UnivAlg.Set (Set, shape) where

import qualified UnivAlg.Boolean as Boolean
import qualified Data.Vector as Vector
--import Control.Exception (assert)

data Set
	= BoolSet
	| PowerSet Set Int
	| SubSet Set BoolPred
	| ProjSet Set Int

instance Show Set where
	show BoolSet = "BoolSet"
	show (PowerSet s n) = "PowerSet (" ++ show s ++ ") " ++ show n
	show (SubSet s _) = "SubSet (" ++ show s ++ ")"
	show (ProjSet s k) = "ProjSet (" ++ show s ++ ") " ++ show k

shape :: Set -> [Int]
shape BoolSet = []
shape (PowerSet s n) = n : (shape s)
shape (SubSet s _) = shape s
shape (ProjSet s k) = case (shape s) of
	(n:ns) | n > 0 -> (n-1 : ns)
	_ -> error "invalid projection"

extent :: Set -> [Int]
extent BoolSet = []
extent (PowerSet s n) = n : (extent s)
extent (SubSet s _) = extent s
extent (ProjSet s _) = extent s

data Array a = Array [Int] (Vector.Vector a)

newtype BoolPred = BoolPred (Boolean.Boolean b => Array b -> b)

--member :: Set -> BoolFun


{-
shape :: Set -> Array.Shape
shape BoolSet = Array.ShapeOne
shape (PowerSet t n) = Array.ShapeProd n (shape t)
shape (SubSet t _) = shape t

member :: Set -> BoolFun
member BoolSet = BoolFun (const Boolean.true)
member (PowerSet n t) = BoolFun fun where
	fun = error "invalid index"
member _ = undefined

elemSize :: Set -> Int
elemSize BoolSet = 1
elemSize (PowerSet s n) = n * (elemSize s)
elemSize (SubSet s _) = elemSize s

newtype BoolFun2 = BoolFun2 (Boolean.Boolean b => Vector.Vector b -> b)

member2 :: Set -> BoolFun2
member2 BoolSet = BoolFun2 test where
	test v = assert (1 == Vector.length v) Boolean.true
member2 _ = undefined

elements :: Set -> [Array.Array Bool]
elements BoolSet = []
elements _ = undefined

-}
