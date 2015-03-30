{-# LANGUAGE Rank2Types #-}

module UnivAlg.Set (Set) where

import qualified UnivAlg.Boolean as Boolean
import qualified Data.Vector as Vector
--import Control.Exception (assert)

data Set
	= SetBool
	| SetPower Set Int
	| SetSub Set BoolPred

instance Show Set where
	show SetBool = "SetBool"
	show (SetPower t n) = "SetPower " ++ "(" ++ show t ++ ")" ++ show n
	show (SetSub t _) = "SetSub (" ++ show t ++ ")"

data Shape
	= ShapeOne
	| ShapePower Shape Int
	deriving (Eq, Show)

data Array a = Array Shape (Vector.Vector a)

newtype BoolPred = BoolPred (Boolean.Boolean b => Array b -> b)

--member :: SetTerm -> BoolFun


{-
shape :: SetTerm -> Array.Shape
shape BoolSet = Array.ShapeOne
shape (PowerSet t n) = Array.ShapeProd n (shape t)
shape (SubSet t _) = shape t

member :: SetTerm -> BoolFun
member BoolSet = BoolFun (const Boolean.true)
member (PowerSet n t) = BoolFun fun where
	fun = error "invalid index"
member _ = undefined

elemSize :: SetTerm -> Int
elemSize BoolSet = 1
elemSize (PowerSet s n) = n * (elemSize s)
elemSize (SubSet s _) = elemSize s

newtype BoolFun2 = BoolFun2 (Boolean.Boolean b => Vector.Vector b -> b)

member2 :: SetTerm -> BoolFun2
member2 BoolSet = BoolFun2 test where
	test v = assert (1 == Vector.length v) Boolean.true
member2 _ = undefined

elements :: SetTerm -> [Array.Array Bool]
elements BoolSet = []
elements _ = undefined

-}
