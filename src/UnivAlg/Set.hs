{-# LANGUAGE Rank2Types #-}

module UnivAlg.Set (BoolFun(..), SetTerm(..), shape, member, member2, BoolFun2(..), elements) where

import qualified UnivAlg.Boolean as Boolean
import qualified UnivAlg.Array as Array
import qualified Data.Vector as Vector
import Control.Exception (assert)

data SetTerm
	= BoolSet
	| ProductSet SetTerm SetTerm
	| SubSet SetTerm BoolFun
	| PowerSet Int SetTerm

instance Show SetTerm where
	show BoolSet = "BoolSet"
	show (ProductSet t1 t2) = "ProductSet (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
	show (SubSet t _) = "SubSet (" ++ show t ++ ")"
	show (PowerSet n t) = "PowerSet " ++ show n ++ "(" ++ show t ++ ")"

shape :: SetTerm -> Array.Shape
shape BoolSet = Array.ShapeOne
shape (ProductSet t1 t2) = Array.ShapeSum (shape t1) (shape t2)
shape (SubSet t _) = shape t
shape (PowerSet n t) = Array.ShapeProd n (shape t)

newtype BoolFun = BoolFun (Boolean.Boolean b => Array.Array b -> b)

member :: SetTerm -> BoolFun
member BoolSet = BoolFun (const Boolean.true)
member (PowerSet n t) = BoolFun fun where
	fun = error "invalid index"
member _ = undefined

elemSize :: SetTerm -> Int
elemSize BoolSet = 1
elemSize (ProductSet s1 s2) = (elemSize s1) + (elemSize s2)
elemSize (PowerSet n s) = n * (elemSize s)
elemSize (SubSet s _) = elemSize s

newtype BoolFun2 = BoolFun2 (Boolean.Boolean b => Vector.Vector b -> b)

member2 :: SetTerm -> BoolFun2
member2 BoolSet = BoolFun2 test where
	test v = assert (1 == Vector.length v) Boolean.true
member2 _ = undefined

elements :: SetTerm -> [Array.Array Bool]
elements BoolSet = []
elements _ = undefined
