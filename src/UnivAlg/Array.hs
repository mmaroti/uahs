
module UnivAlg.Array where

import qualified Data.List as List

data Shape
	= ShapeOne
	| ShapeProd Int Shape
	| ShapeSum Shape Shape
	deriving (Eq, Show)

size :: Shape -> Int
size ShapeOne = 1
size (ShapeProd n s) = n * (size s)
size (ShapeSum s1 s2) = (size s1) + (size s2)

data Array a = Array Shape ([Int] -> a)

instance Show a => Show (Array a) where
	show (Array ShapeOne f) = show (f [])
--	show (Array (ShapeProd n ProdOne) f) = List.intercalate List.Map (\
	show _ = undefined
