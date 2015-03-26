
module UnivAlg.Array (Shape(..), size, Array(..)) where

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

strjoin :: String -> [String] -> String
strjoin _ [] = ""
strjoin _ (x : []) = x
strjoin s (x : xs) = x ++ s ++ (strjoin s xs)

instance Show a => Show (Array a) where
	show (Array ShapeOne f) = show (f [])
--	show (Array (ShapeProd n ProdOne) f) = strjoin " " as where
--		as = map fun
	show _ = undefined
