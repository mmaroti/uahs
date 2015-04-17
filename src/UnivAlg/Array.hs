
module UnivAlg.Array (Array, size, shape, generate, index, scalar, vector, matrix, stack, concat) where

import qualified UnivAlg.Shape as Shape
import qualified Data.Vector as Vector
import Prelude hiding (concat)
import Control.Exception (assert)

data Array a = Array Shape.Shape (Vector.Vector a)
	deriving (Show, Eq)

size :: Array a -> Int
size (Array s _) = Shape.size s

shape :: Array a -> Shape.Shape
shape (Array s _) = s

generate :: Shape.Shape -> ([Int] -> a) -> Array a
generate s f = Array s (Vector.generate (Shape.size s) (f . (Shape.coords s)))

index :: Array a -> [Int] -> a
index (Array s v) xs = (Vector.!) v (Shape.index s xs)

scalar :: a -> Array a
scalar a = Array Shape.Scalar (Vector.singleton a)

vector :: [a] -> Array a
vector as = Array (Shape.Stack (length as) Shape.Scalar) (Vector.fromList as)

matrix :: [[a]] -> Array a
matrix as = stack (map vector as)

stack :: [Array a] -> Array a
stack [] = error "cannot stack an empty list"
stack as =
	let
		s = shape (head as)
		vs = map (\(Array s2 v2) -> assert (s == s2) v2) as
	in
		Array (Shape.Stack (length vs) s) (Vector.concat vs)

concat :: [Array a] -> Array a
concat as =
	let
		s = Shape.Concat (map shape as)
		v = Vector.concat (map (\(Array _ v2) -> v2) as)
	in
		Array s v
