
module UnivAlg.Array (Array, length, generate, index, scalar, vector) where

import qualified UnivAlg.Shape as Shape
import qualified Data.Vector as Vector
import Prelude hiding (length)
import qualified Prelude
--import Control.Exception (assert)

data Array a = Array Shape.Shape (Vector.Vector a)
	deriving (Show, Eq)

length :: Array a -> Int
length (Array s _) = Shape.size s

generate :: Shape.Shape -> ([Int] -> a) -> Array a
generate s f = Array s (Vector.generate (Shape.size s) (f . (Shape.coords s)))

index :: Array a -> [Int] -> a
index (Array s v) xs = (Vector.!) v (Shape.index s xs)

scalar :: a -> Array a
scalar a = Array Shape.Scalar (Vector.singleton a)

vector :: [a] -> Array a
vector as = Array (Shape.Vector (Prelude.length as) Shape.Scalar) (Vector.fromList as)
