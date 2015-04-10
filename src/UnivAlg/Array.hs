
module UnivAlg.Array (Array, generate, index) where

import qualified Data.Vector as Vector
import Prelude hiding (length)
import Control.Exception (assert)

data Array a = Array [(Int,Int)] (Vector.Vector a)
	deriving (Show, Eq)

type Shape = [(Int,Int)]

size :: Shape -> Int
size [] = 1
size ((a,b):_) = a * b

shape :: [Int] -> Shape
shape [] = []
shape (a:as) = let
	bs = shape as
	in (a,size bs) : bs

fromPos :: Shape -> Int -> [Int]
fromPos [] i = assert (i == 0) []
fromPos ((a,b):cs) i = let
	(j,k) = divMod i b
	in assert (0 <= j && j < a) j : fromPos cs k

toPos :: Shape -> [Int] -> Int
toPos [] [] = 0
toPos ((a,b):cs) (d:ds) = assert (0 <= d && d < a) (d * b) + toPos cs ds
toPos _ _ = error "incorrect index"

generate :: [Int] -> ([Int] -> a) -> Array a
generate as f = let
	bs = shape as
	in Array bs (Vector.generate (size bs) (f . fromPos bs))

index :: Array a -> [Int] -> a
index (Array as v) bs = (Vector.!) v (toPos as bs)
