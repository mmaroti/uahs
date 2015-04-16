
module UnivAlg.Shape (Shape(..), size, index, coords) where

import Control.Exception (assert)
--import Data.Function.Memoize as Memoize

data Shape
	= Scalar
	| Repeat Int Shape
	| Stack [Shape]
	deriving (Show, Eq)

size :: Shape -> Int
size Scalar = 1
size (Repeat n s) = n * size s
size (Stack ss) = sum (map size ss)

sumlist :: Int -> [Int] -> [Int]
sumlist _ [] = []
sumlist n (x : xs) = n : sumlist (n + x) xs

index :: Shape -> [Int] -> Int
index Scalar = \xs -> case xs of
	[] -> 0
	_ : _ -> error "too many coords"
index (Repeat n s) =
	let
		k = size s
		f = index s
	in
		\xs -> case xs of
			y : ys -> assert (0 <= y && y < n) k * y + f ys
			[] -> error "too few coords"
index (Stack ss) =
	let
		ts = zip (sumlist 0 (map size ss)) (map index ss)
	in
		\xs -> case xs of
			y : ys -> let (k, f) = ts !! y in k + f ys
			[] -> error "too few coords"

coords :: Shape -> Int -> [Int]
coords Scalar = \k -> assert (k == 0) []
coords (Repeat n s) =
	let
		m = size s
		f = coords s
	in
		\k -> let (i, j) = divMod k m in assert (0 <= i && i < n) i : f j
coords (Stack ss) =
	let
		ts = zip (map size ss) (map coords ss)
		work ((k, f) : rs) a i = if i < k then a : f i else work rs (a + 1) (i - k)
		work [] _ _ = error "too large index"
	in
		work ts 0
