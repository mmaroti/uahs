module UnivAlg.Array2 where

import qualified Data.Vector as Vector
import Control.Exception (assert)

data Array a = Array [Int] (Vector.Vector a)
	deriving (Show, Eq)

coord2pos :: [Int] -> [Int] -> Int
coord2pos [] =
	\xs -> case xs of
		[] -> 0
		_ -> error "too many coordinates"
coord2pos (n : ns) =
	let	m = product ns
		f = coord2pos ns
	in \xs -> case xs of
		(y : ys) -> assert (0 <= y && y < n) y * m + f ys
		_ -> error "too few coordinates"

pos2coord :: [Int] -> Int -> [Int]
pos2coord [] = \k -> assert (k == 0) []
pos2coord (n : ns) =
	let	m = product ns
		f = pos2coord ns
	in \k -> let (i, j) = divMod k m in assert (0 <= i && i < n) i : f j

shape :: Array a -> [Int]
shape (Array ns _) = ns

dim :: Array a -> Int
dim = length . shape

size :: Array a -> Int
size (Array ns v) =
	let m = product ns
	in assert (Vector.length v == m) m

index :: Array a -> [Int] -> a
index (Array ns v) xs = (Vector.!) v (coord2pos ns xs)

slice :: Array a -> Int -> Array a
slice (Array [] _) = error "cannot slice a scalar"
slice (Array (n : ns) v) = let m = product ns in
	\i -> assert (0 <= i && i < n) Array ns (Vector.slice (i * m) m v)

generate :: [Int] -> ([Int] -> a) -> Array a
generate ns f = Array ns (Vector.generate (product ns) (f . pos2coord ns))

array :: [Int] -> [a] -> Array a
array ns as =
	let v = Vector.fromList as
	in assert (product ns == Vector.length v) Array ns v

scalar :: a -> Array a
scalar a = Array [] (Vector.singleton a)

vector :: [a] -> Array a
vector as = Array [length as] (Vector.fromList as)

matrix :: [[a]] -> Array a
matrix as = stack (map vector as)

stack :: [Array a] -> Array a
stack [] = error "cannot stack an empty list"
stack as@(b : _) = stack' (shape b) as

stack' :: [Int] -> [Array a] -> Array a
stack' ns as =
	let vs = map (\(Array ms w) -> assert (ms == ns) w) as
	in Array (length vs : ns) (Vector.concat vs)

instance Functor Array where
	fmap f (Array ns v) = Array ns (fmap f v)

collate :: (a -> a -> a) -> Array a -> Array a -> Array a
collate f (Array ns v) (Array ms w) =
	let	k = product (tail ms)
		l = (head ms) * k
		g = \i -> f ((Vector.!) v (div i k)) ((Vector.!) w (mod i l))
	in assert (last ns == head ms) Array (ns ++ tail ms) (Vector.generate (product ns * k) g)

collapse :: (a -> a -> a) -> Int -> Array a -> Array a
collapse = undefined

compose :: (a -> a -> a) -> (a -> a -> a) -> Array a -> Array a -> Array a
compose f g a b = collapse g (dim a - 1) (collate f a b)

multiply :: Num a => Array a -> Array a -> Array a
multiply a b = compose (*) (+) a b

{-
--multiply :: Array a -> Array a -> Array a
--multiply (Array [] _) _ = error "scalar multiplication"
--multiply (Array [a] v) _ = undefined
--multiply (Array [a:as] v) b = stack (map (slice
-}
