module UnivAlg.Array2 (shape, dim, size, index, generate, constant, scalar, vector, extend, fmap2) where

import qualified Data.Vector as Vector
import Control.Exception (assert)
import Control.Applicative (Applicative, pure, (<*>))

data Array a = MakeArray [(Int,Int)] (Vector.Vector a)
	deriving (Show, Eq)

shape :: Array a -> [Int]
shape (MakeArray ds _) = map snd ds

dim :: Array a -> Int
dim (MakeArray ds _) = length ds

size :: Array a -> Int
size = product . shape

index :: Array a -> [Int] -> a
index (MakeArray cs v) = (Vector.!) v . idx cs where
	idx [] [] = 0
	idx ((a, b) : as) (x : xs) = assert (0 <= x && x < b) a * x + idx as xs
	idx _ _ = undefined

generate :: ([Int] -> a) -> [Int] -> Array a
generate f bs = MakeArray (gen 1 bs) (Vector.generate (product bs) (f . inv bs)) where
	gen _ [] = []
	gen a (x : xs) = (a, x) : gen (a * x) xs
	inv [] n = assert (n == 0) []
	inv (x : xs) n = let (m, k) = divMod n x in k : inv xs m

constant :: a -> [Int] -> Array a
constant a bs = MakeArray (map gen bs) (Vector.singleton a) where
	gen x = (0, x)

scalar :: a -> Array a
scalar a = MakeArray [] (Vector.singleton a)

vector :: [a] -> Array a
vector as = MakeArray [(1, length as)] (Vector.fromList as)

extend :: [Int] -> Array a -> [Int] -> Array a
extend bs (MakeArray cs v) ns = assert (length cs == length ns) MakeArray ds v where
	gen x = (0, x)
	upd (a1, b1) (a2, b2) = assert (b1 == b2) (a1 + a2, b1)
	ds = Vector.toList $ Vector.accum upd (Vector.fromList $ map gen bs) (zip ns cs)

instance Functor Array where
	fmap f a = generate (f . index a) (shape a)

fmap2 :: (a -> b -> c) -> Array a -> Array b -> Array c
fmap2 f a b = assert (shape a == shape b) generate g3 (shape a) where
	g1 = index a
	g2 = index b
	g3 xs = f (g1 xs) (g2 xs)

instance Applicative Array where
	pure = scalar
	(<*>) = fmap2 ($)
