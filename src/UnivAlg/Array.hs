-- Copyright (C) 2015 Miklos Maroti

module UnivAlg.Array (Array, shape, dim, size, index, indexM, generate, generateM,
	toList, fromList, toList2, fromList2, fmapM, constant, constantM,
	scalar, vector, extend, entrywise, entrywiseM, collect, collectM,
	slice, transform) where

import Control.Exception (assert)
import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (liftM, (<=<), foldM)
import qualified Data.Vector as Vector
import Data.List (foldl1')

data Array a = MakeArray [(Int,Int)] (Vector.Vector a)
	deriving (Show, Eq)

shape :: Array a -> [Int]
shape (MakeArray ds _) = map snd ds

dim :: Array a -> Int
dim (MakeArray ds _) = length ds

size :: Array a -> Int
size = product . shape

idx :: [(Int, Int)] -> [Int] -> Int
idx [] [] = 0
idx ((a, b) : as) (x : xs) = assert (0 <= x && x < b) a * x + idx as xs
idx _ _ = undefined

index :: Array a -> [Int] -> a
index (MakeArray ds v) = (Vector.!) v . idx ds

indexM :: Monad m => Array a -> [Int] -> m a
indexM (MakeArray ds v) = Vector.indexM v . idx ds

pos :: [(Int, Int)] -> [Int] -> [Int]
pos [] ys = ys
pos ((a, b) : xs) ys = [ x + y | x <- [0, a .. a * (b - 1)], y <- pos xs ys ]

toList :: Array a -> [a]
toList a@(MakeArray ds v) =
	if ds == gen (shape a) then Vector.toList v
	else fmap ((Vector.!) v) (pos ds [0])

toList2 :: [Array a] -> [a]
toList2 as = concat $ fmap toList as

gen :: [Int] -> [(Int, Int)]
gen [] = []
gen (b : bs) = case gen bs of
	[] -> [(1, b)]
	xs@((x, y) : _) -> (x * y, b) : xs

inv :: [Int] -> Int -> [Int]
inv bs n = let (xs, m) = g bs n in assert (m == 0) xs where
	g [] m = ([], m)
	g (c : cs) m =
		let	(xs, k) = g cs m
			(l, x) = divMod k c
		in (x : xs, l)

fromList :: [Int] -> [a] -> Array a
fromList bs as =
	let v = Vector.fromList as
	in assert (product bs == Vector.length v) MakeArray (gen bs) v

fromList2 :: [[Int]] -> [a] -> [Array a]
fromList2 bs as =
	let	g [] _ = []
		g (x : xs) ys =
			let (us, vs) = splitAt (product x) ys
			in us : g xs vs
		t = sum $ fmap product bs
	in assert (t == length as) $ fmap (uncurry fromList) $ zip bs (g bs as)

generate :: ([Int] -> a) -> [Int] -> Array a
generate f bs = MakeArray (gen bs) (Vector.generate (product bs) (f . inv bs))

generateM :: Monad m => ([Int] -> m a) -> [Int] -> m (Array a)
generateM f bs = (liftM $ MakeArray (gen bs)) (Vector.generateM (product bs) (f . inv bs))

constant :: a -> [Int] -> Array a
constant a bs = MakeArray (map g bs) (Vector.singleton a) where
	g x = (0, x)

constantM :: Monad m => m a -> [Int] -> m (Array a)
constantM a = generateM (const a)

scalar :: a -> Array a
scalar a = MakeArray [] (Vector.singleton a)

vector :: [a] -> Array a
vector as = MakeArray [(1, length as)] (Vector.fromList as)

instance Functor Array where
	fmap f a = generate (f . index a) (shape a)

fmapM :: Monad m => (a -> m b) -> Array a -> m (Array b)
fmapM f a = generateM (f <=< indexM a) (shape a)

entrywise :: (a -> b -> c) -> Array a -> Array b -> Array c
entrywise f a b = assert (shape a == shape b) generate g3 (shape a) where
	g1 = index a
	g2 = index b
	g3 xs = f (g1 xs) (g2 xs)

entrywiseM :: Monad m => (a -> b -> m c) -> Array a -> Array b -> m (Array c)
entrywiseM f a b = assert (shape a == shape b) $ generateM g3 (shape a) where
	g1 = indexM a
	g2 = indexM b
	g3 xs = (f =<< g1 xs) =<< g2 xs

instance Applicative Array where
	pure = scalar
	(<*>) = entrywise ($)

extend :: [Int] -> (Array a, [Int]) -> Array a
extend bs (MakeArray cs v, ns) = assert (length cs == length ns) MakeArray ds v where
	g x = (0, x)
	upd (a1, b1) (a2, b2) = assert (b1 == b2) (a1 + a2, b1)
	ds = Vector.toList $ Vector.accum upd (Vector.fromList $ map g bs) (zip ns cs)

collect :: (a -> a -> a) -> Int -> Array a -> Array a
collect f n (MakeArray cs v) =
	let	(as, bs) = assert (n <= length cs) $ splitAt (length cs - n) cs
		ys = pos bs [0]
		g xs = let x = idx as xs in foldl1' f $ fmap ((Vector.!) v . (x +)) ys
	in generate g (fmap snd as)

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 _ [a] = return a
foldM1 f (a : as) = foldM f a as
foldM1 _ [] = undefined

mapM' :: Monad m => (a -> m b) -> [a] -> [b] -> m [b]
mapM' _ [] ys = return (reverse ys)
mapM' f (x : xs) ys = f x >>= \y -> mapM' f xs (y : ys)

collectM :: Monad m => (a -> a -> m a) -> Int -> Array a -> m (Array a)
collectM f n (MakeArray cs v) =
	let	(as, bs) = assert (n <= length cs) $ splitAt (length cs - n) cs
		ys = pos bs [0]
		g xs = let x = idx as xs in foldM1 f =<< mapM' (Vector.indexM v . (x +)) ys []
	in generateM g (fmap snd as)

slice :: Array a -> [Int] -> Array a
slice (MakeArray cs v) xs =
	let (as, bs) = assert (length xs <= length cs) $ splitAt (length xs) cs
	in MakeArray bs (Vector.drop (idx as xs) v)

transform :: (Array a -> Array b) -> Int -> Array a -> Array b
transform = undefined
