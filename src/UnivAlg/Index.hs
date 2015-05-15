module UnivAlg.Index (decode, encode, Map, create, identity, domain, codomain, compose, apply) where

import Control.Exception (assert)

decode :: [Int] -> Int -> [Int]
decode [] n = assert (n == 0) []
decode (d : ds) n =
	let (m, x) = divMod n d
	in x : decode ds m

sizes :: Int -> [Int] -> [(Int, Int)]
sizes _ [] = []
sizes k (d : ds) = (d, k) : sizes (k * d) ds

encode :: [Int] -> [Int] -> Int
encode = enc . sizes 1 where
	enc [] [] = 0
	enc ((d, k) : ds) (x : xs) = assert (0 <= x && x < d)
		k * x + enc ds xs
	enc _ _ = undefined

data Map = MakeMap [Int] [Int]
	deriving (Show, Eq)

identity :: [Int] -> Map
identity ds = MakeMap ds [0 .. (length ds - 1)]

create :: [Int] -> [Int] -> Map
create ds ms =
	let	n = length ds
		f m = 0 <= m && m < n
	in assert (all f ms) MakeMap ds ms

domain :: Map -> [Int]
domain (MakeMap ds _) = ds

codomain :: Map -> [Int]
codomain (MakeMap ds ms) = fmap (ds !!) ms

compose :: Map -> Map -> Map
compose (MakeMap d1 m1) (MakeMap d2 m2) =
	let	c1 = fmap (d1 !!) m1
		c3 = fmap (m1 !!) m2
	in assert (c1 == d2) MakeMap d1 c3

verify :: [Int] -> [Int] -> Bool
verify [] [] = True
verify (d : ds) (x : xs) = 0 <= x && x < d && verify ds xs
verify _ _ = False

apply :: Map -> [Int] -> [Int]
apply (MakeMap ds ms) xs = assert (verify ds xs)
	fmap (xs !!) ms
