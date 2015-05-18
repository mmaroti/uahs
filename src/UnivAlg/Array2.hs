module UnivAlg.Array2 (shape, dim, size, index, indexM, generate, generateM,
	fmapM, constant, scalar, vector, extend, entrywise, entrywiseM, compose) where

import qualified UnivAlg.Semiring as Semiring
import qualified Data.Vector as Vector
import Control.Exception (assert)
import Control.Applicative (Applicative, pure, (<*>))
import Control.Monad (liftM, (<=<))

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
index (MakeArray cs v) = (Vector.!) v . idx cs

indexM :: Monad m => Array a -> [Int] -> m a
indexM (MakeArray cs v) = Vector.indexM v . idx cs

gen :: Int -> [Int] -> [(Int, Int)]
gen _ [] = []
gen a (x : xs) = (a, x) : gen (a * x) xs

inv :: [Int] -> Int -> [Int]
inv [] n = assert (n == 0) []
inv (x : xs) n = let (m, k) = divMod n x in k : inv xs m

generate :: ([Int] -> a) -> [Int] -> Array a
generate f bs = MakeArray (gen 1 bs) (Vector.generate (product bs) (f . inv bs))

generateM :: Monad m => ([Int] -> m a) -> [Int] -> m (Array a)
generateM f bs = (liftM $ MakeArray (gen 1 bs)) (Vector.generateM (product bs) (f . inv bs))

constant :: a -> [Int] -> Array a
constant a bs = MakeArray (map g bs) (Vector.singleton a) where
	g x = (0, x)

scalar :: a -> Array a
scalar a = MakeArray [] (Vector.singleton a)

vector :: [a] -> Array a
vector as = MakeArray [(1, length as)] (Vector.fromList as)

extend :: [Int] -> Array a -> [Int] -> Array a
extend bs (MakeArray cs v) ns = assert (length cs == length ns) MakeArray ds v where
	g x = (0, x)
	upd (a1, b1) (a2, b2) = assert (b1 == b2) (a1 + a2, b1)
	ds = Vector.toList $ Vector.accum upd (Vector.fromList $ map g bs) (zip ns cs)

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
entrywiseM f a b = assert (shape a == shape b) generateM g3 (shape a) where
	g1 = indexM a
	g2 = indexM b
	g3 xs = (f =<< g1 xs) =<< g2 xs

instance Applicative Array where
	pure = scalar
	(<*>) = entrywise ($)

compose :: Semiring.Semiring a => [Int] -> [(Array a, [Int])] -> Array a
compose s xs =
	let f a (x,y) = entrywise Semiring.prod (extend s x y) a
	in foldl f (constant Semiring.one s) xs

-- project :: Semiring.Semiring a => Array a -> [Int] -> Array a
