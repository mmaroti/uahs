module UnivAlg.Tensor where

import qualified UnivAlg.Array as Array
import qualified UnivAlg.Semiring as Semiring
import Control.Exception (assert)
import Prelude hiding (product)

data Tensor a = Tensor (Array.Array a) Int
	deriving (Show, Eq)

order :: Tensor a -> Int
order (Tensor a _) = Array.dim a

shape :: Tensor a -> [Int]
shape (Tensor a _) = Array.shape a

contravars :: Tensor a -> Int
contravars (Tensor a c) =
	let o = Array.dim a
	in assert (0 <= c && c <= o) c

covariants :: Tensor a -> Int
covariants (Tensor a c) =
	let o = Array.dim a
	in assert (0 <= c && c <= o) o - c

component :: Tensor a -> [Int] -> a
component (Tensor a _) = Array.index a

generate :: [Int] -> Int -> ([Int] -> a) -> Tensor a
generate ns c f = Tensor (Array.generate ns f) c

product :: Semiring.Semiring a => Tensor a -> Tensor a -> Tensor a
product t1 t2 =
	let	o1 = order t1
		c1 = contravars t1
		c2 = contravars t2
		f1 xs =
			let (ys, zs) = splitAt o1 xs
			in (component t1) (take c1 ys ++ take c2 zs)
		f2 xs =
			let (ys, zs) = splitAt o1 xs
			in (component t2) (drop c1 ys ++ drop c2 zs)
		s = shape t1 ++ shape t2
		g xs = Semiring.prod (f1 xs) (f2 xs)
	in Tensor (Array.generate s g) (c1 + c2)

contraction :: Semiring.Semiring a => Int -> Int -> Tensor a -> Tensor a
contraction = undefined

projection :: Semiring.Semiring a => [Int] -> Int -> Tensor a
projection ns i =
	let h xs = if xs !! i == last xs then Semiring.one else Semiring.zero
	in generate (ns ++ [ns !! i]) (length ns) h

-- compose :: Semiring.Semiring a => Tensor a -> [Tensor a] -> Tensor a
-- compose = undefined
