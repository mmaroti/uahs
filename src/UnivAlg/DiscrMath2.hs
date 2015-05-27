module UnivAlg.DiscrMath2 (runSolver, all, any, lessThan, lessThanEqu,
	reflexive, symmetric, transitive) where

import Prelude hiding (all, any)
import Control.Monad (foldM)
import UnivAlg.Boolean2 (Boolean)
import qualified UnivAlg.Boolean2 as Boolean
import UnivAlg.Array (Array)
import qualified UnivAlg.Array as Array

type Solver m b f = ([b] -> m b) -> Int -> f [Bool]
type Problem m b = [Array b] -> m b

runSolver :: (Boolean m b, Functor f) => Solver m b f -> Problem m b -> [[Int]] -> f [Array Bool]
runSolver s p bs =
	let	g = Array.fromList2 bs
		n = sum (fmap product bs)
	in fmap g $ s (p . g) n

all :: Boolean m b => Array b -> m b
all = foldM Boolean.and Boolean.true . Array.toList

any :: Boolean m b => Array b -> m b
any = foldM Boolean.or Boolean.false . Array.toList

lessThan :: Boolean m b => Int -> Array b
lessThan n =
	let	f [x,y] = Boolean.lift (x < y)
		f _ = undefined
	in Array.generate f [n, n]

lessThanEqu :: Boolean m b => Int -> Array b
lessThanEqu n =
	let	f [x,y] = Boolean.lift (x <= y)
		f _ = undefined
	in Array.generate f [n, n]

reflexive :: Boolean m b => Array b -> m b
reflexive a =
	let n = head $ Array.shape a
	in all $ Array.extend [n] (a, [0,0])

symmetric :: Boolean m b => Array b -> m b
symmetric a =
	let	n = head $ Array.shape a
		b = Array.extend [n, n] (a, [1, 0])
		c = lessThanEqu n
	in all =<< (Array.entrywiseM Boolean.or c =<< Array.entrywiseM Boolean.equ a b)

transitive :: Boolean m b => Array b -> m b
transitive a =
	let n = head $ Array.shape a
	in do
		x <- Array.entrywiseM Boolean.and
			(Array.extend [n,n,n] (a, [0,2]))
			(Array.extend [n,n,n] (a, [2,1]))
		y <- Array.collectM Boolean.or 1 x
		z <- Array.entrywiseM Boolean.leq y a
		all z
