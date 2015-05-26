module UnivAlg.DiscrMath2 (all, any, reflexive, symmetric, solveOne) where

import Prelude hiding (all, any)
import Control.Monad (foldM)
import Control.Monad.State (State)
import UnivAlg.Boolean2 (Boolean)
import qualified UnivAlg.Boolean2 as Boolean
import UnivAlg.Array (Array)
import qualified UnivAlg.Array as Array
import UnivAlg.SatSolver2 (Literal, Instance)
import qualified UnivAlg.SatSolver2 as SatSolver

all :: Boolean m b => Array b -> m b
all = foldM Boolean.and Boolean.true . Array.toList

any :: Boolean m b => Array b -> m b
any = foldM Boolean.or Boolean.false . Array.toList

reflexive :: Boolean m b => Array b -> m b
reflexive a =
	let n = head $ Array.shape a
	in all $ Array.extend [n] (a, [0,0])

symmetric :: Boolean m b => Array b -> m b
symmetric a =
	let	n = head $ Array.shape a
		b = Array.extend [n, n] (a, [1, 0])
	in all =<< Array.entrywiseM Boolean.equ a b

solveOne :: ([Array Literal] -> State Instance Literal) -> [[Int]] -> Maybe [Array Bool]
solveOne f bs =
	let	g = Array.fromList2 bs
		n = sum (fmap product bs)
	in fmap (Array.fromList2 bs) $ SatSolver.solveOne (f . g) n
