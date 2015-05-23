module UnivAlg.DiscrMath (Literal, Instance, Problem, literal, generate, solveOne, solveAll) where

import Control.Monad.State (State)
import qualified UnivAlg.Array as Array
import qualified UnivAlg.SatSolver as SatSolver

type Literal = Array.Array SatSolver.Literal
type Instance = SatSolver.Instance
type Problem = State Instance [Literal]

literal :: [Int] -> State Instance Literal
literal = Array.constantM SatSolver.literal

generate :: State Instance a -> (a, Instance)
generate = SatSolver.generate

solveOne :: ([Literal], Instance) -> Maybe [Array.Array Bool]
solveOne (as, i) =
	let (bs, xs) = (fmap Array.shape as, Array.toList2 as)
	in case SatSolver.solveOne (xs, i) of
		Nothing -> Nothing
		Just ys -> Just (Array.fromList2 bs ys)

solveAll :: ([Literal], Instance) -> [[Array.Array Bool]]
solveAll (as, i) =
	let (bs, xs) = (fmap Array.shape as, Array.toList2 as)
	in fmap (Array.fromList2 bs) $ SatSolver.solveAll (xs, i)
