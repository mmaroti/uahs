module UnivAlg.DiscrMath (Variable, Instance, Problem, variable, generate, solveOne, solveAll) where

import Control.Monad.State (State)
import qualified UnivAlg.Array as Array
import qualified UnivAlg.SatSolver as SatSolver

type Variable = Array.Array SatSolver.Variable
type Instance = SatSolver.Instance
type Problem = State Instance [Variable]

variable :: [Int] -> State Instance Variable
variable = Array.constantM SatSolver.variable

generate :: State Instance a -> (a, Instance)
generate = SatSolver.generate

solveOne :: ([Variable], Instance) -> Maybe [Array.Array Bool]
solveOne (as, i) =
	let (bs, xs) = (fmap Array.shape as, Array.toList2 as)
	in case SatSolver.solveOne (xs, i) of
		Nothing -> Nothing
		Just ys -> Just (Array.fromList2 bs ys)

solveAll :: ([Variable], Instance) -> [[Array.Array Bool]]
solveAll (as, i) =
	let (bs, xs) = (fmap Array.shape as, Array.toList2 as)
	in fmap (Array.fromList2 bs) $ SatSolver.solveAll (xs, i)
