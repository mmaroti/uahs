module UnivAlg.DiscrMath (LitArray, Instance, Problem, litArray) where

import Control.Monad.State (State)
--import Control.Monad (liftM)
import qualified UnivAlg.Array as Array
import qualified UnivAlg.SatSolver as SatSolver

type LitArray = Array.Array SatSolver.Literal
type Instance = SatSolver.Instance
type Problem = State Instance [LitArray]

litArray :: [Int] -> State Instance LitArray
litArray = Array.constantM SatSolver.literal

--solveOne :: Problem -> Maybe [Array.Array Bool]
--solveOne p =
--	let	q = liftM Array.toList2 $ p
--		bs = fmap Array.shape
--	in undefined
