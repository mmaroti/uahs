module UnivAlg.DiscrMath (LitArray, Instance, Problem, litarray) where

import Control.Monad.State (State)
import qualified UnivAlg.Array as Array
import qualified UnivAlg.SatSolver as SatSolver

type LitArray = Array.Array SatSolver.Literal
type Instance = SatSolver.Instance
type Problem = State Instance [LitArray]

litarray :: [Int] -> State Instance LitArray
litarray = Array.constantM SatSolver.literal
