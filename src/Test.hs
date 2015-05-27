module Test where

import qualified UnivAlg.SatSolver as SatSolver
import UnivAlg.Boolean (Boolean)
import qualified UnivAlg.Boolean as Boolean
import qualified UnivAlg.DiscrMath as DiscrMath
import UnivAlg.Array (Array)

test1 :: Boolean m b => [b] -> m b
test1 = Boolean.sum

main1 :: IO ()
main1 = do
	print $ Boolean.evaluate test1 [True, False]
	print $ SatSolver.solveOne test1 2
	print $ SatSolver.solveAll test1 2

test2 :: Boolean m b => [Array b] -> m b
test2 [a] = DiscrMath.partialorder a
test2 _ = undefined

main2 :: IO ()
main2 = do
	print $ DiscrMath.runSolver SatSolver.solveOne test2 [[4, 4]]
	print $ length $ DiscrMath.runSolver SatSolver.solveAll test2 [[4, 4]]
