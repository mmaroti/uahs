module Main where

import qualified UnivAlg.SatSolver as SatSolver
import UnivAlg.Boolean (Boolean)
import qualified UnivAlg.Boolean as Boolean
import qualified UnivAlg.DiscrMath as DiscrMath
import UnivAlg.Array (Array)

test1 :: Boolean m b => [b] -> m b
test1 = Boolean.one

main1 :: IO ()
main1 = do
	print $ Boolean.evaluate test1 [True, False]
	print $ SatSolver.solveOne test1 3
	print $ SatSolver.solveAll test1 3

test2 :: Boolean m b => [Array b] -> m b
test2 [a] = DiscrMath.partialorder a
test2 _ = undefined

main :: IO ()
main = let n = 20 in do
	print $ length . show $ DiscrMath.execute SatSolver.solveOne test2 [[n, n]]
	print $ DiscrMath.execute SatSolver.solveOne test2 [[n, n]]
--	print $ length $ DiscrMath.execute SatSolver.solveAll test2 [[n, n]]
