module Main where

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
test2 [a] = DiscrMath.transitive a
test2 _ = undefined

main :: IO ()
main = let n = 70 in do
	print $ length . show $ DiscrMath.runSolver SatSolver.solveOne test2 [[n, n]]
--	print $ length $ DiscrMath.runSolver SatSolver.solveAll test2 [[n, n]]
