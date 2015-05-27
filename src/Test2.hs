module Test2 where

import qualified UnivAlg.SatSolver2 as SatSolver
import UnivAlg.Boolean2 (Boolean)
import qualified UnivAlg.Boolean2 as Boolean
import qualified UnivAlg.DiscrMath2 as DiscrMath
import UnivAlg.Array (Array)
import Control.Monad (foldM)

test1 :: Boolean m b => [b] -> m b
test1 = foldM Boolean.add Boolean.false

main1 :: IO ()
main1 = do
	print $ Boolean.evaluate test1 [True, False]
	print $ SatSolver.solveOne test1 2
	print $ SatSolver.solveAll test1 2

test2 :: Boolean m b => [Array b] -> m b
test2 [a] = do
	t1 <- DiscrMath.reflexive a
	t2 <- DiscrMath.symmetric a
	t3 <- DiscrMath.transitive a
	Boolean.and t1 =<< Boolean.and t2 t3
test2 _ = undefined

main2 :: IO ()
main2 = do
	print $ DiscrMath.runSolver SatSolver.solveOne test2 [[4, 4]]
	print $ length $ DiscrMath.runSolver SatSolver.solveAll test2 [[4, 4]]
