module Test2 where

import qualified UnivAlg.SatSolver2 as SatSolver
import qualified UnivAlg.Boolean2 as Boolean
import Control.Monad (foldM)

test :: Boolean.Boolean m b => [b] -> m b
test xs = foldM Boolean.add Boolean.false xs

main :: IO ()
main = do
	print $ Boolean.evaluate test [True, False]
	print $ SatSolver.solveOne test 2
--	print $ SatSolver.solveAll test 2
