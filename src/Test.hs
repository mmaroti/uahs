module Test where

import UnivAlg.SatSolver as SatSolver
import UnivAlg.DiscrMath as DiscrMath

test1 :: SatSolver.Problem
test1 = do
	x <- SatSolver.variable
	y <- SatSolver.variable
	z <- SatSolver.variable
	u <- SatSolver.xor x y
	v <- SatSolver.xor u z
	assert v
	return [x, y, z]

main1 :: IO ()
main1 = print (SatSolver.solveAll $ SatSolver.generate test1)

test2 :: DiscrMath.Problem
test2 = do
	x <- DiscrMath.variable [2]
	return [x]

main2 :: IO ()
main2 = print (DiscrMath.solveAll $ DiscrMath.generate test2)
