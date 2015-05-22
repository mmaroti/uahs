module Test where

import UnivAlg.SatSolver as SatSolver
import UnivAlg.DiscrMath as DiscrMath

test1 :: SatSolver.Problem
test1 = do
	x <- SatSolver.literal
	y <- SatSolver.literal
	z <- SatSolver.literal
	u <- SatSolver.xor x y
	v <- SatSolver.xor u z
	assert v
	return [x, y, z]

test2 :: DiscrMath.Problem
test2 = do
	x <- DiscrMath.litarray [5]
	return [x]

main :: IO ()
main = print (SatSolver.solveAll test1)
