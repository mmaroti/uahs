module Test where

import UnivAlg.SatSolver as SatSolver
-- import UnivAlg.Array as Array

test :: SatSolver.Problem
test = do
	x <- SatSolver.literal
	y <- SatSolver.literal
	z <- SatSolver.literal
	u <- SatSolver.xor x y
	v <- SatSolver.xor u z
	assert v
	return [x, y, z]

main :: IO ()
main = putStrLn $ show (SatSolver.solveAll test)
