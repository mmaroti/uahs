module Test where

import UnivAlg.SatSolver as SatSolver

test :: SatSolver.Instance
test = SatSolver.generate SatSolver.empty $ do
	x <- SatSolver.literal
	y <- SatSolver.literal
	z <- SatSolver.xor x y
	assert z

sol :: SatSolver.Literal -> Bool
sol = case SatSolver.solve test of
	Just f -> f
	_ -> undefined
