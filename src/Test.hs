module Test where

import UnivAlg.SatSolver as SatSolver
import UnivAlg.DiscrMath as DiscrMath
import UnivAlg.Array as Array

test1 :: SatSolver.Problem
test1 = do
	x <- SatSolver.variable
	y <- SatSolver.variable
	z <- SatSolver.variable
	u <- SatSolver.xor x y
	v <- SatSolver.xor u z
	SatSolver.assert v
	return [x, y, z]

main1 :: IO ()
main1 = print (SatSolver.solveAll $ SatSolver.generate test1)

test2 :: DiscrMath.Problem
test2 = do
	x <- DiscrMath.variable [2, 3]
	y <- DiscrMath.collectXor 1 x
	DiscrMath.assert y
	return [x]

main2 :: IO ()
main2 = print (DiscrMath.solveAll $ DiscrMath.generate test2)

test3 :: DiscrMath.Problem
test3 = let n = 4 in do
	x <- DiscrMath.variable [n,n]
	DiscrMath.assert $ Array.extend [n] (x, [0,0])
	y <- Array.entrywiseM SatSolver.equ x (Array.extend [n,n] (x, [1,0]))
	DiscrMath.assert y
	z <- Array.entrywiseM SatSolver.and
		(Array.extend [n,n,n] (x, [0,2]))
		(Array.extend [n,n,n] (x, [2,1]))
	u <- DiscrMath.collectOr 1 z
	v <- Array.entrywiseM SatSolver.equ x u
	DiscrMath.assert v
	return [x]

main3 :: IO ()
main3 = let p = DiscrMath.generate test3 in do
	print (DiscrMath.literals (snd p))
	print (length $ DiscrMath.clauses (snd p))
	-- print (take 2 $ DiscrMath.solveAll p)
