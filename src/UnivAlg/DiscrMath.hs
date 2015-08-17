-- Copyright (C) 2015 Miklos Maroti

module UnivAlg.DiscrMath (runSolver, all, any, few, one, sum,
	makeRelation, reflexive, symmetric, antisymmetric, transitive,
	equivalence, quasiorder, partialorder) where

import Prelude hiding (all, any, sum)
import qualified Prelude
import UnivAlg.Boolean (Boolean)
import qualified UnivAlg.Boolean as Boolean
import UnivAlg.Array (Array)
import qualified UnivAlg.Array as Array

type Solver m b f = ([b] -> m b) -> Int -> f [Bool]
type Problem m b = [Array b] -> m b

runSolver :: (Boolean m b, Functor f) => Solver m b f -> Problem m b -> [[Int]] -> f [Array Bool]
runSolver s p bs =
	let	g = Array.fromList2 bs
		n = Prelude.sum (fmap product bs)
	in fmap g $ s (p . g) n

all :: Boolean m b => Array b -> m b
all = Boolean.all . Array.toList

any :: Boolean m b => Array b -> m b
any = Boolean.any . Array.toList

few :: Boolean m b => Array b -> m b
few = Boolean.few . Array.toList

one :: Boolean m b => Array b -> m b
one = Boolean.one . Array.toList

sum :: Boolean m b => Array b -> m b
sum = Boolean.sum . Array.toList

makeRelation :: Boolean m b => (Int -> Int -> Bool) -> Int -> Array b
makeRelation f n =
	let	g [x, y] = Boolean.lift $ f x y
		g _ = undefined
	in Array.generate g [n, n]

reflexive :: Boolean m b => Array b -> m b
reflexive a =
	let n = head $ Array.shape a
	in all $ Array.extend [n] (a, [0,0])

symmetric :: Boolean m b => Array b -> m b
symmetric a =
	let	n = head $ Array.shape a
		b = Array.extend [n, n] (a, [1, 0])
		c = makeRelation (<) n
	in all =<< (Array.entrywiseM Boolean.leq c =<< Array.entrywiseM Boolean.equ a b)

antisymmetric :: Boolean m b => Array b -> m b
antisymmetric a =
	let	n = head $ Array.shape a
		b = Array.extend [n, n] (a, [1, 0])
		c = makeRelation (==) n
	in do
		x <- Array.entrywiseM Boolean.and a b
		y <- Array.entrywiseM Boolean.leq x c
		all y

transitive :: Boolean m b => Array b -> m b
transitive a =
	let n = head $ Array.shape a
	in do
		x <- Array.entrywiseM Boolean.and
			(Array.extend [n, n, n] (a, [0, 2]))
			(Array.extend [n, n, n] (a, [2, 1]))
		y <- Array.collectM Boolean.or 1 x
		z <- Array.entrywiseM Boolean.leq y a
		all z

equivalence :: Boolean m b => Array b -> m b
equivalence a = do
	x <- reflexive a
	y <- symmetric a
	z <- transitive a
	Boolean.all [x, y, z]

quasiorder :: Boolean m b => Array b -> m b
quasiorder a = do
	x <- reflexive a
	y <- transitive a
	Boolean.and x y

partialorder :: Boolean m b => Array b -> m b
partialorder a = do
	x <- reflexive a
	y <- antisymmetric a
	z <- transitive a
	Boolean.all [x, y, z]
