{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module UnivAlg.SatSolver2 (Literal, Instance, solveOne, solveAll) where

import Prelude hiding (not, and)
import Control.Monad.State (State, state, runState)
import Control.Monad (replicateM)
import Debug.Trace (trace)
import qualified Data.Set as Set
import qualified Picosat
import UnivAlg.Boolean2

newtype Literal = Literal { getLiteral :: Int }
	deriving (Show, Eq)
data Instance = Instance Int [[Literal]]
	deriving (Show, Eq)

literal :: State Instance Literal
literal = state $ \(Instance ls cs) -> (Literal (ls + 1), Instance (ls + 1) cs)

clause :: [Literal] -> State Instance ()
clause c = state $ \(Instance ls cs) -> ((), Instance ls (c : cs))

instance Boolean (State Instance) Literal where
	true = Literal 1
	not (Literal x) = Literal (negate x)
	and x y
		| x == true = return y
		| x == false = return false
		| y == true = return x
		| y == false = return false
		| otherwise = do
			z <- literal
			clause [x, not z]
			clause [y, not z]
			clause [not x, not y, z]
			return z
	equ x y
		| x == true = return y
		| x == false = return (not y)
		| y == true = return x
		| y == false = return (not x)
		| x == y = return true
		| x == not y = return false
		| otherwise = do
			z <- literal
			clause [x, y, z]
			clause [x, not y, not z]
			clause [not x, y, not z]
			clause [not x, not y, z]
			return z

generate :: ([Literal] -> State Instance Literal) -> Int -> ([Literal], Instance)
generate f n =
	let g = do
		xs <- replicateM n literal
		y <- f xs
		clause [y]
		return xs
	in runState g (Instance 1 [[true]])

literals :: Instance -> Int
literals (Instance ls _) = ls

clauses :: Instance -> [[Int]]
clauses (Instance _ cs) = fmap (fmap getLiteral) cs

answer :: [Int] -> Literal -> Bool
answer as =
	let a = Set.fromList $ filter (> 0) as
	in (`Set.member` a) . getLiteral

solve1 :: ([Literal], Instance) -> Maybe [Bool]
solve1 (xs, i) = case Picosat.unsafeSolve (clauses i) of
	Picosat.Solution as -> Just $ fmap (answer as) xs
	Picosat.Unsatisfiable -> Nothing
	Picosat.Unknown -> error "picosat failed"

solveOne :: ([Literal] -> State Instance Literal) -> Int -> Maybe [Bool]
solveOne f n =
	let	(xs, i) = generate f n
		m = "Solving SAT with " ++ show (literals i)
			++ " literals and " ++ show (length $ clauses i)
			++ " clauses."
	in trace m $ solve1 (xs, i)

liftAdd :: Boolean m b => Bool -> b -> b
liftAdd x y = if x then not y else y

exclude :: Instance -> [Bool] -> [Literal] -> Instance
exclude (Instance ls cs) bs xs = Instance ls (fmap (uncurry liftAdd) (zip bs xs) : cs)

solve2 :: ([Literal], Instance) -> [[Bool]]
solve2 (xs, i) = case solve1 (xs, i) of
	Nothing -> []
	Just bs -> bs : solve2 (xs, exclude i bs xs)

solveAll :: ([Literal] -> State Instance Literal) -> Int -> [[Bool]]
solveAll f n = solve2 $ generate f n
