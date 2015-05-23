module UnivAlg.SatSolver (Literal, Instance, Problem, literal, clause, true, false,
	lift, not, or, liftOr, and, liftAnd, leq, equ, liftEqu, add, liftAdd, xor,
	assert, assertEqu, assertLeq, clauses, literals, solveOne, solveAll) where

import Prelude hiding (not, or, and)
import Control.Monad.State (State, state, runState)
import Control.Monad (liftM)
import qualified Control.Exception as Exception
import qualified Data.Set as Set
import qualified Picosat

type Literal = Int
data Instance = MakeInst Int [[Int]]
	deriving (Show, Eq)
type Problem = State Instance [Literal]

literal :: State Instance Literal
literal = state $ \(MakeInst ls cs) -> (ls + 1, MakeInst (ls + 1) cs)

clause :: [Literal] -> State Instance ()
clause c = state $ \(MakeInst ls cs) -> ((), MakeInst ls (c : cs))

true :: Literal
true = 1

false :: Literal
false = not true

lift :: Bool -> Literal
lift b = if b then true else false

not :: Literal -> Literal
not a = - a

or :: Literal -> Literal -> State Instance Literal
or a b
	| a == false = return b
	| a == true || a == not b = return true
	| b == false || a == b = return a
	| otherwise = do
		c <- literal
		clause [not a, c]
		clause [not b, c]
		clause [a, b, not c]
		return c

liftOr :: Bool -> Literal -> Literal
liftOr a b = if a then true else b

and :: Literal -> Literal -> State Instance Literal
and a b = liftM not (or (not a) (not b))

liftAnd :: Bool -> Literal -> Literal
liftAnd a b = if a then b else false

leq :: Literal -> Literal -> State Instance Literal
leq a = or (not a)

equ :: Literal -> Literal -> State Instance Literal
equ a b
	| a == true = return b
	| a == false = return (not b)
	| b == true = return a
	| b == false = return (not a)
	| a == b = return true
	| a == not b = return false
	| otherwise = do
		c <- literal
		clause [a, b, c]
		clause [a, not b, not c]
		clause [not a, b, not c]
		clause [not a, not b, c]
		return c

liftEqu :: Bool -> Literal -> Literal
liftEqu a b = if a then b else not b

add :: Literal -> Literal -> State Instance Literal
add a = equ (not a)

liftAdd :: Bool -> Literal -> Literal
liftAdd a b = if a then not b else b

-- xor 1 1 is undefined
xor :: Literal -> Literal -> State Instance Literal
xor a b
	| a == true && b == true = undefined
	| a == false = return b
	| b == false = return a
	| a == not b = return true
	| a == b = do
		clause [- a]
		return false
	| otherwise = do
		c <- literal
		clause [a, b, not c]
		clause [not a, c]
		clause [not b, c]
		clause [not a, not b]
		return c

assert :: Literal -> State Instance ()
assert a = clause [a]

assertEqu :: Literal -> Literal -> State Instance ()
assertEqu a b = do
	clause [a, not b]
	clause [not a, b]
	return ()

assertLeq :: Literal -> Literal -> State Instance ()
assertLeq a b = clause [not a, b]

literals :: Instance -> Int
literals (MakeInst ls _) = ls

clauses :: Instance -> [[Int]]
clauses (MakeInst _ cs) = cs

answer :: [Int] -> Int -> Bool
answer as =
	let	a = Set.fromList as
		f x = let y = Set.member x a in Exception.assert (y || Set.member (-x ) a) y
	in f

solve1 :: ([Literal], Instance) -> Maybe [Bool]
solve1 (ls, i) = case Picosat.unsafeSolve (clauses i) of
	Picosat.Solution as -> Just $ fmap (answer as) ls
	Picosat.Unsatisfiable -> Nothing
	Picosat.Unknown -> error "picosat failed"

solveOne :: Problem -> Maybe [Bool]
solveOne p = solve1 $ runState p (MakeInst 1 [[true]])

exclude :: Instance -> [Bool] -> [Literal] -> Instance
exclude (MakeInst ls cs) bs xs = MakeInst ls (fmap (uncurry liftAdd) (zip bs xs) : cs)

solve2 :: ([Literal], Instance) -> [[Bool]]
solve2 (ls, i) = case solve1 (ls, i) of
	Nothing -> []
	Just bs -> bs : solve2 (ls, exclude i bs ls)

solveAll :: Problem -> [[Bool]]
solveAll p = solve2 $ runState p (MakeInst 1 [[true]])
