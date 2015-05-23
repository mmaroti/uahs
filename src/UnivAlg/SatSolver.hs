module UnivAlg.SatSolver (Variable, Instance, Problem, variable, clause, true, false,
	lift, not, or, liftOr, and, liftAnd, leq, equ, liftEqu, add, liftAdd, xor,
	assert, assertEqu, assertLeq, generate, clauses, literals, solveOne, solveAll) where

import Prelude hiding (not, or, and)
import Control.Monad.State (State, state, runState)
import Control.Monad (liftM)
import qualified Data.Set as Set
import qualified Picosat

type Variable = Int
data Instance = MakeInst Int [[Int]]
	deriving (Show, Eq)
type Problem = State Instance [Variable]

variable :: State Instance Variable
variable = state $ \(MakeInst ls cs) -> (ls + 1, MakeInst (ls + 1) cs)

clause :: [Variable] -> State Instance ()
clause c = state $ \(MakeInst ls cs) -> ((), MakeInst ls (c : cs))

true :: Variable
true = 1

false :: Variable
false = not true

lift :: Bool -> Variable
lift b = if b then true else false

not :: Variable -> Variable
not a = - a

or :: Variable -> Variable -> State Instance Variable
or a b
	| a == false = return b
	| a == true || a == not b = return true
	| b == false || a == b = return a
	| otherwise = do
		c <- variable
		clause [not a, c]
		clause [not b, c]
		clause [a, b, not c]
		return c

liftOr :: Bool -> Variable -> Variable
liftOr a b = if a then true else b

and :: Variable -> Variable -> State Instance Variable
and a b = liftM not (or (not a) (not b))

liftAnd :: Bool -> Variable -> Variable
liftAnd a b = if a then b else false

leq :: Variable -> Variable -> State Instance Variable
leq a = or (not a)

equ :: Variable -> Variable -> State Instance Variable
equ a b
	| a == true = return b
	| a == false = return (not b)
	| b == true = return a
	| b == false = return (not a)
	| a == b = return true
	| a == not b = return false
	| otherwise = do
		c <- variable
		clause [a, b, c]
		clause [a, not b, not c]
		clause [not a, b, not c]
		clause [not a, not b, c]
		return c

liftEqu :: Bool -> Variable -> Variable
liftEqu a b = if a then b else not b

add :: Variable -> Variable -> State Instance Variable
add a = equ (not a)

liftAdd :: Bool -> Variable -> Variable
liftAdd a b = if a then not b else b

-- xor 1 1 is undefined
xor :: Variable -> Variable -> State Instance Variable
xor a b
	| a == true && b == true = undefined
	| a == false = return b
	| b == false = return a
	| a == not b = return true
	| a == b = do
		clause [- a]
		return false
	| otherwise = do
		c <- variable
		clause [a, b, not c]
		clause [not a, c]
		clause [not b, c]
		clause [not a, not b]
		return c

assert :: Variable -> State Instance ()
assert a = clause [a]

assertEqu :: Variable -> Variable -> State Instance ()
assertEqu a b = do
	clause [a, not b]
	clause [not a, b]
	return ()

assertLeq :: Variable -> Variable -> State Instance ()
assertLeq a b = clause [not a, b]

generate :: State Instance a -> (a, Instance)
generate p = runState p (MakeInst 1 [[true]])

literals :: Instance -> Int
literals (MakeInst ls _) = ls

clauses :: Instance -> [[Int]]
clauses (MakeInst _ cs) = cs

answer :: [Int] -> Int -> Bool
answer as =
	let	a = Set.fromList $ filter (> 0) as
	in (`Set.member` a)

solveOne :: ([Variable], Instance) -> Maybe [Bool]
solveOne (ls, i) = case Picosat.unsafeSolve (clauses i) of
	Picosat.Solution as -> Just $ fmap (answer as) ls
	Picosat.Unsatisfiable -> Nothing
	Picosat.Unknown -> error "picosat failed"

exclude :: Instance -> [Bool] -> [Variable] -> Instance
exclude (MakeInst ls cs) bs xs = MakeInst ls (fmap (uncurry liftAdd) (zip bs xs) : cs)

solveAll :: ([Variable], Instance) -> [[Bool]]
solveAll (ls, i) = case solveOne (ls, i) of
	Nothing -> []
	Just bs -> bs : solveAll (ls, exclude i bs ls)
