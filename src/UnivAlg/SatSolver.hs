module UnivAlg.SatSolver (Literal, Instance, literal, clause, true, false,
	not, or, and, leq, equ, add, xor, assert, assertequ, assertleq,
	empty, generate, clauses, literals, solve) where

import Prelude hiding (not, or, and)
import Control.Monad.State (State, state, execState)
import Control.Monad (liftM)
import qualified Control.Exception as Exception
import qualified Picosat
import qualified Data.Set as Set

type Literal = Int
data Instance = MakeInst Int [[Int]]
	deriving (Show, Eq)

literal :: State Instance Literal
literal = state $ \(MakeInst ls cs) -> (ls + 1, MakeInst (ls + 1) cs)

clause :: [Literal] -> State Instance ()
clause c = state $ \(MakeInst ls cs) -> ((), MakeInst ls (c : cs))

true :: Literal
true = 1

false :: Literal
false = not true

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

and :: Literal -> Literal -> State Instance Literal
and a b = liftM not (or (not a) (not b))

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

add :: Literal -> Literal -> State Instance Literal
add a = equ (not a)

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

assertequ :: Literal -> Literal -> State Instance ()
assertequ a b = do
	clause [a, not b]
	clause [not a, b]
	return ()

assertleq :: Literal -> Literal -> State Instance ()
assertleq a b = clause [not a, b]

empty :: Instance
empty = MakeInst 1 [[1]]

generate :: Instance -> State Instance a -> Instance
generate i p = execState p i

literals :: Instance -> Int
literals (MakeInst ls _) = ls

clauses :: Instance -> [[Int]]
clauses (MakeInst _ cs) = cs

answer :: [Int] -> Int -> Bool
answer as =
	let	a = Set.fromList as
		f x = let y = Set.member x a in Exception.assert (y || Set.member (-x ) a) y
	in f

solve :: Instance -> Maybe (Literal -> Bool)
solve i =
	case Picosat.unsafeSolve (clauses i) of
		Picosat.Solution as -> Just $ answer as
		Picosat.Unsatisfiable -> Nothing
		Picosat.Unknown -> undefined
