module UnivAlg.SatSolver2 where

import Prelude hiding (not, or, and)

data Tree
	= Empty
	| Literal
	| Clause [Int]
	| Join Int Tree Tree
	deriving (Show, Eq)

clauses :: Tree -> Int
clauses Empty = 0
clauses Literal = 1
clauses (Clause _) = 0
clauses (Join n _ _) = n

data State a = State Tree a

instance Monad State where
	return a = State Empty a
	(State t1 a1) >>= f =
		let (State t2 a2) = f a1
		in State (Join (clauses t1 + clauses t2) t1 t2) a2

literal :: State Int
literal = State Literal 0
