module UnivAlg.Semiring (Semiring, plus, neg, zero, prod, one) where

class Semiring r where
	plus :: r -> r -> r
	neg :: r -> r
	zero :: r
	prod :: r -> r -> r
	one :: r

instance Semiring Int where
	plus = (+)
	neg = negate
	zero = 0
	prod = (*)
	one = 1

instance Semiring Double where
	plus = (+)
	neg = negate
	zero = 0.0
	prod = (*)
	one = 1.0

instance Semiring Bool where
	plus = (||)
	neg = id
	zero = False
	prod = (&&)
	one = True
