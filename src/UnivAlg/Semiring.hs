module UnivAlg.Semiring (Semiring, plus, zero, prod, one) where

class Semiring r where
	plus :: r -> r -> r
	zero :: r
	prod :: r -> r -> r
	one :: r

instance Semiring Int where
	plus = (+)
	zero = 0
	prod = (*)
	one = 1

instance Semiring Double where
	plus = (+)
	zero = 0.0
	prod = (*)
	one = 1.0

instance Semiring Bool where
	plus = (||)
	zero = False
	prod = (&&)
	one = True
