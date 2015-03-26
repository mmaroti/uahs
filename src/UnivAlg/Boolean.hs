
module UnivAlg.Boolean (Boolean(..)) where

import qualified Prelude

infixr 3 &&
infixr 2 ||
infix 1 `xor`, `equ`, `leq`

class Boolean b where
	true :: b
	false :: b
	not :: b -> b
	(&&) :: b -> b -> b
	(||) :: b -> b -> b
	xor :: b -> b -> b
	equ :: b -> b -> b
	leq :: b -> b -> b
	-- default impl
	true = not false
	false = not true
	x && y = not (x || y)
	x || y = not (x && y)
	x `xor` y = not (x `equ` y)
	x `equ` y = not (x `xor` y)
	x `leq` y = not x || y

instance Boolean Prelude.Bool where
	true = Prelude.True
	false = Prelude.False
	not = Prelude.not
	(&&) = (Prelude.&&)
	(||) = (Prelude.||)
	xor = (Prelude./=)
	equ = (Prelude.==)
