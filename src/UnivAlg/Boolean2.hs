{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module UnivAlg.Boolean2 (Boolean(..), evaluate) where

import qualified Prelude
import Prelude hiding (not, or, and)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Applicative ((<$>), Applicative)

class (Monad m, Applicative m) => Boolean m b | b -> m where
	false :: b
	false = not true
	true :: b
	true = lift True
	lift :: Bool -> b
	lift x = if x then true else false
	not :: b -> b
	or :: b -> b -> m b
	or x y = not <$> and (not x) (not y)
	and :: b -> b -> m b
	and x y = not <$> or (not x) (not y)
	leq :: b -> b -> m b
	leq x = or (not x)
	equ :: b -> b -> m b
	equ x = add (not x)
	add :: b -> b -> m b
	add x = equ (not x)

instance Boolean Identity Bool where
	lift = id
	not = Prelude.not
	and x y = return $ x && y
	equ x y = return $ x == y

evaluate :: ([Bool] -> Identity Bool) -> [Bool] -> Bool
evaluate f xs = runIdentity $ f xs
