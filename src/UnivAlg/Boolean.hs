{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module UnivAlg.Boolean (Boolean(..), all, any, few, one, sum, evaluate) where

import qualified Prelude
import Prelude hiding (not, or, and, all, any, sum)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Applicative ((<$>), Applicative)
import Control.Monad (foldM)

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

all :: Boolean m b => [b] -> m b
all = foldM and true

any :: Boolean m b => [b] -> m b
any = foldM or false

oneStep :: Boolean m b => (b, b) -> b -> m (b, b)
oneStep (x, y) z = do
	u <- x `or` z
	v <- x `and` z
	w <- y `and` not v
	return (u, w)

few :: Boolean m b => [b] -> m b
few xs = snd <$> foldM oneStep (false, true) xs

one :: Boolean m b => [b] -> m b
one xs = uncurry and =<< foldM oneStep (false, true) xs

sum :: Boolean m b => [b] -> m b
sum = foldM add false

instance Boolean Identity Bool where
	lift = id
	not = Prelude.not
	and x y = return $ x && y
	equ x y = return $ x == y

evaluate :: ([Bool] -> Identity Bool) -> [Bool] -> Bool
evaluate f xs = runIdentity $ f xs
