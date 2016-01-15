module SetTh.Domain (
	Domain(..),
	size,
	verify,
) where

import qualified Data.Foldable as Foldable
import qualified Control.Applicative as Applicative

data Domain
	= Finite Integer
	| Union [Domain]
	| Product [Domain]
	| Power Domain Domain
	deriving (Show, Eq)

size :: Domain -> Integer
size (Finite s) = s
size (Union ds) = sum (map size ds)
size (Product ds) = product (map size ds)
size (Power d1 d2) = size d1 ^ size d2

verify :: Domain -> Maybe String
verify (Finite s) =
	if s < 0 then Just "negative size" else Nothing
verify (Union ds) =
	Foldable.asum (map verify ds)
verify (Product ds) =
	Foldable.asum (map verify ds)
verify (Power d1 d2) =
	(Applicative.<|>) (verify d1) (verify d2)
