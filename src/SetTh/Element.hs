module SetTh.Element (
	Element(..),
	verify,
	apply,
	graph,
	index,
	lookup,
) where

import Prelude hiding (lookup)
import qualified Data.Foldable as Foldable
import qualified SetTh.Domain as Domain

data Element
	= Finite Int
	| Union Int Element
	| Product [Element]
	| Power [Element]
	deriving (Show, Eq)

verify :: Domain.Domain -> Element -> Maybe String
verify (Domain.Finite s) (Finite i) =
	if i < 0 || toInteger i >= s then
		Just "invalid finite index"
	else
		Nothing
verify (Domain.Union ds) (Union i e) =
	if i < 0 || i >= length ds then
		Just "invalid union index"
	else
		verify (ds !! i) e
verify (Domain.Product ds) (Product es) =
	if length ds /= length es then
		Just "invalid product length"
	else
		Foldable.asum (zipWith verify ds es)
verify (Domain.Power d1 d2) (Power es) =
	if Domain.size d2 /= toInteger (length es) then
		Just "invalid power length"
	else
		Foldable.asum (map (verify d1) es)
verify _ _ =
	Just "type mismatch"

apply :: Domain.Domain -> Element -> Element -> Element
apply (Domain.Power _ d2) (Power es) =
	\e -> let i = index d2 e in es !! i
apply _ _ = undefined

-- (Power d1 d2) -> (d2 -> d1) -> (Power d1 d2)
graph :: Domain.Domain -> (Element -> Element) -> Element
graph (Domain.Power d1 d2) f = undefined
graph _ _ = undefined

index :: Domain.Domain -> Element -> Int
index (Domain.Finite _) (Finite i) = i
index (Domain.Union ds) (Union i e) =
	sum (map (fromInteger . Domain.size) (take (i - 1) ds)) + index (ds !! i) e
index (Domain.Product ds) (Product es) =
	undefined
index _ _ = undefined

lookup :: Domain.Domain -> Integer -> Element
lookup = undefined
