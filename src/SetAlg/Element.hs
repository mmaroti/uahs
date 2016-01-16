module SetAlg.Element (
	Element(..),
	representable,
	verify,
	apply,
	abstract,
	indexable,
	index,
	lookup,
	elements,
) where

import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified SetAlg.Domain as Domain

-- Element

data Element
	= Basic Int
	| Union Bool Element
	| Product Element Element
	| Power [Element]
	deriving (Show, Eq)

-- representable

representable :: Domain.Domain -> Bool
representable (Domain.Basic s) = s <= bound
representable (Domain.Union d1 d2) = representable d1 && representable d2
representable (Domain.Product d1 d2) = representable d1 && representable d2
representable (Domain.Power d1 d2) = representable d1 && indexable d2

bound :: Integer
bound = toInteger (maxBound :: Int)

-- indexable

indexable :: Domain.Domain -> Bool
indexable d = Domain.size d <= bound

-- domainSize

domainSize :: Domain.Domain -> Int
domainSize (Domain.Basic s) = fromInteger s
domainSize (Domain.Union d1 d2) = domainSize d1 + domainSize d2
domainSize (Domain.Product d1 d2) = domainSize d1 * domainSize d2
domainSize (Domain.Power d1 d2) = domainSize d1 ^ domainSize d2

-- verify

verify :: Domain.Domain -> Element -> Maybe String
verify (Domain.Basic s) (Basic i) =
	if i < 0 || i >= fromInteger s then
		Just "invalid finite index"
	else
		Nothing
verify (Domain.Union d1 d2) (Union b e) =
	verify (if b then d2 else d1) e
verify (Domain.Product d1 d2) (Product e1 e2) =
	verify d1 e1 <|> verify d2 e2
verify (Domain.Power d1 d2) (Power es) =
	if domainSize d2 /= length es then
		Just "invalid power length"
	else
		Foldable.asum (map (verify d1) es)
verify _ _ =
	Just "type mismatch"

-- apply

apply :: Domain.Domain -> Element -> Element -> Element
apply (Domain.Power _ d2) (Power es) =
	\e -> let i = index d2 e in es !! i
apply _ _ = undefined

-- define

abstract :: Domain.Domain -> (Element -> Element) -> Element
abstract (Domain.Power _ d2) f =
	Power (map f (elements d2))
abstract _ _ = undefined

-- divide

divide :: Int -> Int -> [Int]
divide d x =
	let (x1, x2) = divMod x d in
	x2 : divide d x1

-- combine

combine :: Int -> [Int] -> Int
combine d = combine2 0 1 where
	combine2 a _ [] = a
	combine2 a m (y : ys) = combine2 (a + m * y) (d * m) ys

-- index

index :: Domain.Domain -> Element -> Int
index (Domain.Basic _) (Basic i) = i
index (Domain.Union d1 d2) (Union b e) =
	if b then
		domainSize d1 + index d2 e
	else
		index d1 e
index (Domain.Product d1 d2) (Product e1 e2) =
	index d1 e1 * domainSize d2 + index d2 e2
index (Domain.Power d1 _) (Power es) =
	combine (domainSize d1) (map (index d1) es)
index _ _ = undefined

-- lookup

lookup :: Domain.Domain -> Int -> Element
lookup (Domain.Basic _) i = Basic i
lookup (Domain.Union d1 d2) i =
	let s = domainSize d1 in
	if i < s then
		Union False (lookup d1 i)
	else
		Union True (lookup d2 (i - s))
lookup (Domain.Product d1 d2) i =
	let (i1, i2) = divMod i (domainSize d2) in
	Product (lookup d1 i1) (lookup d2 i2)
lookup (Domain.Power d1 d2) i =
	let xs = take (domainSize d2) (divide (domainSize d1) i) in
	Power (map (lookup d1) xs)

-- elements

elements :: Domain.Domain -> [Element]
elements d = map (lookup d) (enumFromTo 0 (domainSize d - 1))
