module SetAlg.Term (
	Term(..),
	verify,
	domain,
	evaluate,
) where

import qualified SetAlg.Domain as Domain
import qualified SetAlg.Element as Element

data Term
	= Literal Domain.Domain Element.Element
	| Apply Term Term
--	| Variable String
--	| LetIn String Term Term
--	| ForAll String Domain.Domain Term
	deriving (Show, Eq)

-- verify

verify :: Term -> Maybe String
verify t =
	case verify2 t of
		Left s -> Just s
		Right _ -> Nothing

verify2 :: Term -> Either String Domain.Domain
verify2 (Literal d e) =
	if Element.representable d then
		case Element.verify d e of
			Just s -> Left s
			Nothing -> Right d
	else
		Left "non-representable literal"
verify2 (Apply t1 t2) =
	case verify2 t1 of
		Left s -> Left s
		Right d -> case d of
			Domain.Power d1 d2 ->
				case verify2 t2 of
					Left s -> Left s
					Right d3 ->
						if d2 /= d3 then
							Left "argument mismatch"
						else
							Right d1
			_ -> Left "non-applicable term"

-- domain

domain :: Term -> Domain.Domain
domain (Literal d _) = d
domain (Apply t1 _) =
	case domain t1 of
		Domain.Power _ d2 -> d2
		_ -> undefined

-- evaluate

evaluate :: Term -> Element.Element
evaluate (Literal _ e) = e
evaluate (Apply t1 t2) =
	Element.apply (domain t1) (evaluate t1) (evaluate t2)
