module SetAlg.Domain (
	Domain(..),
	size,
	verify,
	format,
) where

import Control.Applicative ((<|>))

data Domain
	= Basic Integer
	| Union Domain Domain
	| Product Domain Domain
	| Power Domain Domain
	deriving (Show, Eq)

-- size

size :: Domain -> Integer
size (Basic s) = s
size (Union d1 d2) = size d1 + size d2
size (Product d1 d2) = size d1 * size d2
size (Power d1 d2) = size d1 ^ size d2

-- verify

verify :: Domain -> Maybe String
verify (Basic s) =
	if s < 0 then Just "negative size" else Nothing
verify (Union d1 d2) = verify d1 <|> verify d2
verify (Product d1 d2) = verify d1 <|> verify d2
verify (Power d1 d2) = verify d1 <|> verify d2

-- format

format :: Domain -> String
format d = format2 d 0

format2 :: Domain -> Int -> String
format2 (Basic s) = \_ -> show s
format2 (Union d1 d2) = format3 '+' 10 d1 d2
format2 (Product d1 d2) = format3  '*' 20 d1 d2
format2 (Power d1 d2) = format3 '^' 30 d1 d2

format3 :: Char -> Int -> Domain -> Domain -> Int -> String
format3 c q d1 d2 p =
	let s = format2 d1 q ++ [c] ++ format2 d2 (q + 1) in
	if p <= q then s else "(" ++ s ++ ")"
