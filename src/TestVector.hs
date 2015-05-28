module Main where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad.State.Strict (evalState)
import Control.Monad (liftM)

len :: Int
len = 1000000

vec1 :: Monad m => m (Vector Int)
vec1 = Vector.generateM len return

vec2 :: Vector Int
vec2 = evalState vec1 ()

vec3 :: Monad m => m [Int]
vec3 = mapM' return [1 .. len] []

vec4 :: Monad m => m (Vector Int)
vec4 = liftM Vector.fromList $ vec3

vec5 :: Vector Int
vec5 = evalState vec4 ()

main :: IO ()
main = do
	x <- vec1
	print $ Vector.length x
	z <- vec4
	print $ Vector.length z
--	let y =  vec5
	let y =  vec2
	print $ Vector.length y

mapM' :: Monad m => (a -> m b) -> [a] -> [b] -> m [b]
mapM' _ [] ys = return (reverse ys)
mapM' f (x : xs) ys = f x >>= \y -> mapM' f xs (y : ys)
