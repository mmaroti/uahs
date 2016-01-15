module Main where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad.State.Strict (evalState)
import Control.Monad (liftM)

len :: Int
len = 10000000

vec1 :: Monad m => m (Vector Int)
vec1 = Vector.generateM len return

vec2 :: Monad m => m (Vector Int)
vec2 = liftM Vector.fromList (mapM' return [1 .. len] [])

main :: IO ()
main = do
	let y =  evalState vec2 ()
	print $ Vector.length y

mapM' :: Monad m => (a -> m b) -> [a] -> [b] -> m [b]
mapM' _ [] ys = return (reverse ys)
mapM' f (x : xs) ys = f x >>= \y -> mapM' f xs (y : ys)
