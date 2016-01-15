module Main where

--import Control.Monad (forM)
import UnivAlg.MyPicosat as Picosat

vars :: Int
vars = 3000000

clauses :: [[Int]]
clauses = fmap (\x -> [x]) [1 .. vars]

main :: IO ()
main = do
	putStrLn "Running new forM':"
	sol1 <- forM' [1 .. vars] return
	print $ last sol1
--	putStrLn "Running old forM:"
--	sol2 <- forM [1 .. vars] return
--	print $ last sol2
	putStrLn $ "Running Picosat with " ++ show (length clauses) ++ " clauses:"
	sol <- Picosat.solve clauses
	print $ length (show sol)

mapM' :: Monad m => (a -> m b) -> [a] -> [b] -> m [b]
mapM' _ [] ys = return (reverse ys)
mapM' f (x : xs) ys = f x >>= \y -> mapM' f xs (y : ys)

forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' xs f = mapM' f xs []
