import Control.Parallel.Strategies hiding (parMap, parList, evalList)

main = do
  let lst = map fib [30 .. 35] `using` parList rseq
  print lst

evalList :: Strategy a -> Strategy [a]
evalList strat []     = return []
evalList strat (x:xs) = do
  x'  <- strat x
  xs' <- evalList strat xs
  return (x':xs)

parList :: Strategy a -> Strategy [a]
parList strat = evalList $ rparWith strat

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
