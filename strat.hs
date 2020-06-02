import Control.Parallel.Strategies hiding (parPair)

main = print ((fib 36, fib 35) `using` parPair)

parPair :: Strategy (a,b)
parPair (a,b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
