import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import System.Environment
import Text.Printf

main = do
  [n] <- getArgs
  let test = [test1, test2, test3, test4] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0

x = 36 :: Integer
y = 35 :: Integer

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

f = fib

test1 = do
  x <- rpar (f x)
  y <- rpar (f y)
  return (x,y)

test2 = do
  x <- rpar (f x)
  y <- rseq (f y)
  return (x,y)

test3 = do
  x <- rpar (f x)
  y <- rseq (f y)
  rseq x
  return (x,y)

test4 = do
  x <- rpar (f x)
  y <- rpar (f y)
  rseq x
  rseq y
  return (x,y)

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
