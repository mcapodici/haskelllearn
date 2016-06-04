module MVarsExample1 where

import Control.Concurrent
import Control.Monad
import System.IO

exampleMain = do
  hSetBuffering stdout NoBuffering
  x <- newMVar 0
  forkIO $ forever $ loop "A" x
  loop "B" x

loop name mvar = forever $ do
  n <- takeMVar mvar
  putStrLn $ "Thread " ++ name ++ " setting to " ++ (show n)
  putMVar mvar (n + 1)

-- This example has 2 threads. Each thread is passed the new mvar, and will attemp to get
-- a lock and increment the held number, whilst reporting the new value and which thread
-- to the user. When run the increment and putStrLn is in a 'critical section' effectively
-- so the increments are thread safe and you see 1,2,3,... as expected, but which thread
-- gets to be the the one is not necessarily deterministic, but most often will be taken in turns
-- due to the round robin scheduling in Haskell along with the fait queuing mechanism it uses
-- (I think!)



