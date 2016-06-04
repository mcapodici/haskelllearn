module DIYChan where

import Control.Concurrent hiding (Chan, readMVar, newChan, readChan, writeChan)
import Control.Monad
import System.IO

exampleMain :: IO ()
exampleMain = do
  sx <- stockExchange
  forM [1..10] (\id -> forkIO $ tradingBot (show id) sx)
  getLine
  return ()

type Order = String -- Repesents an order for the stock exhange from a client. Shoddily implemented as a string!

tradingBot :: String -> Chan Order -> IO () -- Trading bot than runs in the same process as the exhange. Can't get more high frequency than that!
tradingBot clientid sx = forever $ do
  let order = "Buy MSFT $100 200 shares. Client: " ++ clientid -- Clever prop trading algo!
  writeChan sx order -- Placing the order
  threadDelay 1000000 -- 1 second delay 

stockExchange :: IO (Chan Order)
stockExchange = do
  channel <- newChan
  forkIO $ forever $ do
    order <- readChan channel -- Check for new orders
    putStrLn $ "Order placed in stock exhange: " ++ order -- Tell console about the order
    -- TODO: Do stock exchangey stuff here
  return channel

-- Below is reinventing the "Channel" wheel. You can get all this from the base library but
-- it is interesting for pedagogical purposes.

type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)

data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))

newChan :: IO (Chan a)
newChan = do
  hole  <- newEmptyMVar
  readVar  <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole

-- Bad version: Will block on reading for duplicated channels
{--
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  stream <- takeMVar readVar            -- 1
  Item val tail <- takeMVar stream      -- 2
  putMVar readVar tail                  -- 3
  return val
--}
--
-- Good version - Plays nice! Will put back the value of the head MVar so that another channel can read it
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  stream <- takeMVar readVar
  Item val tail <- readMVar stream      -- 1
  putMVar readVar tail
  return val

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole <- readMVar writeVar
  newReadVar <- newMVar hole
  return (Chan newReadVar writeVar)

readMVar :: MVar a -> IO a
readMVar m = do
  a <- takeMVar m
  putMVar m a
  return a



