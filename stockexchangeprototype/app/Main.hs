module Main where

import Control.Concurrent
import SEP.Data
import Data.Set
import Control.Monad
import Data.DateTime

main :: IO ()
main = do
  putStrLn "Prototype Stock Exchange"
  putStrLn "Press x to exit"
  sxState <- orderProcessor
  forkIO $ tradingBot "1" (orderChannel sxState) 
  waitForX

waitForX :: IO()
waitForX = do
  x <- getLine
  if (x /= "x" && x /= "X") 
    then waitForX
    else putStrLn "Exited"

data SXState = SXState {
  orderChannel :: Chan BuyOrSellOrder
  }


tradingBot :: String -> Chan BuyOrSellOrder -> IO () -- Trading bot than runs in the same process as the exhange. Can't get more high frequency than that!
tradingBot clientid channel = forever $ do
  time <- getCurrentTime
  let order = BOrder $ BuyOrder 1000 10 time  
  writeChan channel order -- Placing the order
  threadDelay 1000000 -- 1 second delay 

-- Thread that takes valuse from channel of orders, processes the order, updates the queue
orderProcessor :: IO SXState
orderProcessor = do
  channel <- newChan
  forkIO $ loop (OrderCollection empty empty) channel
  return $ SXState channel
    where 
      loop oc channel = do 
        nextOrder <- readChan channel
        let newOc = processOrder nextOrder oc
        putStrLn $ show newOc
        loop newOc channel

processOrder :: BuyOrSellOrder -> OrderCollection -> OrderCollection
processOrder = queueOrder
 -- TODO sometimes you can fill the order immediately and skip queueing



