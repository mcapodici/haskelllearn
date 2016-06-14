module Main where

import Control.Concurrent
import SEP.Data
import Data.Set
import Control.Monad
import Data.DateTime
import System.Random

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
  price <- randomRIO (900,1100)
  qty <- randomRIO (10,40)
  orderType <- randomRIO (0, 1) :: IO Int
  let order = if orderType == 1 then BOrder (BuyOrder price qty time) else SOrder (SellOrder price qty time)
  writeChan channel order -- Placing tihe order
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
        putStrLn "Processing Order"
        putStrLn $ "Incoming Order " ++ (show nextOrder)
        let (newOc, trades) = processOrder nextOrder oc
        putStrLn "OrderCollection: "
        putStrLn $ show newOc
        putStrLn "Trades executed: "
        putStrLn $ show trades
        putStrLn "-------------------------------------------------------------"
        loop newOc channel

processOrder :: BuyOrSellOrder -> OrderCollection -> (OrderCollection, [Trade])
processOrder order collection =
  case order of
    BOrder bOrder -> 
      let currSellOrders = sellQueue collection in 
      case maxView currSellOrders of
        Nothing -> (queueOrder order collection, []) -- No sell orders at all, so no match possible just queue the buy order
        Just (mso, remSellOrders) -> -- Matched Sell Order and Remaining sell orders
          if canMatch bOrder mso then
            let matchedQty = min (order_quantity mso) (order_quantity order) in
            let msoNewQty = order_quantity mso - matchedQty in 
            let orderNewQty = order_quantity order - matchedQty in
            let trade = Trade (choosePrice (order_price order) (order_price mso)) matchedQty in
            case msoNewQty of
              0 -> case orderNewQty of
                      0 -> (OrderCollection (buyQueue collection) remSellOrders, [trade]) -- perfect match - just remove the corresponding order
                      _ -> let result = processOrder (BOrder $ BuyOrder (order_price order) orderNewQty (order_creationTimeUtc order)) (OrderCollection (buyQueue collection) remSellOrders) in
                           (fst result, (trade : (snd result)))
                              -- ^^ Some order left unfulfilled, process the remainder of that order with the remainder of the order queue
              _ -> let newSellOrder = SellOrder (order_price mso) msoNewQty (order_creationTimeUtc mso) in
                   (OrderCollection (buyQueue collection) (insert newSellOrder (delete mso currSellOrders)), [trade]) -- Doesn't require all of the matched order, so
                     -- just updated that matched order to remove the quantity matched in this trade
          else
            (queueOrder order collection, []) -- No sell orders that can match the buy order

    SOrder x -> (queueOrder order collection, []) -- todo implement DRY

canMatch :: BuyOrder -> SellOrder -> Bool
canMatch b s = (order_price b) >= (order_price s)

choosePrice :: Price -> Price -> Price
choosePrice buyPrice sellPrice = floor $ ((fromIntegral buyPrice) + (fromIntegral sellPrice)) / 2

-- TODO sometimes you can fill the order immediately and skip queueing



