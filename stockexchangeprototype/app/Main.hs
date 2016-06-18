module Main where

import System.IO
import Control.Concurrent
import SEP.Data
import Data.Set
import Control.Monad
import Data.DateTime
import System.Random
import Network.Socket
import SEP.Parser

main :: IO ()
main = do
  putStrLn "Prototype Stock Exchange"
  putStrLn "Press x to exit"
  sxState <- orderProcessor
  forkIO $ tradingBot 1 (orderChannel sxState) 
  sock <- initialiseNetwork
  mainLoop sock (orderChannel sxState) 1

data SXState = SXState {
  orderChannel :: Chan BuyOrSellOrderWithConfirmationChannel
  }

mainLoop :: Socket -> Chan BuyOrSellOrderWithConfirmationChannel -> Int -> IO ()
mainLoop sock chan clientId = do
  conn <- accept sock
  forkIO (runTraderConnection conn chan clientId)
  mainLoop sock chan $! clientId + 1

initialiseNetwork :: IO Socket
initialiseNetwork = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  return sock

runTraderConnection :: (Socket, SockAddr) -> Chan BuyOrSellOrderWithConfirmationChannel -> ClientIdentifier -> IO ()
runTraderConnection (connectedSock, _) channel clientId = do
  hdl <- socketToHandle connectedSock ReadWriteMode
  hSetBuffering hdl NoBuffering
  confirmationChannel <- newChan
  
  -- Additional thread: Communicaion of trade confirmations
  forkIO $ forever $ do
    confirmation <- readChan confirmationChannel 
    hPutStrLn hdl $ "Trade confirmation: " ++ (show confirmation)

  -- This thread: Taking of orders loop
  forever $ do
    maybeUnstampedOrder <- parseUnstampedOrder . init <$> hGetLine hdl
    case maybeUnstampedOrder of
      Nothing -> hPutStrLn hdl "Invalid order syntax"
      Just uo -> do
        time <- getCurrentTime
        writeChan channel (BuyOrSellOrderWithConfirmationChannel (uo time clientId) confirmationChannel) -- Placing tihe order

-- do stuf
  return ()
  

tradingBot :: ClientIdentifier -> Chan BuyOrSellOrderWithConfirmationChannel -> IO () -- Trading bot than runs in the same process as the exhange. Can't get more high frequency than that!
tradingBot clientid channel = do
  confirmationChannel <- newChan
{--  forkIO $ forever $ do
    confirmation <- readChan confirmationChannel 
    putStr "Trade confirmation: "
    putStrLn $ show confirmation
    putStrLn "-------------------------------------------------------------" --}
  forever $ do
    time <- getCurrentTime
    price <- randomRIO (900,1100)
    qty <- randomRIO (10,40)
    orderType <- randomRIO (0, 1) :: IO Int
    let order = if orderType == 1 then BOrder (BuyOrder price qty time clientid) else SOrder (SellOrder price qty time clientid)
    writeChan channel (BuyOrSellOrderWithConfirmationChannel order confirmationChannel) -- Placing tihe order
    threadDelay 10000 -- 0.01 second delay 

-- Thread that takes valuse from channel of orders, processes the order, updates the queue
orderProcessor :: IO SXState
orderProcessor = do
  channel <- newChan
  forkIO $ loop ((OrderCollection empty empty), 1) channel
  return $ SXState channel
    where 
      loop :: (OrderCollection, Int) -> Chan BuyOrSellOrderWithConfirmationChannel -> IO()
      loop (oc, i) channel = do 
        BuyOrSellOrderWithConfirmationChannel nextOrder confirmationChannel <- readChan channel
        let (newOc, trades) = processOrder nextOrder oc
{--        putStrLn $ "Processing Order #" ++ (show i)
        putStrLn $ "Incoming Order " ++ (show nextOrder)
        putStrLn "OrderCollection: "
        putStrLn $ show newOc
        putStrLn "Trades executed: "
        putStrLn $ show trades
        putStrLn "-------------------------------------------------------------"--}
        mapM_ (writeChan confirmationChannel) trades
        loop (newOc, i+1) channel

processOrder :: BuyOrSellOrder -> OrderCollection -> (OrderCollection, [Trade])
processOrder (BOrder order) oc = 
  let (bq, sq, trades) = processOrder' order (buyQueue oc) (sellQueue oc) 
  in (OrderCollection bq sq, trades)

processOrder (SOrder order) oc = 
  let (sq, bq, trades) = processOrder' order (sellQueue oc) (buyQueue oc) 
  in (OrderCollection bq sq, trades)

processOrder' :: (Order o, Order p, QuantityAdjustable o, QuantityAdjustable p) => o -> Set o -> Set p -> (Set o, Set p, [Trade])
processOrder' order queue matchQueue =
  case maxView matchQueue of
    Nothing -> (insert order queue, matchQueue, []) -- No candidate matching orders at all, so just queue this order for now
    Just (mo {-- matched order --}, rmq {-- remaining match queue --}) -> 
      if canMatch order mo then
        let matchedQty = min (order_quantity mo) (order_quantity order) in
        let moNewQty = order_quantity mo - matchedQty in 
        let orderNewQty = order_quantity order - matchedQty in
        let trade = Trade (choosePrice (order_price order) (order_price mo)) matchedQty in
        case moNewQty of
          0 -> case orderNewQty of
                  0 -> (queue, rmq, [trade]) -- perfect match - just remove the corresponding order
                  _ -> let (q, mq, ts) = processOrder' (adjustQuantity orderNewQty order) queue rmq in
                       (q, mq, trade : ts)
                          -- ^^ Some order left unfulfilled, process the remainder of that order with the remainder of the order queue
          _ -> (queue, insert (adjustQuantity moNewQty mo) rmq, [trade]) -- Doesn't require all of the matched order, so
                 -- just updated that matched order to remove the quantity matched in this trade
      else
        (insert order queue, matchQueue, []) -- No candidate matching orders with suitable price, so just queue this order for now


canMatch :: (Order a, Order b) => a -> b -> Bool
canMatch x y =
  if (direction x == Buy) 
    then (order_price x) >= (order_price y)
    else (order_price x) <= (order_price y)

choosePrice :: Price -> Price -> Price
choosePrice buyPrice sellPrice = floor $ ((fromIntegral buyPrice) + (fromIntegral sellPrice)) / 2

-- TODO sometimes you can fill the order immediately and skip queueing



