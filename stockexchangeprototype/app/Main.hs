{-# LANGUAGE OverloadedStrings #-}

-- TODO:
--
-- Generally tease out functionality from Main.hs into seperate modules to make it easier to follow
-- Reduce use of IO
-- Property based and spec based tests
-- STM?
-- Handle exceptions and close file handle when writing with TradeWriter
-- Circular writing of trade files
-- Accounting - checking balances before allowing trades, keeping track of balances and number of shares, transfers in/out etc
-- Decide if we want to write to a DB and implement that
-- Exception handling in general, both synchronous and asynchronous
-- Security - how to we authenticate users?

module Main where

import Prelude hiding (putStrLn, init)
import Control.Concurrent
import Control.Monad
import Data.DateTime
import Data.Set
import Data.Foldable
import Network.Socket
import SEP.Data
import SEP.OrderBook
import SEP.Parser
import SEP.TradeWriter
import System.IO (hSetBuffering, IOMode(..), BufferMode(..))
import System.Random
import qualified Data.Map.Strict as SMap
import qualified Data.Text.Lazy.IO as TextLzIO
import qualified Data.Text.Lazy as TextLz
import qualified Data.Text.Format as Text

main :: IO ()
main = do
  TextLzIO.putStrLn "Prototype Stock Exchange"
  tradeWriter <- getTradeWriter
  sxState <- orderProcessor tradeWriter
  --forkIO $ tradingBot (-1) (orderChannel sxState) 
  sock <- initialiseNetwork
  mainLoop sock (orderChannel sxState) 1

data SXRequest
  = SXRequestOrder Order
  | RegisterClientChannel ClientIdentifier (Chan TradeConfirmation)

data SXState = SXState {
  orderChannel :: Chan SXRequest
  }

mainLoop :: Socket -> Chan SXRequest -> Int -> IO ()
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

runTraderConnection :: (Socket, SockAddr) -> Chan SXRequest -> ClientIdentifier -> IO ()
runTraderConnection (connectedSock, _) channel clientId = do
  hdl <- socketToHandle connectedSock ReadWriteMode
  hSetBuffering hdl NoBuffering
  confirmationChannel <- newChan
  writeChan channel (RegisterClientChannel clientId confirmationChannel)

  -- Additional thread: Communicaion of trade confirmations
  forkIO $ forever $ do
    confirmation <- readChan confirmationChannel 
    TextLzIO.hPutStrLn hdl $ 
      Text.format "Trade confirmation: {} {}@{}" 
      (if tradeConfirmationDirection confirmation == Buy then "B" else "S" :: TextLz.Text, 
      tradeConfirmationQuantity confirmation, 
      tradeConfirmationPrice confirmation)

  forever $ do
    maybeUnstampedOrder <- parseUnstampedOrder . TextLz.init <$> TextLzIO.hGetLine hdl
    case maybeUnstampedOrder of
      Nothing -> TextLzIO.hPutStrLn hdl "Invalid order syntax"
      Just uo -> do
        time <- getCurrentTime
        writeChan channel (SXRequestOrder $ uo time clientId) -- Placing tihe order

-- do stuf
  return ()
  

tradingBot :: ClientIdentifier -> Chan SXRequest -> IO () -- Trading bot than runs in the same process as the exhange. Can't get more high frequency than that!
tradingBot clientId channel = do
  confirmationChannel <- newChan
  writeChan channel (RegisterClientChannel clientId confirmationChannel)
  forever $ do
    time <- getCurrentTime
    orderTypeInt <- randomRIO (0, 1) :: IO Int
    let orderType = if orderTypeInt == 0 then Buy else Sell
    price <- case orderType of
      Buy -> randomRIO (999,1050)
      Sell -> randomRIO (950, 1001)
    qty <- randomRIO (10,40)
    let order = Order orderType price qty time clientId
    writeChan channel (SXRequestOrder order) -- Placing tihe order
    threadDelay 10000 -- 0.01 second delay 

-- Thread that takes valuse from channel of orders, processes the order, updates the queue
orderProcessor :: TradeWriter -> IO SXState
orderProcessor tradeWriter = do
  channel <- newChan
  forkIO $ loop (emptyBook, SMap.empty, 1) channel
  return $ SXState channel
    where
      -- Todo need to ensure order collections are strict!
      loop :: (OrderBook, SMap.Map ClientIdentifier (Chan TradeConfirmation), Int) -> Chan SXRequest -> IO()
      loop (book, tradeChannels, i) channel = do 
        nextRequest <- readChan channel
        case nextRequest of
          SXRequestOrder nextOrder -> do
            let (newOc, trades) = processOrder nextOrder book
            forM_ trades (\trade -> 
              let sendConfirmation clientId = case SMap.lookup clientId tradeChannels of
                                                Nothing -> return ()
                                                Just confirmationChannel -> forM_ (tradeToConfirmation clientId trade) (writeChan confirmationChannel)
              in 
                writeTrade tradeWriter trade >>
                sendConfirmation (tradeBuyClient trade) >> 
                sendConfirmation (tradeSellClient trade)
              )
            loop (newOc, tradeChannels, i+1) channel
          RegisterClientChannel clientId chan -> loop (book, SMap.insert clientId chan tradeChannels, i) channel

processOrder :: Order -> OrderBook -> (OrderBook, [Trade])
processOrder order book =
  let maybeMatchOrder = case orderDirection order of
                          Buy -> bestSellOrder book
                          Sell -> bestBuyOrder book in
  case maybeMatchOrder of
    Nothing -> (queueOrder order book, []) -- No candidate matching orders at all, so just queue this order for now
    Just mo -> 
      if canMatch order mo then
        let matchedQty = min (orderQuantity mo) (orderQuantity order) in
        let moNewQty = orderQuantity mo - matchedQty in 
        let orderNewQty = orderQuantity order - matchedQty in
        let (buyClientId, sellClientId) = case orderDirection order of 
                                          Buy -> (orderClientId order, orderClientId mo)
                                          Sell -> (orderClientId mo, orderClientId order) in
        let trade = Trade (choosePrice (orderPrice order) (orderPrice mo)) matchedQty buyClientId sellClientId in
        let bookWithoutMo = removeOrder mo book in
        case moNewQty of
          0 -> 
            case orderNewQty of
              -- perfect match - just remove the corresponding order
              0 -> (bookWithoutMo, [trade])
                      
              -- Some order left unfulfilled, process the remainder of that order with the remainder of the order queue
              _ -> let (newBook, ts) = processOrder (adjustQuantity orderNewQty order) bookWithoutMo in
                     (newBook, trade : ts)

          -- Didn't require all of the matched order, so just update the matched order quantity
          _ -> (queueOrder (adjustQuantity moNewQty mo) bookWithoutMo, [trade])
      else
        (queueOrder order book, []) -- No candidate matching orders with suitable price, so just queue this order for now


canMatch :: Order -> Order -> Bool
canMatch x y =
  if orderDirection x == Buy 
    then orderPrice x >= orderPrice y
    else orderPrice x <= orderPrice y

choosePrice :: Price -> Price -> Price
choosePrice buyPrice sellPrice = floor $ (fromIntegral buyPrice + fromIntegral sellPrice) / 2
