{-# LANGUAGE OverloadedStrings #-}

module SEP.TradeWriter (TradeWriter, getTradeWriter, writeTrade) where

import Control.Concurrent
import Control.Monad
import Data.Text.Format as T
import Data.Text.Lazy (Text)
import SEP.Data
import System.IO
import qualified Data.Text.Lazy.IO as DTLIO

{-|
  TradeWriter provides an abstract ability to write a trade to a permanent
  data store.
-}
data TradeWriter = TW { writeTrade :: Trade -> IO () }

{-|
  Gets a new trade writer. Should only be called once as each call will use
  thread resoruces and different instances may conflict.
-}
getTradeWriter :: IO TradeWriter
getTradeWriter =
  do
    channel <- newChan
    fileHandle <- openFile "trades.txt" AppendMode
    hSetBuffering fileHandle NoBuffering
    forkIO $ tradeWriterLoop channel fileHandle
    return $ TW (writeChan channel)

{-|
  The main IO loop for processing the writing of trades
-}
tradeWriterLoop :: Chan Trade -> Handle -> IO () 
tradeWriterLoop channel fileHandle =
  forever $ do
    trade <- readChan channel
    DTLIO.hPutStrLn fileHandle (tradeToLog trade)
    -- todo catch any exceptions - probably best to just write them to
    -- error output as if we are having trouble just writing to a file
    -- then logging may be an issue!

{-|
  Formats a trade into a string for the purposes of the trade writer.
  Unfettered control over the format is required so (show) isn't sufficent.
-}
tradeToLog :: Trade -> Text
tradeToLog (Trade price qty buyer seller) = T.format "p={},q={},bcid={},scid={}" (price, qty, buyer, seller)

