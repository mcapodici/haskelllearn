module SEP.TradeWriter (TradeWriter, getTradeWriter, writeTrade) where

import System.IO
import Control.Monad
import SEP.Data
import Control.Concurrent
import Text.Printf

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
    forkIO $ tradeWriterLoop channel fileHandle
    return $ TW (writeChan channel)

{-|
  The main IO loop for processing the writing of trades
-}
tradeWriterLoop :: Chan Trade -> Handle -> IO () 
tradeWriterLoop channel fileHandle =
  forever $ do
    trade <- readChan channel
    hPutStrLn fileHandle (tradeToLog trade)
    -- todo catch any exceptions - probably best to just write them to
    -- error output as if we are having trouble just writing to a file
    -- then logging may be an issue!

{-|
  Formats a trade into a string for the purposes of the trade writer.
  Unfettered control over the format is required so (show) isn't sufficent.
-}
tradeToLog :: Trade -> String
tradeToLog (Trade price qty buyer seller) = printf "p=%d,q=%d,bcid=%d,scid=%d" price qty buyer seller

