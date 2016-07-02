module SEP.Data where

import Data.DateTime
import Text.Printf
import Control.Concurrent

data Direction = Buy | Sell deriving (Show, Eq, Ord)

type Price = Int -- Price is represented as integer (allowed range [-2^29, 2^29 - 1]) the exchange deals with the 
                 -- smallest unit, e.g. you may want to allow dollars to 2.d.p then then price is in cents.

type ClientIdentifier = Int

data Order = Order { 
  orderDirection :: Direction,
  orderPrice :: Price, 
  orderQuantity :: Int, 
  orderCreationTimeUtc :: DateTime,
  orderClientId :: ClientIdentifier
  }
  deriving (Eq)

instance Show Order where
  show (Order d p q ts i) = printf "%s %d@%d (client %d)" (if d == Buy then "B" else "S") q p i 

adjustQuantity :: Int -> Order -> Order
adjustQuantity q (Order d p _ t i) = Order d p q t i

data Trade = Trade {
  tradePrice :: Price,
  tradeQuantity :: Int,
  tradeBuyClient :: ClientIdentifier,
  tradeSellClient :: ClientIdentifier
--  trade_creationTimeStampUtc :: DateTime
}

data TradeConfirmation = TradeConfirmation {
  tradeConfirmationDirection :: Direction,
  tradeConfirmationPrice :: Price,
  tradeConfirmationQuantity :: Int
}
  
tradeToConfirmation :: ClientIdentifier -> Trade -> Maybe TradeConfirmation
tradeToConfirmation clientIdentifier (Trade p q b s)
  | clientIdentifier == b = Just $ TradeConfirmation Buy p s
  | clientIdentifier == s = Just $ TradeConfirmation Sell p s
  | otherwise = Nothing
