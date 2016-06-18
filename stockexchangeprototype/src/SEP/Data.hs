module SEP.Data where

import Data.Set
import Data.DateTime
import Data.Ord
import Text.Printf
import Control.Concurrent

-- Describes a trade

type Price = Int -- Price is represented as integer (allowed range [-2^29, 2^29 - 1]) the exchange deals with the 
                 -- smallest unit, e.g. you may want to allow dollars to 2.d.p then then price is in cents.

type ClientIdentifier = Int

data BuyOrder = BuyOrder { 
  buy_order_price :: Price, 
  buy_order_quantity :: Int, 
  buy_order_creationTimeUtc :: DateTime,
  buy_order_clientId :: ClientIdentifier
  }
  deriving (Eq)

instance Show BuyOrder where
  show (BuyOrder p q ts i) = printf "B %d@%d (client %d)" q p i 

instance Ord BuyOrder where
  compare o1 o2 =
    case comparing order_price o1 o2 of
      GT -> GT -- The bigger the buy price the better the offer
      LT -> LT
      EQ -> comparing order_creationTimeUtc o1 o2

data SellOrder = SellOrder { 
  sell_order_price :: Price, 
  sell_order_quantity :: Int, 
  sell_order_creationTimeUtc :: DateTime,
  sell_order_clientId :: ClientIdentifier
  }
  deriving (Eq)

instance Show SellOrder where
  show (SellOrder p q ts i) = printf "S %d@%d (client %d)" q p i 
  
instance Ord SellOrder where
  compare o1 o2 =
    case comparing order_price o1 o2 of
      GT -> LT -- The bigger the sell price the worse the offer 
      LT -> GT
      EQ -> comparing order_creationTimeUtc o1 o2

data OrderCollection = OrderCollection {
  buyQueue :: Set BuyOrder,
  sellQueue :: Set SellOrder
  }
  deriving Show

data Direction = Buy | Sell deriving (Show, Eq, Ord)

class Ord o => Order o where
  order_price :: o -> Price
  order_quantity :: o -> Int
  order_creationTimeUtc :: o -> DateTime
  queueOrder :: o -> OrderCollection -> OrderCollection
  direction :: o -> Direction

instance Order BuyOrder where
  order_price = buy_order_price
  order_quantity = buy_order_quantity
  order_creationTimeUtc = buy_order_creationTimeUtc
  queueOrder order queue = OrderCollection (insert order (buyQueue queue)) (sellQueue queue) 
  direction _ = Buy

instance Order SellOrder where
  order_price = sell_order_price
  order_quantity = sell_order_quantity
  order_creationTimeUtc = sell_order_creationTimeUtc
  queueOrder order queue = OrderCollection (buyQueue queue) (insert order (sellQueue queue)) 
  direction _ = Sell

class QuantityAdjustable a where
  adjustQuantity :: Int -> a -> a

instance QuantityAdjustable BuyOrder where
  adjustQuantity q (BuyOrder p _ t i) = BuyOrder p q t i

instance QuantityAdjustable SellOrder where
  adjustQuantity q (SellOrder p _ t i) = SellOrder p q t i

topBuyOrder :: OrderCollection -> Maybe BuyOrder
topBuyOrder queue = fst <$> maxView (buyQueue queue)

topSellOrder :: OrderCollection -> Maybe SellOrder
topSellOrder queue = fst <$> maxView (sellQueue queue)

data BuyOrSellOrderWithConfirmationChannel = BuyOrSellOrderWithConfirmationChannel {
  bosowcc_order :: BuyOrSellOrder,
  bosowcc_channel :: Chan Trade
}

data BuyOrSellOrder =  
    BOrder BuyOrder
  | SOrder SellOrder

{--
instance Order BuyOrSellOrder where
  order_price (BOrder o) = order_price o
  order_price (SOrder o) = order_price o
  order_quantity (BOrder o) = order_quantity o
  order_quantity (SOrder o) = order_quantity o
  order_creationTimeUtc (BOrder o) = order_creationTimeUtc o
  order_creationTimeUtc (SOrder o) = order_creationTimeUtc o
  queueOrder (BOrder o) = queueOrder o
  queueOrder (SOrder o) = queueOrder o
--}
instance Show BuyOrSellOrder where
  show (BOrder o) = show o
  show (SOrder o) = show o
--
data Trade = Trade {
  trade_price :: Price,
  trade_quantity :: Int
--  trade_creationTimeStampUtc :: DateTime
}

instance Show Trade where
  show t = "T " ++ show (trade_quantity t) ++ "@" ++ show (trade_price t)
