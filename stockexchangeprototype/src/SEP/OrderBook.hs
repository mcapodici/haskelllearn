module SEP.OrderBook (OrderBook, emptyBook, queueOrder, bestBuyOrder, bestSellOrder, removeOrder) where

import SEP.Data
import Data.Set as S
import Data.Ord

data OrderBook = OrderBook {
  buyQueue :: S.Set Order,
  sellQueue :: S.Set Order
  }
  deriving Show

emptyBook :: OrderBook
emptyBook = OrderBook empty empty

queueOrder :: Order -> OrderBook -> OrderBook
queueOrder o@(Order Buy _ _ _ _) (OrderBook buys sells) = (OrderBook (S.insert o buys) sells)
queueOrder o@(Order Sell _ _ _ _) (OrderBook buys sells) = (OrderBook buys (S.insert o sells))

bestBuyOrder :: OrderBook -> Maybe Order
bestBuyOrder book = fst <$> maxView (buyQueue book)

bestSellOrder :: OrderBook -> Maybe Order
bestSellOrder book = fst <$> maxView (sellQueue book)

removeOrder :: Order -> OrderBook -> OrderBook
removeOrder order (OrderBook buys sells) = OrderBook (delete order buys) (delete order sells)

-- Set up an ordering instance from worst to best price with earlier timestamp winning any
-- tiebreakers. This allows these to be ordered in a tree/set etc. for the purpose of deciding
-- which order to execute next. Ideally this woule be a seperate ordering rather than THE Ord
-- instance but this is convenient to use with Set type that works with the instance.
instance Ord Order where 
  compare (Order Buy _ _ _ _) (Order Sell _ _ _ _) = GT -- Arbitrary ordering for buy vs. sell. We'll keep them seperate anyway
  compare (Order Sell _ _ _ _) (Order Buy _ _ _ _) = LT

  compare (Order Buy p1 _ t1 _ ) (Order Buy p2 _ t2 _ ) =
    case compare p1 p2 of
      GT -> GT 
      LT -> LT
      EQ -> compare t1 t2

  compare (Order Sell p1 _ t1 _ ) (Order Sell p2 _ t2 _ ) =
    case compare p1 p2 of
      GT -> LT 
      LT -> GT
      EQ -> compare t1 t2


