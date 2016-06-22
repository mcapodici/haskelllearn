module SEP.Parser (parseUnstampedOrder) where

import Text.ParserCombinators.Parsec
import SEP.Data
import Data.DateTime

type UnstampedOrder = DateTime -> ClientIdentifier -> BuyOrSellOrder

unstampedOrderParser :: Parser UnstampedOrder
unstampedOrderParser = 
  parserFor 'B' (\p q t i -> BOrder (BuyOrder p q t i)) <|>
  parserFor 'S' (\p q t i -> SOrder (SellOrder p q t i))
  where
    parserFor :: Char -> (Price -> Int -> DateTime -> ClientIdentifier -> BuyOrSellOrder) -> Parser UnstampedOrder
    parserFor orderTypeChar constructor =
          do
            char orderTypeChar
            qty <- read <$> many1 digit -- TODO: Limit number of digits of price and qty?
            char '@'
            price <- read <$> many1 digit
            return (\ts i -> constructor price qty ts i)

parseUnstampedOrder :: String -> Maybe UnstampedOrder
parseUnstampedOrder = rightToMaybe . (parse unstampedOrderParser "order")

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left a) = Nothing
rightToMaybe (Right b) = Just b

