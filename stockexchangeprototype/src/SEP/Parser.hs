module SEP.Parser (parseUnstampedOrder) where

import Text.ParserCombinators.Parsec
import SEP.Data
import Data.DateTime

type UnstampedOrder = DateTime -> ClientIdentifier -> BuyOrSellOrder

unstampedOrderParser :: Parser UnstampedOrder
unstampedOrderParser = 
  do
    char 'B'
    qty <- read <$> many1 digit -- TODO: Limit number of digits of price and qty?
    char '@'
    price <- read <$> many1 digit
    return (\ts i -> (BOrder (BuyOrder price qty ts i)))
  <|>
  do
    char 'S'
    qty <- read <$> many1 digit
    char '@'
    price <- read <$> many1 digit
    return (\ts i -> (SOrder (SellOrder price qty ts i)))

parseUnstampedOrder :: String -> Maybe UnstampedOrder
parseUnstampedOrder = rightToMaybe . (parse unstampedOrderParser "order")

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left a) = Nothing
rightToMaybe (Right b) = Just b

