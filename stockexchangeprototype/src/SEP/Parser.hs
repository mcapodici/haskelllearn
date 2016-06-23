module SEP.Parser (parseUnstampedOrder) where

import Text.ParserCombinators.Parsec
import SEP.Data
import Data.DateTime

type UnstampedOrder = DateTime -> ClientIdentifier -> Order

unstampedOrderParser :: Parser UnstampedOrder
unstampedOrderParser = 
  do
    directionChar <- oneOf "BS"
    direction <- case directionChar of 
      'B' -> return Buy
      _ -> return Sell
    qty <- read <$> many1 digit -- TODO: Limit number of digits of price and qty?
    char '@'
    price <- read <$> many1 digit
    return (\ts i -> Order direction price qty ts i)

parseUnstampedOrder :: String -> Maybe UnstampedOrder
parseUnstampedOrder = rightToMaybe . (parse unstampedOrderParser "order")

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left a) = Nothing
rightToMaybe (Right b) = Just b

