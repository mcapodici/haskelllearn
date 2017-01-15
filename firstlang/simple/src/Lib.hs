module Lib where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Applicative ((<*),(*>))
import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Lambda Calculus Data Structure

type Name = String

data Expression =
    Func  Name       Expression
  | Call  Expression Expression
  | Var   Name
  deriving Show

-- Parser For Lambda Calculus

spaces :: Parser ()
spaces = skipMany1 space

variable :: Parser String
variable = do 
  first <- letter 
  rest <- many (letter <|> digit)
  return (first:rest)

variableExp :: Parser Expression
variableExp = Var <$> variable

call :: Parser Expression
call = chainl1 parseExprExceptCall (spaces >> return Call) 

func :: Parser Expression
func = do
  char '\\'
  v <- variable
  char '.'
  skipMany spaces
  e <- parseExprTotal
  return $ Func v e

parseExprExceptCall :: Parser Expression
parseExprExceptCall =
  func <|>
  variableExp <|> 
  (between (char '(') (char ')') parseExprTotal)

parseExprTotal :: Parser Expression
parseExprTotal = 
  call <|> parseExprExceptCall
  
