module Lib where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Applicative ((<*),(*>))
import Control.Monad

someFunc :: IO ()
someFunc = do
  fileName <- getArgs
  text <- readFile (head fileName) -- yes I know!
  let expression = parse parseLambda "lambda" text
  putStrLn (show expression)

-- Lambda Calculus Data Structure

type Name = String

data Expression =
    Func  Name       Expression
  | Call  Expression Expression
  | Var   Name
  deriving Show

-- Parser For Lambda Calculus

lexeme :: Parser a -> Parser a
lexeme a = a <* spaces

variable :: Parser String
variable = do 
  first <- letter 
  rest <- many (letter <|> digit)
  return (first:rest)

variableExp :: Parser Expression
variableExp = Var <$> variable 

call :: Parser Expression
call = chainl1 (lexeme parseExprExceptCall) (return Call) 

func :: Parser Expression
func = do
  char '\\'
  v <- lexeme variable
  char '.'
  spaces
  e <- parseExprTotal
  return $ Func v e

parseExprExceptCall :: Parser Expression
parseExprExceptCall =
  func <|>
  variableExp <|> 
  (between (char '(') (char ')') parseExprTotal)

parseExprTotal :: Parser Expression
parseExprTotal = 
  lexeme call <|> lexeme parseExprExceptCall
 
parseLambda :: Parser Expression
parseLambda = do
  spaces
  result <- parseExprTotal
  return result
