module Lib where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Applicative ((<*),(*>))
import Control.Monad
--import Data.Map

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
func = Func <$> ((char '\\') >> variable) <*> (spaces *> parseExprTotal) 

parseExprExceptCall :: Parser Expression
parseExprExceptCall =
  func <|>
  variableExp <|> 
  (between (char '(') (char ')') parseExprTotal)

parseExprTotal :: Parser Expression
parseExprTotal = 
  call <|> parseExprExceptCall
  
-- Interpreter For Lambda Calculus

--type Context = Map Name Expression

--data ExpressionWithContext = ExpressionWithContext Name Expression Context

--lookup :: Name -> Context -> Maybe Expression


--run :: Context -> Expression -> ExpressionWithContext
--run ctx (Var x) 








