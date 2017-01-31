module Named.Parser where

import Named.Data
import Text.ParserCombinators.Parsec
import Control.Applicative ((<*),(*>))

parseNamed = parse parseLambda "Lambda"

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
  char '\\' <|> char 'λ'
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
