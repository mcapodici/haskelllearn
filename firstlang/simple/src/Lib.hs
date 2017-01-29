module Lib where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Applicative ((<*),(*>))
import Control.Monad
import Data.List

someFunc :: IO ()
someFunc = do
  fileName <- getArgs
  text <- readFile (head fileName) -- yes I know!
  case parse parseLambda "lambda" text of
    Right expression -> do
      putStrLn "Your Expression as an object:"
      putStrLn (show expression)
      putStrLn "\nFree variables:"
      let freeVars = getFree expression
      putStrLn $ show $ freeVars
      putStrLn "\nNameless:"
      let nameless = toNameless freeVars expression
      putStrLn $ show $ nameless 
      putStrLn "\nNameless Evalutated:"
      let evaled = evaluate nameless
      putStrLn $ join $ intersperse "\n" (map show evaled)
    _ ->
      putStrLn "Error!"

-- Lambda Calculus Data Structure

type Name = String

data Expression =
    Func  Name       Expression
  | Call  Expression Expression
  | Var   Name
  deriving Show

data NamelessExp =
    NlFunc NamelessExp
  | NlCall NamelessExp NamelessExp
  | NlVar Int

instance Show NamelessExp where
  show (NlFunc e) = "\\. " ++ show e
  show (NlCall e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
  show (NlVar k) = show k


-- Evaluation

evaluate :: NamelessExp -> [NamelessExp]
evaluate t = case evaluateStep t of
  Just t' -> t : (evaluate t')
  _ -> [t]

evaluateStep :: NamelessExp -> Maybe NamelessExp
evaluateStep (NlCall e1 e2) = case evaluateStep e1 of
  Just e1' -> Just $ NlCall e1' e2
  Nothing -> case evaluateStep e2 of
    Just e2' -> Just $ NlCall e1 e2'
    Nothing -> case e1 of
      (NlFunc t) -> Just $ shift (-1) 0 (substitute 0 (shift 1 0 e2) t)
      _ -> Nothing
evaluateStep (NlFunc e) = NlFunc <$> (evaluateStep e)
evaluateStep _ = Nothing

substitute :: Int -> NamelessExp -> NamelessExp -> NamelessExp
substitute j s (NlVar k)
  | k == j = s
  | otherwise = NlVar k
substitute j s (NlFunc t) = NlFunc (substitute (j + 1) (shift 1 0 s) t)
substitute j s (NlCall t1 t2) = NlCall (substitute j s t1) (substitute j s t2)

shift :: Int -> Int -> NamelessExp -> NamelessExp
shift d c (NlVar k) 
  | k < c = NlVar k
  | otherwise = NlVar (k + d)
shift d c (NlFunc t) = NlFunc (shift d (c + 1) t)
shift d c (NlCall t1 t2) = NlCall (shift d c t1) (shift d c t2)


-- Convert expression to nameless expression

type NamingContext = [Name] -- Naming context for DeBrujin, head = 0

getFree :: Expression -> NamingContext
getFree (Func b ex) = filter (b /=) (getFree ex)
getFree (Call e1 e2) = union (getFree e1) (getFree e2)
getFree (Var x) = [x]

toNameless :: NamingContext -> Expression -> NamelessExp
toNameless ctx (Var x) = case elemIndex x ctx of
  Just i -> NlVar i
  Nothing -> NlVar (-1) -- Dodgyyyyy!

toNameless ctx (Call e1 e2) = NlCall (toNameless ctx e1) (toNameless ctx e2)
toNameless ctx (Func n e) = NlFunc (toNameless (n : ctx) e) 

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
