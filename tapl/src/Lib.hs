module Lib where

import Text.ParserCombinators.Parsec
import System.IO

main :: IO ()
main = do
  putStr ">"
  hFlush stdout
  text <- getLine
  if text == "quit" then return () else do
    let result = parse pexpression "" text
    case result of 
      Left msg -> putStrLn $ "Error " ++ (show msg)
      Right term -> putStrLn $ show (eval term)
    main

data Term
  = TTrue
  | TFalse
  | TZero
  | TSucc Term
  | TIsZero Term
  | TPred Term
  | IfThenElse Term Term Term
  | Wrong
  deriving Show

data Value
  = VTrue
  | VFalse
  deriving Show

data Terminate = Terminate

-- Parsers --

lexeme :: Parser a -> Parser a
lexeme a = a <* spaces

pexpression :: Parser Term
pexpression = pterm <* eof

-- todo see http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/

pterm :: Parser Term
pterm = choice $ map (try.lexeme) [ptrue, pfalse, p0, psucc, ppred, piszero, pitt, pparens]

pparens = string "(" *> pterm <* string ")"
ptrue = string "true" *> pure TTrue
pfalse =  string "false" *> pure TFalse 
p0 = string "0" *> pure TZero
psucc = lexeme (string "succ") *> (TSucc <$> pterm)
ppred = lexeme (string "pred") *> (TPred <$> pterm)
piszero = lexeme (string "iszero") *> (TIsZero <$> pterm)
pitt = lexeme $ do
  lexeme . string $ "if"
  condition <- pterm
  lexeme . string $ "then"
  trueb <- pterm
  lexeme . string $ "else"
  falseb <- pterm
  return $ IfThenElse condition trueb falseb

--add n t = (iterate TSucc t) !! n
--zero = TZero
--num n = add n zero
--minus n t = (iterate TPred t) !! n
--asNumber TZero = Just 0
--asNumber (TSucc t) = (+1) <$> asNumber t
--asNumber _ = Nothing
--itt = IfThenElse
--aand t1 t2 = itt t1 t2 TFalse
--i
--
--
isNumValue TZero = True
isNumValue (TSucc t) = isNumValue t
isNumValue _ = False

-- Evaluation


smallStep (IfThenElse TTrue term _) = return term
smallStep (IfThenElse TFalse _ term) = return term
smallStep (IfThenElse term1 term2 term3) = case smallStep term1 of
  Left Terminate -> return Wrong -- Case where term1 is a value but not true/false
  next -> IfThenElse <$> next <*> Right term2 <*> Right term3

smallStep (TIsZero TZero) = return TTrue
smallStep (TIsZero (TSucc _)) = return TFalse
smallStep (TIsZero term) = case smallStep term of
  Left Terminate -> return Wrong -- Case where term1 is a value but not a number
  next -> TIsZero <$> next

smallStep (TPred TZero) = return TZero
smallStep (TPred (TSucc term)) = return term
smallStep (TPred term) = case smallStep term of
  Left Terminate -> return Wrong -- Case where term1 is a value but not a number
  next -> TPred <$> next

smallStep (TSucc TTrue) = return Wrong
smallStep (TSucc TFalse) = return Wrong
smallStep (TSucc Wrong) = return Wrong
smallStep (TSucc term) = TSucc <$> smallStep term

smallStep _ = Left Terminate

eval :: Term -> Term
eval term =
  case smallStep term of
    Left Terminate -> term
    Right (term') -> eval term'

--bigStep :: Term -> Maybe Value
--bigStep (IfThenElse term1 term2 term3) = do
--  result <- bigStep term1
--  case result of
--    VTrue -> bigStep term2
--    VFalse -> bigStep term3
--bigStep TTrue = Just VTrue
--bigStep TFalse = Just VFalse
--bigStep _ = Nothing




