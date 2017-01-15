module Lib where

main :: IO ()
main = putStrLn "someFunc"

data Term
  = TTrue
  | TFalse
  | TZero
  | TSucc Term
  | TIsZero Term
  | TPred Term
  | IfThenElse Term Term Term
  deriving Show

data Value
  = VTrue
  | VFalse
  deriving Show

data Terminate = Terminate

add n t = (iterate TSucc t) !! n
zero = TZero
num n = add n zero
minus n t = (iterate TPred t) !! n
asNumber TZero = Just 0
asNumber (TSucc t) = (+1) <$> asNumber t
asNumber _ = Nothing
itt = IfThenElse
aand t1 t2 = itt t1 t2 TFalse

smallStep (IfThenElse TTrue term _) = return term
smallStep (IfThenElse TFalse _ term) = return term
smallStep (IfThenElse term1 term2 term3) = IfThenElse <$> smallStep term1 <*> Right term2 <*> Right term3
smallStep (TIsZero TZero) = return TTrue
smallStep (TIsZero (TSucc _)) = return TFalse
smallStep (TPred TZero) = return TZero
smallStep (TPred (TSucc term)) = return term
smallStep (TSucc term) = TSucc <$> smallStep term
smallStep (TPred term) = TPred <$> smallStep term
smallStep (TIsZero term) = TIsZero <$> smallStep term
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




