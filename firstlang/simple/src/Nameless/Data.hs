module Nameless.Data where

data NamelessExp =
    NlFunc NamelessExp
  | NlCall NamelessExp NamelessExp
  | NlVar Int

instance Show NamelessExp where
  show (NlFunc e) = "\\. " ++ show e
  show (NlCall e1 e2) = "(" ++ show e1 ++ ")(" ++ show e2 ++ ")"
  show (NlVar k) = show k


