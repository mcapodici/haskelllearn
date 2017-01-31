module Nameless.Evaluation where

import Nameless.Data

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



