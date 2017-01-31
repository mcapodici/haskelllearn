module Nameless.Evaluation where

import Nameless.Data

evaluate :: (NamelessExp -> Maybe NamelessExp) -> NamelessExp -> [NamelessExp]
evaluate step t = case step t of
  Just t' -> t : (evaluate step t')
  _ -> [t]

callByValueStep :: NamelessExp -> Maybe NamelessExp
callByValueStep (NlCall e1 e2) = case callByValueStep e1 of
  Just e1' -> Just $ NlCall e1' e2
  Nothing -> case callByValueStep e2 of
    Just e2' -> Just $ NlCall e1 e2'
    Nothing -> case e1 of
      (NlFunc t) -> Just $ shift (-1) 0 (substitute 0 (shift 1 0 e2) t)
      _ -> Nothing
callByValueStep (NlFunc e) = NlFunc <$> (callByValueStep e)
callByValueStep _ = Nothing

callByNameStep :: NamelessExp -> Maybe NamelessExp
callByNameStep (NlCall e1 e2) = case callByValueStep e1 of
  Just e1' -> Just $ NlCall e1' e2
  Nothing -> case e1 of
      (NlFunc t) -> Just $ shift (-1) 0 (substitute 0 (shift 1 0 e2) t)
      _ -> Nothing
callByNameStep (NlFunc e) = NlFunc <$> (callByValueStep e)
callByNameStep _ = Nothing


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



