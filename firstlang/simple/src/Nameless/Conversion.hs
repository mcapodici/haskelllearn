module Nameless.Conversion where

import Named.Data
import Nameless.Data
import Data.List

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


