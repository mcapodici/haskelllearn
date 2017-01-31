module Lib where

import System.Environment
import Control.Monad
import Nameless.Data
import Named.Data
import Named.Parser
import Nameless.Evaluation
import Nameless.Conversion
import Data.List

someFunc :: IO ()
someFunc = do
  args <- getArgs
  let fileName = args !! 0
  let step = if length args > 1 && args !! 1 == "lazy" then callByNameStep else callByValueStep
  text <- readFile fileName
  case parseNamed text of
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
      let evaled = evaluate step nameless
      putStrLn $ join $ intersperse "\n" (map show evaled)
    _ ->
      putStrLn "Error!"
