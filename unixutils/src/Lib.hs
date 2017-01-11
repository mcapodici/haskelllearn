module Lib
    ( main
    ) where

import System.Environment
import Data.Char
import Data.List.Safe
import Prelude hiding (head, tail)

data Util = Cat

main :: IO ()
main = do
  args <- getArgs
  case splitArgsForUtil args of
    Nothing -> help
    Just (util, utilArgs) -> process util utilArgs

help :: IO ()
help = putStrLn "usage: unixutils TOOL [params..]"

process :: Util -> [String] -> IO()
process Cat = cat 

splitArgsForUtil :: [String] -> Maybe (Util, [String])
splitArgsForUtil args = do
  firstArg <- head args
  utilArgs <- tail args
  util <- parseUtilName firstArg
  return (util, utilArgs)
  
parseUtilName :: String -> Maybe Util
parseUtilName x = p $ map toLower x
  where
    p "cat" = Just Cat
    p _     = Nothing


cat :: [String] -> IO()
cat = const $ putStrLn "cat"
