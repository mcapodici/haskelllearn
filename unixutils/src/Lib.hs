module Lib
    ( main
    ) where

import System.Environment
import qualified System.IO as SIO
import Data.Char
import Data.List.Safe
import Prelude hiding (head, tail)
import qualified Data.ByteString.Lazy as BL
import Control.Exception

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
splitArgsForUtil args = (,) <$> (head args >>= parseUtilName) <*> (tail args)
  
parseUtilName :: String -> Maybe Util
parseUtilName x = p $ map toLower x
  where
    p "cat" = Just Cat
    p _     = Nothing

cat :: [String] -> IO()
cat = sequence_ . (map streamFileToOutput)

streamFileToOutput :: String -> IO()
streamFileToOutput s = catch (BL.readFile s) handler >>= BL.putStr
  where 
    handler e = do 
      let err = show (e :: IOException)
      SIO.hPutStr SIO.stderr ("Aborting concatenation: " ++ err ++ "\n")
      return BL.empty
