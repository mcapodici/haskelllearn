module Main where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import Control.Monad
import Data.Ix

buildGrid = do 
 arr <- newArray ((1,1),(10,11)) ' ' :: ST s (STArray s (Int, Int) Char)
 forM_ [1..10] (\row -> writeArray arr (row, 11) '\n')
 forM_ [1..10] (\diag -> writeArray arr (diag, diag) '!')
 chars <- forM (range ((1,1),(10,11))) (readArray arr)
 return chars
 
main = putStr $ runST buildGrid
