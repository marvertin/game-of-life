module Main where

import GameOfLife
import Control.Monad

zacatek1 = [(7,2), (7,3), (7,4)]
zacatek2 = [(1,1), (1,3), (2,2), (2,3), (3,2)]
zacatek3 = [ (x,y) | x <- [1..5], y <- [1..5]]
zacatek4 = zacatek3 ++ [(1, 6)]
zacatek5 = [ (x,x) | x <- [1..100]]
zacatek6 = [ (x,0) | x <- [1..5]] ++ [ (x,1) | x <- [2..6]]

zacatek :: Generace
zacatek = zacatek6


cykli :: Generace -> IO ()
cykli g = do
  xx <- getLine
  putStrLn $ textoveObaleno g
  when (xx /= "x") $ cykli (nextg g)


main = do
  putStrLn "praskni do entru"
  cykli zacatek
