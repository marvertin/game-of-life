module GameOfLife (
   Generace,
   Pozice,
   nextg,
   textoveObaleno,
   nekonecny,

) where

import Data.Char
import Control.Arrow
import Data.List
import Control.Monad
import Lib


type Pozice = (Int, Int)
type Generace = [Pozice]

plnDlePozic :: Integral i => i -> i -> a -> a -> [i] -> [a]
plnDlePozic low high neni je sez = pln low (sort sez ++ [high + 1])
  where
    pln i xxs@(x:xs)
      | i > high = []
      | x < i = zbytek xs
      | x == i = je : zbytek xs
      | otherwise = neni: zbytek xxs
      where
        zbytek = pln (i+1)

meze :: Generace -> ((Int, Int), (Int, Int))
meze [] = ((0,0),(0,0))
meze g = let xx = map fst g
             yy = map snd g
         in ((minimum xx, minimum yy), (maximum xx, maximum yy))

textove :: Generace -> [String]
textove g =  [kraj] ++ [ "| " ++ concat (plnDlePozic x1 x2 "  " "* " radek) ++ "|"
              | y <- [y1..y2],
               let radek = map fst $ filter ((==y) . snd)  g]
               ++ [kraj] ++ [""]
    where ((x1, y1), (x2, y2)) = meze g;
          kraj = "+-" ++ take ((x2 - x1 + 1) * 2) (cycle "-") ++ "+"

textoveObaleno :: Generace -> String
textoveObaleno g = unlines $  textove g

sousede1 :: Pozice -> [Pozice]
sousede1 (x, y) = [(x-1, y-1), (x-1, y), (x-1, y+1),
                   (x, y-1), (x, y+1),
                   (x+1, y-1), (x+1, y), (x+1, y+1)]

sousede :: Generace -> [(Int, Pozice)]
sousede g = frequency $ g >>= sousede1

nextg :: Generace -> Generace
nextg g = filter (flip elem $ vyberDlePoctuSousedu 2) g
      ++ vyberDlePoctuSousedu 3
  where sous = sousede g
        vyberDlePoctuSousedu n = map snd $ filter ((==n) . fst) sous


nekonecny :: Generace -> [Generace]
nekonecny = genList nextg



-- main = interact (\x -> unlines $ map textoveObaleno $ map snd $ zip (lines x) (nekonecny zacatek))
