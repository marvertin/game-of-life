module GameOfLife (cykli, zacatek) where
  
import Data.Char
import Control.Arrow
import Data.List
import Control.Monad

zacatek1 = [(7,2), (7,3), (7,4)]
zacatek2 = [(1,1), (1,3), (2,2), (2,3), (3,2)]
zacatek3 = [ (x,y) | x <- [1..5], y <- [1..5]]
zacatek4 = zacatek3 ++ [(1, 6)]
zacatek5 = [ (x,x) | x <- [1..100]]
zacatek6 = [ (x,0) | x <- [1..5]] ++ [ (x,1) | x <- [2..6]]

zacatek :: Generace
zacatek = zacatek6

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (length &&& head) (group (sort list))

genList :: (a -> a) -> a -> [a]
genList f x = x : genList f (f x)


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

nextgn :: Int -> Generace -> Generace
nextgn 0 g = g
nextgn n g = nextgn (n-1) (nextg g)

nekonecny :: Generace -> [Generace]
nekonecny = genList nextg

vysl = unlines $ map textoveObaleno $ take 12 $ nekonecny zacatek

cykli :: Generace -> IO ()
cykli g = do
  xx <- getLine
  putStrLn $ textoveObaleno g
  when (xx /= "x") $ cykli (nextg g)

main = do
  putStrLn "praskni"
  cykli zacatek

-- main = interact (\x -> unlines $ map textoveObaleno $ map snd $ zip (lines x) (nekonecny zacatek))
