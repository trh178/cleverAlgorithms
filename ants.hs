-- ANT COLONY PROJECT IN HASKELL

import System.Random (randomRIO)
import Data.List (delete)

type City = (Int, Int)
type Distance = Int
type Cities = [City]
type Permutation = [Int]

euc2D :: City -> City -> Distance
euc2D (x1, x2) (y1, y2) = truncate(sqrt(((d x1 y1) ** 2) + ((d x2 y2) ** 2)))
      where 
      	    d :: Int -> Int -> Float 
      	    d x y = fromIntegral(x - y)

cost :: Permutation -> Cities -> Distance
cost ps@(p:_) cities = cost' ps
     where
	 cost' (p1:p2:ps) = euc2D (cities !! p1) (cities !! p2) + cost' (p2:ps)
	 cost' (p1:[]) = euc2D (cities !! p1) (cities !! p)

random_permutation :: (Eq a) => [a] -> IO [a]
random_permutation [] = return []
random_permutation list = randomRIO(0, length list - 1) >>=
		   	  (\x -> return (list !! x)) >>=
			  (\y -> random_permutation (delete y list) >>= 
			      (\ys -> return (y:ys)))
			  
-- TEST CASE
berlin52 = [(565,575),(25,185),(345,750),(945,685),(845, 655),
            (880, 660),(25, 230),(525, 1000),(580, 1175),(650, 1130),
            (1605, 620), (1220, 580),(1465, 200),(1530, 5), (845, 680),
            (725, 370),(145, 665), (415, 635),(510, 875), (560, 365),
            (300, 465),(520, 585),(480, 415), (835, 625),(975, 580),
            (1215, 245),(1320, 315),(1250, 400),(660, 180), (410, 250),
            (420, 555),(575, 665),(1150, 1160),(700, 580),(685, 595),
            (685, 610),(770, 610),(795, 645),(720, 635),(760, 650),
            (475, 960), (95, 260),(875, 920),(700, 500),(555, 815),
            (830, 485),(1170, 65), (830, 610),(605, 625),(595, 360),
            (1340, 725),(1740, 245)]


