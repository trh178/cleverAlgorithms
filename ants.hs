-- ANT COLONY PROJECT IN HASKELL

type City = (Int, Int)
type Distance = Int
type Cities = [City]

euc2D :: City -> City -> Distance
euc2D (x1, x2) (y1, y2) = truncate(sqrt(((d x1 y1) ** 2) + ((d x2 y2) ** 2)))
      where 
      	    d :: Int -> Int -> Float 
      	    d x y = fromIntegral(x - y)

cost :: Permutation -> Cities -> Distance
cost [p:ps] [c:cs] = 

-- TEST CASE
berlin52 = []



