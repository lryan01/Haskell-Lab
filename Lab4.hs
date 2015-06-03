{-        Lab 4 Lauren Ryan         -}
import Data.List

-- Problem #1
gallons :: Float -> Float
gallons l = l*0.26417

usd :: Float -> Float
usd c = c*0.79

price :: Float -> Float -> Float
price l c =  usd c / gallons l 

{- Answer:  
62.3 liters = 16.45779 gallons
78.4 CAD = 61.94 USD-
USD/Gallon = $3.763-}

-- Problem #2
radians :: Fractional a => a -> a
radians x = x*0.0174532925

flightDistance :: (Float, Float) -> (Float, Float) -> Float
flightDistance (lat1, long1) (lat2, long2) = 3443.753*acos(cos(x1)*cos(x2)*cos(y1-y2)+sin(x1)*sin(x2))
    where x1 = radians lat1 
          y1 = radians long1
          x2 = radians lat2
          y2 = radians long2
{- Answer: 
2287.008
  -}

-- Problem #3
factorial :: Integer -> Integer
factorial x 
    | x < 0 = error "negative input"
    | otherwise = foldl (*) 1 [1..x]
    
{- Answer:  
 93326215443944152681699238856266700490715968264381621468592963895217599993229915
6089414639761565182862536979208272237582511852109168640000000000000000000000 -}

-- Problem #4
isEven :: Int -> Bool
isEven x = 
    if mod x 2 == 0 
        then True
        else False
{- Answer:  -}

-- Problem #5
cubeSum :: Integer
cubeSum = sum[x*x*x | x <- [1000..2000], odd x]
{- Answer:  
1874999250000-}

-- Problem #6

summation :: Integer -> Integer
summation n = sum[x*x | x <- [0..n]]

closed :: Integer -> Integer
closed n = (n*(n+1)*(2*n+1))`div`6

scCompare :: Integer -> Bool
scCompare n = summation n == closed n

{- Answer: 
True -}


-- Problem #7
count :: Eq a => [a] -> a -> Int
count xs y = length [ v| v <- xs, v == y ]

count' :: (Eq a, Num b) => [a] -> a -> b
count' [] y = 0
count' (x:xs) y 
        |x == y = 1 + count' xs y
        |otherwise = 0 + count' xs y
        
{- Answer:  
7 -}

-- Problem #8
maxList :: (Ord a) => [a] -> a  
maxList [] = error "empty list"  
maxList [x] = x  
maxList (x:xs)   
    | x > maxList xs = x  
    | otherwise =  maxList xs
 
{- Answer:  -}

-- Problem #9
removeSpace :: [Char] -> [Char]
removeSpace xs = filter (/= ' ') xs  
--removeEven xs = filter (not even x) xs
doubleAll :: Num b => [b] -> [b]
doubleAll xs = map (*2) xs
any55 :: [Int] -> Bool
any55 xs = any (== 55) xs
allOdd :: [Int] -> Bool
allOdd xs = all (odd) xs
{- Answer:  -}

-- Problem #10
isPrime :: Int -> Bool
isPrime x 
    |x == 2 = True
    |mod x 2 == 0 = False
    |any (== 0)[mod x n | n <- take (x-4) [3,4..]] = False
    |otherwise = True

{- Answer: 
Prime numbers between 1000 and 1020 :  1009, 1013, 1019 -}


