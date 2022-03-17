module Practice06 where

-- Small test 05

data OneTwoThree = One 
                 | Two 
                 | Three
                 deriving(Show)

-- Define a function `toInt :: OneTwoThree -> Int` 
--  that sends each value of type OneTwoThree to 
--  the corresponding integer.

toInt :: OneTwoThree -> Int
toInt One   = 1
toInt Two   = 2
toInt Three = 3

-- Define a function `fromInt :: Int -> OneTwoThree`.
--  It should send every integer to the element of the 
--  type OneTwoThree that corresponds to the nearest integer.
--  (Integers below 1 should be sent to One, 
--   integers greater than 3 should be mapped to Three)

fromInt :: Int -> OneTwoThree
fromInt x
    | x <= 1        = One
    | x == 2        = Two
    | otherwise     = Three


-- Tests: 
--   toInt Two == 2
--   toInt Three == 3
--   fromInt (-10) == One
--   fromInt 1 == One
--   fromInt 2 == Two
--   fromInt 3 == Three
--   fromInt 100 == Three


data Time = Time
            {
              hours :: Int,
              minutes :: Int
            }
            deriving (Show)
-- Field names!

t1 :: Time
t1 = Time 9 45

t2 :: Time
t2 = Time 17 34

-- Define a function that increases the time by one minute!
nextMinute :: Time -> Time
nextMinute t =
    let h = hours t
        m = minutes t
    in Time h (m+1)


nextMinute' :: Time -> Time
nextMinute' (Time h m) = Time h (m+1)

nextMinute1 :: Time -> Time
nextMinute1 (Time 23 59) = Time 0 0
nextMinute1 (Time h 59) = Time (h+1) 0
nextMinute1 (Time h m) = Time h (m+1)


nextMinute1' :: Time -> Time
nextMinute1' (Time h m)
    | h == 23       = Time 0 0
    | (m + 1) == 59 = Time (h + 1) m
    | otherwise     = Time h m


--- Recursive functions

-- Example: the factorial function.
fact :: Integer -> Integer
fact n = if n == 0
         then 1
         else n * fact (n -1)


fact' :: Integer -> Integer
fact' n 
    | n <= 0    = 1
    | otherwise = n * fact' (n-1)

-- Define the fibonacci sequence.
fibo :: Integer -> Integer
fibo n 
    | n <= 1   =  1
    | otherwise =  fibo (n-1) + fibo (n-2)

-- fibo 4 = 3

-- Redefine the power operation (^).
power :: Integer -> Integer -> Integer
power x n | n < 0 = error "undefined"
power x 0         = 1
power x n         = x * x ^ (n-1)

-- Redefine the haskell expression [x .. y] using recursion.
countTo :: Int -> Int -> [Int]
countTo x y
    | x > y     =  []
    | otherwise = x : countTo (x+1) y
    
-- countTo 1 5  == [1,2,3,4,5] 
-- countTo 1 10 == [1..10] 

-- Redefine the length function.
length' :: [Int] -> Int
length' []      =  0
length' (x:xs)  = 1 + length' xs


-- Redefine the following functions using recursion instead of list comprehensions
mapIncr :: [Int] -> [Int]
mapIncr xs = [ x+1 | x <- xs ]

mapIncr' :: [Int] -> [Int]
mapIncr' []     = []
mapIncr' (x:xs) = (x+1) : mapIncr' xs


filterEven :: [Int] -> [Int]
filterEven xs = [ x | x <- xs, even x ]

filterEven' :: [Int] -> [Int]
filterEven' []   =  []
filterEven' (x:xs) 
    | even x    =  x : filterEven' xs
    | otherwise =  filterEven' xs


-- Other recursive functions

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs

concat' :: [[Int]] -> [Int]
concat' []    = []
concat' (x:xs) = x ++ concat' xs
