module Practice04 where
import Practice03 (squares)

-- Small test 03:

-- 1. Define a function f3 :: [Int] -> [Int]
-- `f3 xs` should be a list that contains the elements of the list `xs` that are divisible by 3.


f3 :: [Int] -> [Int]
f3 l = [x | x <- l, x `mod` 3 == 0]
-- 2. Define a function hasLength2 :: [Int] -> Bool
--    `hasLength2 xs` should return True when the list `xs` has exactly two elements.
 
hasLength2 :: [Int] -> Bool
hasLength2 l = length l == 2


-- Conversions
--------------

-- We have several number types:
--   Int, Integer, Float, Double, ...
--
--                       ┌──────────────────────────────────────┐
--                       │                Num                   │
--                       │======================================│
--                       │ Int, Integer, Float, Double, Complex │
--                       └─────────────────┬────────────────────┘
--                                         │
--        ┌─────────────────────────────┐  │  ┌────────────────────────┐
--        │            Real             │  │  │       Fractional       │
--        │=============================│<─┴─>│========================│
--        │ Int, Integer, Float, Double │     │ Float, Double, Complex │
--        └────────────┬────────────────┘     └──┬─────────────────────┘
--                     │                         │
-- ┌──────────────┐    │    ┌───────────────┐    │    ┌────────────────────────┐
-- │   Integral   │    │    │   RealFrac    │    │    │        Floating        │
-- │==============│<───┴───>│===============│<───┴───>│========================│
-- │ Int, Integer │         │ Float, Double │         │ Float, Double, Complex │
-- └──────────────┘         └───────┬───────┘         └────────┬───────────────┘
--                                  │                          │
--                                  │    ┌───────────────┐     │
--                                  │    │   RealFloat   │     │
--                                  └───>│===============│<────┘
--                                       │ Float, Double │
--                                       └───────────────┘
--
-- ceiling, floor, round :: (RealFrac a, Integral b) => a -> b
-- ceiling, floor, round :: Double -> Int
-- ceiling, floor, round :: Float  -> Integer

-- fromIntegral :: (Integral a, Num b) => a -> b

-- fromIntegral :: Int     -> Float
--fromIntegral :: Integer -> Double
-- fromIntegral :: Int     -> Integer
-- fromIntegral :: Integer -> Int

-- Compute the length of a vector with int coordinates.
vecLen :: (Int, Int) -> Double
vecLen (x, y) = sqrt (fromIntegral (x^2 + y^2))
-- vecLen (x, y) == square root of x²+y²
-- sqrt takes floating object as its input. So we use fromIntegral.

-- ^   ∙(x, y)
-- |  /
-- | /
-- |/
-- |------->

--- Compute the average of a list of integers.
average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral(length xs)
-- average [1, 2, 3, 4] == 2.5

-- More lists operations
------------------------

-- zip :: [a] -> [b] -> [(a, b)]
-- (e.g. zip :: [Int] -> [Char] -> [(Int, Char)])
-- zip [1, 2, 3] ['a', 'b', 'c'] == [(1, 'a'), (2, 'b'), (3, 'c')]

-- zip   [1,        2,        3]
--       ['a',      'b',      'c']
-- ==    [(1, 'a'), (2, 'b'), (3, 'c')]

-- Example:
filterEvenPos :: [Char] -> [Char]
filterEvenPos cs = [c | (i,c) <- zip [0..] cs , even i]
-- filterEvenPos ['a', 'b', 'c', 'd', 'e'] == ['a', 'c', 'e']

-- Note: Infinite lists!

getNth :: Int -> [Char] -> Char
getNth n xs = head (drop n xs)
-- xs !! max 0 n

-- Examples:
--   getNth 0 ['a', 'b', 'c'] == 'a'
--   getNth 1 ['a', 'b', 'c'] == 'b'
--   getNth 2 ['a', 'b', 'c'] == 'c'
--   getNth 3 ['a', 'b', 'c'] is undefined

-- splitting lists:
--  take :: Int -> [a] -> [a]
--  drop :: Int -> [a] -> [a]
-- Example:
--   take 5 [1..10] = [1,2,3,4,5]
--   drop 5 [1..10] = [6,7,8,9,10]

getNth' :: Int -> [Int] -> Int
getNth' n xs  
    | n < 0 || n > length xs = error "getNth: out of bound"
    | otherwise              = head [x | (i,x) <- zip [0..] xs, i == n]


-- Note: Canvas extra solution with `cycle`!

-- Appending lists:
--  (++)   :: [a] -> [a] -> [a]
--  concat :: [[a]] -> [a]
-- Examples:
--   [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]
--   concat [ [1, 2], [3], [4, 5, 6] ] == [1,2,3,4,5,6]

-- Define a function rotate1 that rotates a list 1 step to the left.
-- Examples:
--  rotate1 []           == []
--  rotate1 [1, 2, 3, 4] == [2, 3, 4, 1]
--  rotate1 [4, 2, 3, 1] == [2, 3, 1, 4]
rotate1 :: [Int] -> [Int]
rotate1 xs = drop 1 xs ++ take 1 xs

-- rotate1 [2, 3, 4, 1]
--   == tail [2, 3, 4, 1] ++ [head [2, 3, 4, 1]]
--   == [3, 4, 1] ++ [2]
--   == [3, 4, 1, 2]

-- tail xs   = drop 1 xs
-- [head xs] = take 1 xs

-- Define a function rotateN that rotates a list n steps to the left.
-- Examples:
--   rotateN 2 [1, 2, 3, 4] == [3, 4, 1, 2]
--   rotateN 6 [1, 2, 3, 4] == [3, 4, 1, 2]
rotateN :: Int -> [Int] -> [Int]
rotateN n [] = []
rotateN n xs = 
    let n' = n `mod` length xs in
        drop n' xs ++ take n' xs

--
allSquares :: [Int]
allSquares = squares [0..]

firstNSquares :: Int -> [Int]
firstNSquares n = take n allSquares

-- `firstNPrimes n` should compute the first n prime numbers.
isPrime :: Int -> Bool
isPrime p = length [d | d <- [1..p], p `mod` d == 0] == 2

-- Note: Variations!

allPrimes :: [Int]
allPrimes = [x | x <- [2..], isPrime x]

firstNPrimes :: Int -> [Int]
firstNPrimes n = take n allPrimes

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at
-- even and odd positions:
-- (You can assume that the length of the input list is even.)
-- Example:
-- swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
swapEvenOddPos :: [Int] -> [Int]
swapEvenOddPos xs = 
  let xs_evens = [ x | (i,x) <- zip [0..] xs, even i]
      xs_odds  = [ x | (i,x) <- zip [0..] xs, odd i]
  in concat [ [b,a] | (a,b) <- zip xs_evens xs_odds ]


{- Extra tasks -}

--                Widht Height
type Rectangle = (Int,  Int)

-- Calculate the areas of the rectangles in the list!
areas :: [Rectangle] -> [Int]
areas xs = [ l*b | (l, b) <- xs ]
-- Example: areas [(2,3), (4,5), (1,6)] == [6, 20, 6]

areas' rs = [ area r | r <- rs ]
    where area(w,h) = w * h


-- Check if all elements in a list are equal to each other!
allEqual :: [Int] -> Bool
allEqual [] = True
allEqual (x:xs) = null [y | y <- xs, y /= x]
-- Examples:
-- ∙ allEqual []      == True
-- ∙ allEqual [1,2]   == False
-- ∙ allEqual [3,3,3] == TrueS


-- Let's say that a doctor is available from Monday to Friday, 8-17 every day.
days :: [String]
days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]

hours :: [Int]
hours = [8..16]

-- Combine these values for all the possible (one hour long) appointments!
appointments :: [(String, Int)]
appointments = [ (day,hour) | day <- days, hour <- hours ]
