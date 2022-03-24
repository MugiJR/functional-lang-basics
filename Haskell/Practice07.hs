module Practice07 where

-- Small test 06

-- Define a function `countToStep :: Int -> Int -> Int -> [Int]`.
--  `countToWithStep from to step` should return the list of all
--  numbers of the form `from+i*step` that are less than or equal 
--  to `to`.
countToWithStep :: Int -> Int -> Int -> [Int]

-- Tests:
--   countToWithStep 1 10 2 == [1,3,5,7,9]
--   countToWithStep 1 10 3 == [1,4,9]
--   countToWithStep 1 10 7 == [1,8]

countToWithStep from to step 
        | from + step >= to + step = []
        | otherwise    = from : countToWithStep (from + step) to step


-- concat' [[1,2],[3],[4,5]] == [1,2,3,4,5]
--          [1,2] ++ [3] ++ [4,5]
concat' :: [[Int]] -> [Int]
concat' (x:xs) = x ++ concat' xs
concat' []     = []


product' :: [Int] -> Int
product' []   =  1
product' (x:xs) = x * product' xs

-- reverse' [1,2,3] == [3,2,1]
reverse' :: [Int] -> [Int]
reverse' []     = []  
reverse' (x:xs) = reverse' xs ++ [x]


-- Redefine the function take. `take n xs` takes the first n elements of the list xs.
-- take' 0 [1, 2, 3] == []
-- take' 2 [1, 2, 3] == [1, 2]
take' :: Int -> [Int] -> [Int]
take' n xs | n < 0 = error "take: n < 0"
take' 0 xs         = []
take' n []         = []
take' n (x:xs)     = x : take' (n-1) xs

-- Redefine the function drop. `drop n xs` drops the first n elements of the list xs.
-- drop' 0 [1, 2, 3] == [1, 2, 3]
-- drop' 2 [1, 2, 3] == [3]
drop' :: Int -> [Int] -> [Int]
drop' n xs | n < 0 = error "drop: n < 0"
drop' 0 xs         = xs
drop' n []         = []
drop' n (x:xs)       = drop' (n-1) xs

-- Redefine the function splitAt.
-- splitAt' 1 [1,2,3] == ([1], [2,3])
-- splitAt' 2 [1,2,3] == ([1,2], [3])
splitAt' :: Int -> [Int] -> ([Int], [Int])

--splitAt' n xs = (take' n xs, drop' n xs)
-- Other Implementation
splitAt' 0 xs = ([], xs)
splitAt' _ [] = ([], [])
splitAt' n (x:xs) = 
        let (p1, p2) = splitAt' (n - 1) xs in
                (x:p1,p2)


-- splitEven [2,4,1,3,4] == ([2,4], [1,3,4])
--  splitEven xs = (largest prefix of xs that only contains even numbers, remaining elements)
splitEven :: [Int] -> ([Int], [Int])

splitEven []       = ([], [])
splitEven (x:xs) 
        | even x   = 
                let (p1, p2) = splitEven xs in
                        (x:p1, p2)
        | otherwise =
                ([],x:xs)