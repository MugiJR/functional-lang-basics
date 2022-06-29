module Assignment1 where

-- Task 1

wordChain :: [String] -> Bool
wordChain []    = True
wordChain (x:xs) = wordChainHelper xs (last x)

wordChainHelper [] _ = False
wordChainHelper [y] prev 
    | prev == head y = True 
    | otherwise      = False 
wordChainHelper (y:ys) prev 
    | prev == head y  = wordChainHelper ys (last y)
    | otherwise       = False


-- Task 2

countDifferences :: Eq a => [a] -> [a] -> Int
countDifferences l1 l2 = countDifferencesHelper l1 l2 0
countDifferencesHelper [] [] count = count
countDifferencesHelper (x:xs) [] count = countDifferencesHelper xs [] count+1
countDifferencesHelper [] (y:ys) count = countDifferencesHelper [] ys count+1
countDifferencesHelper (x:xs) (y:ys) count 
        | x == y    = countDifferencesHelper xs ys count
        | otherwise = countDifferencesHelper xs ys count+1


-- Tast 3

smallestResult :: (Int -> Int) -> [Int] -> Int
smallestResult f l =  smallestResultHelper f l (maxBound :: Int) (maxBound :: Int)
smallestResultHelper _ [] _ latestindex = latestindex
smallestResultHelper f (x:xs) latestmin latestindex
    | f x < latestmin  = smallestResultHelper f xs (f x) x
    | otherwise        = smallestResultHelper f xs latestmin latestindex


-- Task 4

-- isVowel :: Char -> Bool
-- isVowel c = c `elem` "aeiou"

separateBy :: (a -> Bool) -> [a] -> ([a], [a])
separateBy f l = separateByHelper f l ([], [])

separateByHelper _ [] (l1, l2) = (l1,l2)
separateByHelper f (x:xs) (l1,l2) 
    | f x   = separateByHelper f xs (l1 ++ [x], l2)
    | otherwise = separateByHelper f xs (l1, l2 ++ [x])


