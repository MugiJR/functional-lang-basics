{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module E1 where



-- ################ Word duplication ####################

-- Define the function duplicateWords, which - given an input string - should produce an output, 
-- that has every word of the original sentence repeated twice!
-- Words are defined by continuous character sequences separated by spaces. 
-- You can assume that the input is finite and there is no space at the beginning or at the end of the input.

-- #######################################################

-- duplicateWords "" == ""
-- duplicateWords "cheese" == "cheese cheese"
-- duplicateWords "This is an example" == "This This is is an an example example"
-- duplicateWords "Lorem ipsum dolor sit amet" == "Lorem Lorem ipsum ipsum dolor dolor sit sit amet amet"

duplicateWords :: String -> String
duplicateWords [] = []
duplicateWords ls = unwords [ x ++ " " ++ x | x <- words ls]






-- ################ Apply a function multiple times ####################

-- Implement a function that applies a function a given amount of times on an input and returns the result!
-- In case the requested number of applications is negative, return Nothing, otherwise wrap the value in a Just constructor!

-- #####################################################################

-- applyTimes (-3) (+1) 5        == Nothing
-- applyTimes 0    (+1) 5        == Just 5
-- applyTimes 3    (+1) 5        == Just 8
-- applyTimes 5    id   0        == Just 0
-- applyTimes 3    (*2) 1        == Just 8
-- applyTimes 4    tail "qwerty" == Just "ty"

applyTimes :: Integral i => i -> (a -> a) -> a -> Maybe a

applyTimes c _ _ | c < 0 = Nothing
applyTimes c f n 
    | c == 0    = Just n
    | otherwise = applyTimes (c-1) f (f n)




-- ################ Function separation ####################

-- Define a function, which converts a function that has a pair as its output type into a pair of functions, 
-- each with one of the outputs.

-- Hint: Use composition, fst and snd, and potentially lambda functions as well.

-- #####################################################################

-- fst (divFun (`divMod` 5)) 23 == 4
-- snd (divFun (`divMod` 5)) 23 == 3
-- fst (divFun (\x -> (x*2,x+10))) 90 == 180
-- snd (divFun (\x -> (x*2,x+10))) 90 == 100
-- fst (divFun (\x -> (x,length x))) "abc" == "abc"
-- snd (divFun (\x -> (x,length x))) "abc" == 3

divFun :: (a -> (b,c)) -> (a -> b, a -> c)

divFun f = (fst . f, snd . f)




-- ################ Conditional joining ####################

-- Implement the applyWhen function, which has the following four parameters:

-- A joining function (a -> b -> c)
-- A binary predicate (a -> b -> Bool)
-- Two input lists ([a] and [b])
-- It traverses the two lists in parallel, and at each pair of elements creates 
-- an output result using the joining function if the predicate holds for the two values or skips them otherwise.

-- The output list is at most as long, as the shorter list among the inputs.

-- #####################################################################


-- applyWhen (,) (>) [1..10] [10,9..1] == [(6,5),(7,4),(8,3),(9,2),(10,1)]
-- applyWhen (,) (>) [] [1..] == []
-- applyWhen (,) (>) [1..] [] == []
-- applyWhen (+) (\a b -> a + b == 5) [0,1,2,3,4,5] [5,4,3,2,1,0] == replicate 6 5
-- applyWhen (++) (\a b -> null a || null b) [[], "apple", "banana", []] ["lemon", [], "peach", []] == ["lemon", "apple", ""]
-- applyWhen (\_ b -> (b, not b)) (flip const) (repeat undefined) (take 100 $ iterate not True) == replicate 50 (True, False)

applyWhen :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]

applyWhen f pred l1 l2 = [f x y |(x,y) <- zip l1 l2, pred x y]





-- ################ Negativity based grouping ####################

-- Group numbers from a list into sublists of negative and non-negative values while keeping their original order.

-- #####################################################################

-- groupNegativesAndNonnegatives [1,2,3,4,0,-1,0,-2,3] == [[1,2,3,4,0],[-1],[0],[-2],[3]]
-- groupNegativesAndNonnegatives [0,1,2,3,4,0,-1,0,-2,3,0] == [[0,1,2,3,4,0],[-1],[0],[-2],[3,0]]
-- groupNegativesAndNonnegatives [] == []
-- groupNegativesAndNonnegatives [1] == [[1]]
-- groupNegativesAndNonnegatives [-1] == [[-1]]
-- groupNegativesAndNonnegatives [1,-1] == [[1],[-1]]
-- groupNegativesAndNonnegatives [-1,1,-1] == [[-1],[1],[-1]]
-- groupNegativesAndNonnegatives [-1,1,-1,1] == [[-1],[1],[-1],[1]]
-- groupNegativesAndNonnegatives [1,-1,1,-1,1] == [[1],[-1],[1],[-1],[1]]
-- take 10 (groupNegativesAndNonnegatives [-5..] !! 1) == [0..9]
-- take 5 (groupNegativesAndNonnegatives (cycle [-2..2])) == [[-2,-1],[0,1,2],[-2,-1],[0,1,2],[-2,-1]]

groupNegativesAndNonnegatives :: (Ord a, Num a) => [a] -> [[a]]

groupNegativesAndNonnegatives [] = []
groupNegativesAndNonnegatives [x] = [[x]]
groupNegativesAndNonnegatives (x:xs) 
    | x >= 0    = 
        let n = getNextNegativeIndex (x:xs) in take (n-1) (x:xs) : groupNegativesAndNonnegatives (drop (n-1) (x:xs))
    | otherwise  = 
        let n = getNextPositiveIndex (x:xs) in take (n-1) (x:xs) : groupNegativesAndNonnegatives (drop (n-1) (x:xs))

getNextNegativeIndex ls 
    | not (any (<0) ls) = length ls + 1
    | otherwise         = head [i | (i,x) <-zip [1..] ls, x < 0] 

getNextPositiveIndex ls 
    | not (any (>=0) ls) = length ls + 1
    | otherwise         = head [i | (i,x) <-zip [1..] ls, x>=0] 

-- NOTE :  Last two test cases have some issue




-- ################ Quantifiers and fulfillment checking ####################

-- Define a datatype named Quantifier to represent quantification over lists for a predicate. 
-- It should have two constructors, Forall (∀) and Exists (∃).

-- The latter (Exists) should also have a Bool valued parameter, which determines whether the existence is unique. (See the next task for explanation!)


-- Derive the Show and Eq type class instances automatically!

-- ============================
-- Is the condition satisfied?
-- ============================

-- Define the function satisfies, which checks if a given quantification holds for a given predicate over a given list.

-- Hint: In case of Exists False, you need to check if there is at least one element satisfying the predicate, 
-- in case of Exists True you need to check whether there is exactly one element doing so, 
-- and finally in case of Forall you need to check if all the elements comply with the requirement.

-- #####################################################################

-- satisfies Forall (\x -> length x <= 7) ["lorem", "ipsum", "dolor", "sit", "amet"] == True
-- satisfies (Exists False) (>10) [1..] == True
-- satisfies (Exists True) (== 4) [1..10] == True
-- satisfies Forall (>5) [1..10] == False
-- satisfies (Exists False) (== 10) [] == False
-- satisfies (Exists True) (== 4) [x `mod` 1000 | x <- [1..]] == False

data Quantifier = Forall | Exists Bool deriving (Eq, Show)

satisfies :: Quantifier -> (a -> Bool) -> [a] -> Bool


satisfies Forall _ []     = True 
satisfies Forall f (x:xs) =  f x && satisfies Forall f xs
satisfies (Exists True) _ []  = False
satisfies (Exists False) _ []  = False
satisfies (Exists True) f ls = satisfiesHelper f ls False False 
satisfies (Exists False) f (x:xs) 
          | f x  = satisfies (Exists False) f xs 
          | otherwise = True

satisfiesHelper:: (a->Bool) -> [a] -> Bool -> Bool-> Bool
satisfiesHelper  _ [] a1 a2 = a1 || a2
satisfiesHelper f (x:xs) True False
    | f x = False
    | otherwise = satisfiesHelper f xs True False
satisfiesHelper f (x:xs) False False 
    | f x = satisfiesHelper f xs True False
    | otherwise = satisfiesHelper f xs False False



-- ################ Differences of strings ####################

-- Implement a function that compares two strings by checking their corresponding characters and returning the differences found. 
-- If the two strings are the same, Nothing should be returned, 
-- otherwise the result should be the list of the mismatched characters from the first string wrapped in a Just constructor.

-- Only iterate through the strings until the end of the first one!

-- #####################################################################

-- difference "peach" "peach" == Nothing
-- difference "pear" "peach" == Just "r"
-- difference "peach" "pear" == Just "ch"
-- difference "" "" == Nothing
-- difference "apple" "" == Just "apple"
-- difference "" "apple" == Nothing
-- difference "cheese" "cheesecake" == Nothing
-- difference "shield" "spread" == Just "hil"
-- difference "abrakadabra" "abbrakadabra" == Just "rakadabra"
-- difference (reverse ('b':(replicate 20 'a'))) (replicate 21 'a') == Just "b"
-- difference ((replicate 10 'a') ++ ('b':(replicate 10 'a'))) (replicate 21 'a')  == Just "b"
-- difference (replicate 21 'a') (reverse ('b':(replicate 20 'a'))) == Just "a"
-- difference (replicate 20 'a') (reverse ('b':(replicate 20 'a'))) == Nothing
-- difference (replicate 21 'a') (repeat 'a') == Nothing
-- take 10 (fromJust (difference ['a'..] "apple")) == "bcdfghijkl"
-- fromJust (difference "apple" ['a'..]) == "ppl"
-- difference "apple" (cycle "apple") == Nothing
-- take 8 (fromJust (difference (cycle "apple") "apple")) == "appleapp"

difference :: String -> String -> Maybe String

difference [] []    = Nothing 
difference []  _    = Nothing 
difference l1 []    = Just l1
difference l1 l2 
        | l1 == l2  = Nothing 
        | otherwise = 
            let r = differenceHelper l1 l2 in 
                if null r then Nothing 
                else Just r

differenceHelper [] [] = []
differenceHelper [] _  = []
differenceHelper ls [] = ls
differenceHelper (x:xs) (y:ys) 
    | x /= y           = x : differenceHelper xs ys
    | otherwise        =     differenceHelper xs ys 
