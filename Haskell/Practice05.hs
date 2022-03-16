module Practice05 where
-- Small test 

-- 1. Define a function `squaresBelow`.
--    `squaresBelow n` should return the list of all non-negatice square numbers that are less than or equal to n.

squaresBelow :: Int -> [Int]
squaresBelow n = [x^2 | x<-[1..n] , x^2 <= n]

filterEvenPos :: [Int] -> Int
filterEvenPos xs = head [i | (i,c)<-zip [0..] xs,  even c]


{- Pattern matching -}

-- General schema of a function definition in haskell
--   {function name} :: {function type}
--   {function name} {list of patterns} = {function body}
--   {function name} {list of patterns} = {function body}
--   ...

-- Patterns:
--   Variable pattern x         : matches anything

f1 x = x

--   Wildcard pattern _         : matches anything

f2 _ = 10

--   Constant integer pattern 42 : matches an integer with that value

f3 10 = 20
f3 20 = 30
f3 n  = n

f3' x = if x == 10
          then 20
        else if x == 20
          then 30
        else x

-- f3 10 == ?

-- Order of patterns!

{--

f4 n  = n
f4 10 = 20
f4 20 = 30

-- commented this due to warning --}

-- f4 10 == ? [-- WARNING : Pattern Match redundant]

--   Tuple pattern (p1, p2, p2) : matches any 3-tuple

-- Polymorphism : when a single function definition can be used with different types.

f5 :: ((x, y), (z, w)) -> (x, y, z, w)
-- f5 :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int)
f5 ((a, b), (c, d)) = (a, b, c, d)

f5Int :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int)
f5Int = f5

--   List pattern [p1, p2, ...] : matches a list with the correct length
--     Special case: empty list pattern []

f6 :: [(Char, Bool)] -> Int
f6 [(a, b), (c, d)] = 0
f6 _ = 1

f7 :: String -> String
f7 "a" = "A"
f7 xs  = xs

--   "Cons" pattern (x:xs)      : matches a non-empty list
--   Constructor patterns False, True, ... : matches a constructor of a data type.

not' :: Bool -> Bool
not' True = False 
not' False = True

-- Exercises:
-- Define `and`, `or` and `xor` :: Bool -> Bool -> Bool using pattern matching.
and' :: Bool -> Bool -> Bool
and' True True = True 
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

xor' :: Bool -> Bool -> Bool
xor' False True = True
xor' True False = True 
xor' _ _ = False


{- Guard clauses -}

-- Instead of:
g1 :: Bool -> Int
g1 b = if b then 42 else 10

-- We can write:
g2 :: Bool -> Int
g2 b 
  | b           = 42
  | otherwise   = 10

-- Redefine the function min (that computes the
--  minimum of two integers) using pattern guards.
min' :: Int -> Int -> Int
-- min' a b = if a < b then a else b
min' a b 
    | a < b       = a
    | otherwise   = b

-- Define a function that sorts a pair of integers.
sort2 :: (Int, Int) -> (Int, Int)
-- sort2 (1, 2) == (1, 2)
-- sort2 (2, 1) == (1, 2)

sort2 (x, y) 
  | x < y       = (x, y)
  | otherwise   = (y, x)


-- Data declarations

-- Data type : type defined as an algebraic data type
--             defined by a list of data constructors.

data Bool' = False'
           | True'
           deriving (Show, Eq)

-- deriving : automatic definition of functions.
--   deriving (Show) :  show :: Bool' -> String
--   deriving (Eq) :    (==) :: Bool' -> Bool' -> String

data List a = Empty              -- []
            | Cons a (List a)    -- (:)
            deriving (Show)

headList :: List a -> a
headList (Cons x _) = x
headList Empty     = error "empty list"


data Color = Red | Green | Blue
           deriving (Show)

-- Define eqColor without using deriving
eqColor :: Color -> Color -> Bool
eqColor Red   Red     = True 
eqColor Blue  Blue    = True 
eqColor Green Green   = True 
eqColor _     _       = False
