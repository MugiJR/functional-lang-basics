module Practice02 where

isOperator :: Char -> Bool

isOperator '+' = True 
isOperator '-' = True 
isOperator '*' = True 
isOperator '/' = True 
isOperator _ = False 


{- Tuples -}

-- Empty Tuple
emptyTuple :: ()
emptyTuple = ()

-- Single Tuple
oneTuple :: (Int)
oneTuple = (5)

-- Double Tuple
twoTuple :: (Int, String)
twoTuple = (4,"Second Arg")

first :: Int
first = fst twoTuple   -- get first argument from two tuples

second :: String
second = snd twoTuple  -- get second argument from two tuples

swap1 :: (Int, String) -> (String, Int)
swap1 t = (snd twoTuple,fst twoTuple)   -- Swap tuples (type 1)
swap2 (x,y) = (y,x) -- Swap tuples (type 2)

-- Triple Tuple
threeTuple :: (Int, String, Double)
threeTuple = (14,"MR",88.5)

swap3 :: (Int, String, Double) -> (Double, Int, String)
swap3 (x,y,z) = (z,x,y)


{- Person -}

--             Name    Age  Height Weight
type Person = (String, Int, Float, Float)

me :: Person
me = ("Mr. MR", 24,176.5,88)

name :: Person -> String 
name (x,_,_,_) = x

age :: Person -> Int 
age (_,x,_,_) = x

height :: Person -> Float 
height (_,_,x,_) = x

weight :: Person -> Float 
weight (_,_,_,x) = x

-- Body Mass Index = weight / (height ^ 2)

bmi :: Person -> Float
bmi p = weight p / (height p ^ 2)

bmi' :: Person -> String
bmi' p = show(bmi p)   -- { show() ~ converts any given input to String}

describe :: Person -> String
describe p = "The BMI of "++ name p ++ " is " ++ bmi' p

describe' p = 
    let b = bmi' p in
    "The BMI of "++ name p ++ " is " ++ b



{- Fractions -}

-- in C++
--   typedef (Int, Int) Frac
--   using Frac = (Int, Int)

-- Type synonym
type Frac = (Int,      Int)
--           numerator denominator
-- (a, b) represents the fraction (a / b)

-- Note: Built in fraction type:
--   import Data.Ratio
--   type Frac' = Ratio Int
--   (%) :: Int -> Int -> Ratio Int

zeroFrac :: Frac
zeroFrac = (0,0)

oneFrac :: Frac
oneFrac = (1,1)

twoThirdsFrac :: Frac
twoThirdsFrac = undefined

-- (an / ad) + (bn / bd)
addFrac :: Frac -> Frac -> Frac
addFrac (an, ad) (bn, bd) = (an * bd + bn * ad,ad * bd)

-- - (an / ad)
negFrac :: Frac -> Frac
negFrac (an, ad) = (-an,ad)

-- (an / ad) - (bn / bd)
subFrac :: Frac -> Frac -> Frac
subFrac (an, ad) (bn, bd) = (an * bd - bn * ad,ad * bd)

-- (an / ad) * (bn / bd)
mulFrac :: Frac -> Frac -> Frac
mulFrac (an, ad) (bn, bd) = (an * bn, ad * bd)


invFrac :: Frac -> Frac
invFrac (an,ad) = (ad,an)

-- (an / ad) / (bn / bd)
divFrac :: Frac -> Frac -> Frac
divFrac a b = mulFrac a (invFrac b)

-- divFrac a b = a `mulFrac` invFrac b

{-

EXPLANATION:
divFrac (1,2)(3,4) = (4,6)

1   3     1   4    4
- / -  ~  - * -  = - 
2   4     2   3    6

-}

--  Compute the reduced form of a fraction!
--  Examples:
--  ∙ reduceFrac (0, 5)   == (0, 1)
--  ∙ reduceFrac (-1, -1) == (1, 1)
--  ∙ reduceFrac (3, 9)   == (1, 3)
--
--  Use the built in "gcd" (greatest common divisor) function!
reduceFrac :: Frac -> Frac
reduceFrac (an, ad) = 
  let g = gcd an ad * (if ad<0 then -1 else 1) in
     (an `div` g, ad `div` g)




