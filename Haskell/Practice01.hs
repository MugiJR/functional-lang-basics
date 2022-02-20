import Prelude hiding(even)

inc :: Int -> Int
inc x = x + 1

f:: Int -> Int -> Bool 
f x y = x/=y

checkeven :: Int -> Bool
checkeven n = n `mod` 2 == 0

isOperator :: Char -> Bool 
isOperator c = c == '%' || c == '/' || c == '*' || c == '+' || c == '-'

m :: String
m = "mr"

even x = mod x 2 == 0

divides :: Int -> Int -> Bool 
divides a b = mod b a == 0
