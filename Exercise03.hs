module Exercise03 where


-- Exercise 3.1
-----------------------------------------------------------------------------------------------------------
-- Implement a function which create a string of “hello” 10 times
-- Example: “hello hello hello hello …..”
-----------------------------------------------------------------------------------------------------------

helloList :: String
helloList = take 50 (cycle "hello ")

-- Exercise 3.2
-----------------------------------------------------------------------------------------------------------
-- Convert the expression {2 * x | x ∈ N, x ≤ 10} to a function of Haskell
-----------------------------------------------------------------------------------------------------------

doubleList :: [Int]
doubleList = [x*2 | x <- [1..10]]

-- Exercise 3.3
-----------------------------------------------------------------------------------------------------------
-- In the list of Exercise 3.2 we want only the elements 
-- which after being doubled, are greater than or equal to 12
-----------------------------------------------------------------------------------------------------------

doubleList' :: [Int]
doubleList' = [x*2 | x <- [1..10], x*2 >= 12]

-- Exercise 3.4
-----------------------------------------------------------------------------------------------------------
-- Implement a function which get all numbers from 50 to 100 
-- whose remainder when divided by 7 is 3
-----------------------------------------------------------------------------------------------------------

div7List :: [Int]
div7List = [ x | x <- [50..100], x `mod` 7 == 3]

-- Exercise 3.5
-----------------------------------------------------------------------------------------------------------
-- Implement a function which replace every odd number greater than 10 with "BANG!", 
-- and every odd number less than 10 with "BOOM!". 
-- If a number isn’t odd, we throw it out of our list
-----------------------------------------------------------------------------------------------------------

boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- Exercise 3.6
-----------------------------------------------------------------------------------------------------------
-- Implement a function which create a list with all numbers 
-- from 10 to 20 that are not 13, 15 or 19
-----------------------------------------------------------------------------------------------------------

ex6List :: [Int]
ex6List = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

-- Exercise 3.7
-----------------------------------------------------------------------------------------------------------
-- Implement a function which draw values from 2 lists 
-- and get the products of all possible combinations that are more than 50
-----------------------------------------------------------------------------------------------------------

productList :: [Int] -> [Int] -> [Int]
productList xs ys = [ x*y | x <- xs, y <- ys ]

-- Exercise 3.8
-----------------------------------------------------------------------------------------------------------
-- Implement a function which draw values from 2 lists 
-- and get the products of all possible combinations of numbers in those lists
-----------------------------------------------------------------------------------------------------------

product50List :: [Int] -> [Int] -> [Int]
product50List xs ys = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

-- Exercise 3.9
-----------------------------------------------------------------------------------------------------------
-- Implement a function which make a list comprehension 
-- that combines a list of adjectives and a list of nouns
-----------------------------------------------------------------------------------------------------------

adjNoun :: [String] -> [String] -> [String]
nouns = ["hobo","frog","pope"]
adjs = ["lazy","grouchy","scheming"]
adjNoun adjs nouns = [adj ++ " " ++ noun | adj <- adjs, noun <- nouns]

-- Exercise 3.10
-----------------------------------------------------------------------------------------------------------
-- Using list comprehensions to write our own version of the length function
-- We’ll call it length1
-----------------------------------------------------------------------------------------------------------

length1 :: [a] -> Int 
length1 xs = sum [1 | _ <- xs]

-- Exercise 3.11
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a string and removes all the lowercase letters
-----------------------------------------------------------------------------------------------------------

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Exercise 3.12
-----------------------------------------------------------------------------------------------------------
-- Implement a function which remove the spaces from the string, 
-- then return the resultant string.
-----------------------------------------------------------------------------------------------------------

noSpace :: String -> String 
noSpace s = [x | x <- s, x /= ' ']

noSpace' :: String -> String
noSpace' = filter (/=' ')

-- Exercise 3.13
-----------------------------------------------------------------------------------------------------------
-- Implement a function which caculate factorial 
-----------------------------------------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n = product [1..n]

-- Exercise 3.14
-----------------------------------------------------------------------------------------------------------
-- Implement a function which calculate the circumference of a circle
-----------------------------------------------------------------------------------------------------------

circumference :: Float -> Float
circumference r = 2 * pi * r

-- Exercise 3.15
-----------------------------------------------------------------------------------------------------------
-- Implement a function which compares 2 numbers.
-- f a b => a > b -> GT (Greater Than)
-- 			a < b -> LT (Lesser Than)
--			a = b -> EQ (Equal)
-----------------------------------------------------------------------------------------------------------

compareNum :: (Ord a) => a -> a -> Ordering
compareNum a b = a `compare` b 

-- Exercise 3.16
-----------------------------------------------------------------------------------------------------------
-- Implement a function which can transform a string into a number
-- Examples: "1234" --> 1234 
-- 			 "605" --> 605 
--			 "1405" --> 1405 
-- 			 "-7" --> -7
-----------------------------------------------------------------------------------------------------------

stringToNumber :: String -> Integer 
stringToNumber = read

stringToNumber' :: String -> Integer 
stringToNumber' s = read s :: Integer


-- Exercise 3.17
-----------------------------------------------------------------------------------------------------------
-- Create a function that takes 2 integers in form of a string as an input, and outputs the sum (also as a string):
-- Example: (Input1, Input2 -->Output)
-- 			"4", "5" --> "9" 
--			"34", "5" --> "39" 
--			"", "" --> "0" 
--			"2", "" --> "2" 
--			"-5", "3" --> "-2"
-----------------------------------------------------------------------------------------------------------

sumStr :: String -> String -> String 
sumStr a b = show(readInt(a) + readInt(b)) 

readInt :: String -> Int 
readInt "" = 0 
readInt s = read s

-- Exercise 3.18
-----------------------------------------------------------------------------------------------------------
-- Make a function that will return a greeting statement that uses an input; 
-- your program should return, "Hello, <name> how are you doing today?".
-----------------------------------------------------------------------------------------------------------

greeting :: [Char] -> [Char]
greeting name = "Hello, " ++ name ++ " how are you doing today?"

-- Exercise 3.19
-----------------------------------------------------------------------------------------------------------
-- You get an array of numbers, return the sum of all of the positives ones.
-- Example: [1,-4,7,12] => 1 + 7 + 12 = 20
-----------------------------------------------------------------------------------------------------------

positiveSum :: [Int] -> Int 
positiveSum numbers = sum [ a | a <- numbers, a > 0]

positiveSum' :: [Int] -> Int 
positiveSum' = sum . filter (>0)

-- Exercise 3.20
-----------------------------------------------------------------------------------------------------------
-- Complete the function that takes a non-negative integer n as input, 
-- and returns a list of all the powers of 2 
-- with the exponent ranging from 0 to n
-- Examples: n = 0 ==> [1] # [2^0] 
-- 			 n = 1 ==> [1, 2] # [2^0, 2^1] 
--			 n = 2 ==> [1, 2, 4] # [2^0, 2^1, 2^2]
-----------------------------------------------------------------------------------------------------------

powersOfTwo :: Int -> [Int] 
powersOfTwo n = [ 2^x | x <- [0 .. n] ]

powersOfTwo' :: Int -> [Int] 
powersOfTwo' n = map (2 ^) [0..n]

