module Exercise01 where


-- Exercise 1.1
-----------------------------------------------------------------------------------------------------------
-- You need to double the integer and return it.
-- Example: doubleInteger 2 -> 4
-----------------------------------------------------------------------------------------------------------

doubleInteger :: Num a => a -> a 
doubleInteger = (*2)

doubleInteger' :: Num a => a -> a 
doubleInteger' x = x*2

-- Exercise 1.2
-----------------------------------------------------------------------------------------------------------
-- Bob needs a fast way to calculate the volume of a cuboid with three values: 
-- the length,width and height of the cuboid. 
-- Write a function to help Bob with this calculation.
-----------------------------------------------------------------------------------------------------------

getVolumeOfCuboid :: Double -> Double -> Double -> Double 
getVolumeOfCuboid l w h = l * w * h

getVolumeOfCuboid' :: Num a => a -> a -> a -> a 
getVolumeOfCuboid' l w h = product [l, w, h]

-- Exercise 1.3
-----------------------------------------------------------------------------------------------------------
-- Implement a function which compares two numbers. 
-- If 2 numbers are equal, the result is True, otherwise False
-----------------------------------------------------------------------------------------------------------

numberEqual :: Int -> Int -> Bool
numberEqual  x y = x == y

numberEqual' :: Int -> Int -> Bool
numberEqual'  = (==)

-- Exercise 1.4
-----------------------------------------------------------------------------------------------------------
-- Implement a function which compares two numbers. 
-- If 2 numbers are different, the result is True, otherwise False
-----------------------------------------------------------------------------------------------------------

numberDiff :: Int -> Int -> Bool
numberDiff x y = x /= y

numberDiff' :: Int -> Int -> Bool
numberDiff' = (/=)

-- Exercise 1.5
-----------------------------------------------------------------------------------------------------------
-- Implement a function which gives 2 numbers 
-- and the result is the addition of those 2 numbers
-----------------------------------------------------------------------------------------------------------

numPlus :: Num a => a -> a -> a
numPlus x y = x + y

numPlus' :: Num a => a -> a -> a
numPlus' = (+)

-- Exercise 1.6
-----------------------------------------------------------------------------------------------------------
-- Implement a function which gives 2 numbers 
-- and the result is the subtraction of those 2 numbers
-----------------------------------------------------------------------------------------------------------

numSub :: Int -> Int -> Int
numSub x y = x - y

numSub' :: Int -> Int -> Int
numSub' = (-)

-- Exercise 1.7
-----------------------------------------------------------------------------------------------------------
-- Implement a function which multiplies two numbers.
-----------------------------------------------------------------------------------------------------------

multiply :: Num a => a -> a -> a 
multiply = (*)

multiply' :: Num a => a -> a -> a 
multiply' x y = x * y

-- Exercise 1.8
-----------------------------------------------------------------------------------------------------------
-- Implement a function which divide two numbers.
-----------------------------------------------------------------------------------------------------------

numDiv :: Float -> Float -> Float
numDiv = (/)

numDiv' :: Float -> Float -> Float
numDiv' x y = x / y

-- Exercise 1.9
-----------------------------------------------------------------------------------------------------------
-- Implement a function which returns the next number
-- Example: 1 -> 2
-- 			6 -> 7
-----------------------------------------------------------------------------------------------------------

nextNum :: Int -> Int
nextNum a = succ a

nextNum' :: Int -> Int
nextNum' = succ 

-- Exercise 1.10
-----------------------------------------------------------------------------------------------------------
-- Implement a function which returns the higher number from 2 numbers
-----------------------------------------------------------------------------------------------------------

maxNum :: Int -> Int -> Int
maxNum a b = max a b

maxNum' :: Int -> Int -> Int
maxNum' = max

-- Exercise 1.11
-----------------------------------------------------------------------------------------------------------
-- Implement a function which returns the lower number from 2 numbers
-----------------------------------------------------------------------------------------------------------

minNum :: Int -> Int -> Int
minNum x y = min x y

minNum' :: Int -> Int -> Int
minNum' = min

-- Exercise 1.12
-----------------------------------------------------------------------------------------------------------
-- Implement a function which divide 2 numbers and the result is an integer division
-- Example: 9 / 2 = 4
-- 			11 / 3 = 3
-----------------------------------------------------------------------------------------------------------

divNum :: Int -> Int -> Int
divNum x y = div x y

divNum' :: Int -> Int -> Int
divNum' x y = x `div` y

-- Exercise 1.13
-----------------------------------------------------------------------------------------------------------
-- Implement a function which divide 2 numbers and the result is a modulo
-- Example: 9 modulo 2 = 1
-- 			13 modulo 5 = 3
-----------------------------------------------------------------------------------------------------------

modNum :: Int -> Int -> Int
modNum x y = mod x y

modNum' :: Int -> Int -> Int
modNum' x y = x `mod` y

-- Exercise 1.14
-----------------------------------------------------------------------------------------------------------
-- Implement a function which check a number is an odd number or not
-----------------------------------------------------------------------------------------------------------

oddNum :: Int -> Bool
oddNum x = odd x

oddNum' :: Int -> Bool
oddNum'  = odd 

-- Exercise 1.15
-----------------------------------------------------------------------------------------------------------
-- Implement a function which check a number is an even number or not
-----------------------------------------------------------------------------------------------------------

evenNum :: Int -> Bool
evenNum x = even x

evenNum' :: Int -> Bool
evenNum' = even

-- Exercise 1.16
-----------------------------------------------------------------------------------------------------------
-- Create a function that takes an integer as an argument 
-- and returns "Even" for even numbers or "Odd" for odd numbers.
-----------------------------------------------------------------------------------------------------------

evenOrOdd :: Integral a => a -> [Char] 
evenOrOdd n = if n `mod` 2 == 0
             then "Even" 
             else "Odd"

evenOrOdd' :: Integral a => a -> [Char] 
evenOrOdd' n
    | even n = "Even" 
    | otherwise = "Odd"

-- Exercise 1.17
-----------------------------------------------------------------------------------------------------------
-- Timmy & Sarah think they are in love, but around where they live, 
-- they will only know once they pick a flower each. 
-- If one of the flowers has an even number of petals 
-- and the other has an odd number of petals it means they are in love.
-- Write a function that will take the number of petals of each flower 
-- and return true if they are in love and false if they aren't.
-----------------------------------------------------------------------------------------------------------

inlove :: Int -> Int -> Bool 
inlove a b = if (mod a 2) == (mod b 2) 
            then False 
            else True

inlove' :: Int -> Int -> Bool 
inlove' a b = odd (a + b)

-- Exercise 1.18
-----------------------------------------------------------------------------------------------------------
-- Create a function that takes an integer and make it negative 
-- if it’s positive and make it positive if it’s negative
-- Example: (-1) -> 1
-- 			2 -> (-2)
-----------------------------------------------------------------------------------------------------------

negativeNum :: Integer -> Integer
negativeNum a = negate a

negativeNum' :: Integer -> Integer
negativeNum' a = (-a)

-- Exercise 1.19
-----------------------------------------------------------------------------------------------------------
-- Create a function that takes an integer and make it absolute
-----------------------------------------------------------------------------------------------------------

absoluteNum :: Integer -> Integer
absoluteNum a = abs a

absoluteNum' :: (Num a, Ord a) => a -> a 
absoluteNum' a = if a < 0 
                then (-a) 
                else a 

-- Exercise 1.20
-----------------------------------------------------------------------------------------------------------
-- In this simple assignment you are given a number and have to make it negative. 
-- But maybe the number is already negative?
-- Examples
-- makeNegative 1 -- return -1 
-- makeNegative (-5) -- return -5 
-- makeNegative 0 -- return 0 
-- makeNegative 0.12 -- return -0.12
-----------------------------------------------------------------------------------------------------------

makeNegative :: Num a => a -> a 
makeNegative = negate . abs

makeNegative' :: (Num a, Ord a) => a -> a 
makeNegative' n 
    | n <= 0 = n 
    | otherwise = -n
