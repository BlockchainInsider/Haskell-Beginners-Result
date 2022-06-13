module Exercise02 where


-- Exercise 2.1
-----------------------------------------------------------------------------------------------------------
-- Implement a function which connect 2 lists or 2 strings
-- Example: “hello” “ world” -> “hello world”
-- 			[1,2,3] [4,5,6] -> [1,2,3,4,5,6]
-----------------------------------------------------------------------------------------------------------

connectList :: [a] -> [a] -> [a]
connectList x y = x ++ y

connectList' :: [a] -> [a] -> [a]
connectList' = (++)

-- Exercise 2.2
-----------------------------------------------------------------------------------------------------------
-- Implement a function which add 1 element to the head of a list
-- Example: ‘A’ “ Car” -> “A Car”
--			 1 [2,3,4,5] -> [1,2,3,4,5]
-----------------------------------------------------------------------------------------------------------

addElem :: a -> [a] -> [a]
addElem x xs = x:xs

-- Exercise 2.3
-----------------------------------------------------------------------------------------------------------
-- Implement a function which get an element of a list by index
-----------------------------------------------------------------------------------------------------------

getElem :: Int -> [a] -> a 
getElem n xs = xs !! n

-- Exercise 2.4
-----------------------------------------------------------------------------------------------------------
-- Complete the function that takes two integers (a, b, where a < b) 
-- and return an array of all integers between the input parameters, including them.
-- For example: a = 1 b = 4 --> [1, 2, 3, 4]
-----------------------------------------------------------------------------------------------------------

between :: Integer -> Integer -> [Integer] 
between a b = [a..b]

between' :: Integer -> Integer -> [Integer] 
between' = enumFromTo

-- Exercise 2.5
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list and returns its head, or first element
-----------------------------------------------------------------------------------------------------------

headList :: [a] -> a 
headList xs = head xs

headList' :: [a] -> a 
headList' = head

-- Exercise 2.6
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list and returns its tail. 
-- In other words, it chops off a list’s head
-----------------------------------------------------------------------------------------------------------

tailList :: [a] -> [a]
tailList xs = tail xs

tailList' :: [a] -> [a]
tailList' = tail

-- Exercise 2.7
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list and returns a list’s last element
-----------------------------------------------------------------------------------------------------------

lastList :: [a] -> a
lastList = last

lastList' :: [a] -> a
lastList' xs = last xs

-- Exercise 2.8
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list 
-- and returns everything except its last element
-----------------------------------------------------------------------------------------------------------

initList :: [a] -> [a]
initList = init 

initList' :: [a] -> [a]
initList' xs = init xs

-- Exercise 2.9
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list 
-- and returns its length (the number of element in the list)
-----------------------------------------------------------------------------------------------------------

lengthList :: [a] -> Int 
lengthList = length

lengthList' :: [a] -> Int 
lengthList' xs = length xs

-- Exercise 2.10
-----------------------------------------------------------------------------------------------------------
-- Implement a function which checks if a list is empty
-----------------------------------------------------------------------------------------------------------

nullList :: [a] -> Bool
nullList = null

nullList' :: [a] -> Bool
nullList' xs = null xs

-- Exercise 2.11
-----------------------------------------------------------------------------------------------------------
-- Implement a function which reverses a list
-----------------------------------------------------------------------------------------------------------

reverseList :: [a] -> [a]
reverseList = reverse

reverseList' :: [a] -> [a]
reverseList' xs = reverse xs

-- Exercise 2.12
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a number and a list. 
-- It extracts the specified number elements from the beginning of the list
-- Example: 3 [5,4,3,2,1] -> [5,4,3]
-----------------------------------------------------------------------------------------------------------

takeList :: Int -> [a] -> [a]
takeList = take

takeList' :: Int -> [a] -> [a]
takeList' n xs = take n xs

-- Exercise 2.13
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a number and a list. 
-- It drops (at most) the specified number of elements from the beginning of a list
-- Example: 3 [5,4,3,2,1] -> [2,1]
-----------------------------------------------------------------------------------------------------------

dropList :: Int -> [a] -> [a]
dropList = drop

dropList' :: Int -> [a] -> [a]
dropList' n xs = drop n xs

-- Exercise 2.14
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list of items 
-- that can be put in some kind of order and returns the largest element
-----------------------------------------------------------------------------------------------------------

maxList :: (Num a , Ord a) => [a] -> a  
maxList = maximum

maxList' :: (Num a , Ord a) => [a] -> a  
maxList' xs = maximum xs

-- Exercise 2.15
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list of items 
-- that can be put in some kind of order and returns the smallest element
-----------------------------------------------------------------------------------------------------------

minList :: (Num a , Ord a) => [a] -> a
minList = minimum

minList' :: (Num a , Ord a) => [a] -> a
minList' xs = minimum xs

-- Exercise 2.16
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list of numbers and returns their sum
-----------------------------------------------------------------------------------------------------------

sumList :: [Integer] -> Integer
sumList = sum

sumList' :: [Integer] -> Integer
sumList' xs = sum xs

-- Exercise 2.17
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes a list of numbers and returns their product
-----------------------------------------------------------------------------------------------------------

productList :: [Integer] -> Integer
productList = product

productList' :: [Integer] -> Integer
productList' xs = product xs

-- Exercise 2.18
-----------------------------------------------------------------------------------------------------------
-- Implement a function which takes an item and a list of items 
-- and tells us if that item is an element of the list
-----------------------------------------------------------------------------------------------------------

elemList :: (Eq a) => a -> [a] -> Bool
elemList x xs = x `elem` xs

elemList' :: (Eq a) => a -> [a] -> Bool
elemList' x xs = elem x xs

-- Exercise 2.19
-----------------------------------------------------------------------------------------------------------
-- Implement a function which create a list of all alphabet from a to z
-----------------------------------------------------------------------------------------------------------

alphaList :: [Char]
alphaList = ['a'..'z']

upperAlphaList :: [Char]
upperAlphaList = ['A'..'Z']

-- Exercise 2.20
-----------------------------------------------------------------------------------------------------------
-- Implement a function which create a list of all even numbers from 2 to 50
-----------------------------------------------------------------------------------------------------------

evenList :: [Int]
evenList = [2,4..50]
