module Exercise06 where


-- Exercise 6.1
-----------------------------------------------------------------------------------------------------------
-- Complete the function so that it finds the average of the three scores passed to it 
-- and returns the letter value associated with that grade.

-- Numerical Score Letter Grade
-- 90 <= score <= 100  'A'
-- 80 <= score < 90  'B'
-- 70 <= score < 80  'C'
-- 60 <= score < 70  'D'
-- 0 <= score < 60 'F'
-- Tested values are all between 0 and 100. 
-- Theres is no need to check for negative values or values greater than 100
-----------------------------------------------------------------------------------------------------------

getGrade :: Double -> Double -> Double -> Char
getGrade x y z
  | score >= 90 = 'A'
  | score >= 80 = 'B'
  | score >= 70 = 'C'
  | score >= 60 = 'D'
  | otherwise = 'F'
  where score = (x + y + z) / 3


-- Exercise 6.2
-----------------------------------------------------------------------------------------------------------
-- Your classmates asked you to copy some paperwork for them. 
-- You know that there are 'n' classmates and the paperwork has 'm' pages.

-- Your task is to calculate how many blank pages do you need. If n < 0 or m < 0 return 0.

-- Example:
--          n= 5, m=5: 25
--          n=-5, m=5:  0
-----------------------------------------------------------------------------------------------------------

paperwork :: Int -> Int -> Int
paperwork n m 
    | n < 0 = 0
    | m < 0 = 0 
    | otherwise = m * n


-- Exercise 6.3
-----------------------------------------------------------------------------------------------------------
-- Your function takes two arguments:
-- current father's age (years)
-- current age of his son (years)
-- Сalculate how many years ago the father was twice as old as his son 
-- (or in how many years he will be twice as old).

-- father + x = 2*(son + x) => x = father - 2*son
-- father - x = 2*(son - x) => x = - (father - 2*son)
-----------------------------------------------------------------------------------------------------------

twice_as_old :: Int -> Int -> Int
twice_as_old father son = abs $ father - 2*son


-- Exercise 6.4
-----------------------------------------------------------------------------------------------------------
-- A hero is on his way to the castle to complete his mission. 
-- However, he's been told that the castle is surrounded with a couple of powerful dragons! 
-- each dragon takes 2 bullets to be defeated, our hero has no idea how many bullets he should carry.. 
-- Assuming he's gonna grab a specific given number of bullets 
-- and move forward to fight another specific given number of dragons, will he survive?

-- Return True if yes, False otherwise
-----------------------------------------------------------------------------------------------------------

hero :: Int -> Int -> Bool
hero bullets dragons = bullets >= 2 * dragons


hero' :: Int -> Int -> Bool
hero' b d = div b d >= 2


-- Exercise 6.5
-----------------------------------------------------------------------------------------------------------
-- Complete the square sum function so that it squares each number passed into it 
-- and then sums the results together.

-- For example, for [1, 2, 2] it should return 9 because 1^2 + 2^2 + 2^2 = 9
-----------------------------------------------------------------------------------------------------------

squareSum :: [Integer] -> Integer
squareSum = sum . map (^2)


squareSum' :: [Integer] -> Integer
squareSum' [] = 0
squareSum' (x:xs) = x ^ 2 + squareSum xs


-- Exercise 6.6
-----------------------------------------------------------------------------------------------------------
-- Character recognition software is widely used to digitise printed texts. 
-- Thus the texts can be edited, searched and stored on a computer.
-- When documents (especially pretty old ones written with a typewriter), 
-- are digitised character recognition softwares often make mistakes.

-- Your task is correct the errors in the digitised text. 
-- You only have to handle the following mistakes:

-- S is misinterpreted as 5
-- O is misinterpreted as 0
-- I is misinterpreted as 1
-- The test cases contain numbers only by mistake
-----------------------------------------------------------------------------------------------------------

correct :: String -> String
correct = map correctHelp

correctHelp char =
  case char of
    '5' -> 'S'
    '0' -> 'O'
    '1' -> 'I'
    c -> c


correct' :: String -> String
correct' xs = [fix x | x <- xs] where
  fix letter
    | letter == '5' = 'S'
    | letter == '0' = 'O'
    | letter == '1' = 'I'
    | otherwise     = letter


-- Exercise 6.7
-----------------------------------------------------------------------------------------------------------
-- Kids drink toddy.
-- Teens drink coke.
-- Young adults drink beer.
-- Adults drink whisky.
-- Make a function that receive age, and return what they drink.

-- Rules:

-- Children under 14 old.
-- Teens under 18 old.
-- Young under 21 old.
-- Adults have 21 or more.

-- Examples: (Input --> Output)

-- 13 --> "drink toddy"
-- 17 --> "drink coke"
-- 18 --> "drink beer"
-- 20 --> "drink beer"
-- 30 --> "drink whisky"
-----------------------------------------------------------------------------------------------------------

peopleWithAgeDrink :: Int -> String
peopleWithAgeDrink age = "drink " ++ drink
  where drink | age < 14  = "toddy"
              | age < 18  = "coke"
              | age < 21  = "beer"
              | otherwise = "whisky"


-- Exercise 6.8
-----------------------------------------------------------------------------------------------------------
-- After a hard quarter in the office you decide to get some rest on a vacation. 
-- So you will book a flight for you and your girlfriend and try to leave all the mess behind you.
-- You will need a rental car in order for you to get around in your vacation. 
-- The manager of the car rental makes you some good offers.
-- Every day you rent the car costs $40. 
-- If you rent the car for 7 or more days, you get $50 off your total. 
-- Alternatively, if you rent the car for 3 or more days, you get $20 off your total.

-- Write a code that gives out the total amount for different days(d)
-----------------------------------------------------------------------------------------------------------

rentalCarCost :: Int -> Int
rentalCarCost d
 | d >= 7    = total - 50
 | d >= 3    = total - 20
 | otherwise = total
 where total = 40 * d


-- Exercise 6.9
-----------------------------------------------------------------------------------------------------------
-- The first century spans from the year 1 up to and including the year 100, 
-- the second century - from the year 101 up to and including the year 200, etc.

-- Task: Given a year, return the century it is in.

-- Examples
-- 1705 --> 18
-- 1900 --> 19
-- 1601 --> 17
-- 2000 --> 20
-----------------------------------------------------------------------------------------------------------

century::Int -> Int
century year = div (year + 99) 100


century'::Int -> Int
century' year = (year - 1) `div` 100 + 1


-- Exercise 6.10
-----------------------------------------------------------------------------------------------------------
-- The cockroach is one of the fastest insects. 
-- Write a function which takes its speed in km per hour and returns it in cm per second, 
-- rounded down to the integer (= floored).

-- For example: 1.08 --> 30
-- Note! The input is a Real number (actual type is language dependent) and is >= 0. 
-- The result should be an Integer.
-----------------------------------------------------------------------------------------------------------

cockroachSpeed :: Double -> Integer
cockroachSpeed = floor . (/3600) . (*100000)


-- Exercise 6.11
-----------------------------------------------------------------------------------------------------------
-- You are given two interior angles (in degrees) of a triangle.
-- Write a function to return the 3rd.
-- Note: only positive integers will be tested.
-- https://en.wikipedia.org/wiki/Triangle
-----------------------------------------------------------------------------------------------------------

otherAngle :: Int -> Int -> Int
otherAngle a b = 180 - (a + b)


-- Exercise 6.12
-----------------------------------------------------------------------------------------------------------
-- Return the Nth Even Number

-- Example(Input --> Output)

-- 1 --> 0 (the first even number is 0)
-- 3 --> 4 (the 3rd even number is 4 (0, 2, 4))
-- 100 --> 198
-- 1298734 --> 2597466
-----------------------------------------------------------------------------------------------------------


nthEven :: Int -> Int
nthEven n
  | n > 0 = [0,2..] !! (n - 1)
  | otherwise = -([2, 4..] !! (-n))


nthEven' :: Int -> Int
nthEven' n | n > 0 =  last $ take n $ filter even [0..]
          | otherwise = negate $ last $ take (negate n+2) $ filter even [0..]


-- Exercise 6.13
-----------------------------------------------------------------------------------------------------------
-- Clock shows h hours, m minutes and s seconds after midnight.
-- Your task is to write a function which returns the time since midnight in milliseconds.

-- Example:
-- h = 0
-- m = 1
-- s = 1
-- result = 61000

-- Input constraints:
-- 0 <= h <= 23
-- 0 <= m <= 59
-- 0 <= s <= 59
-----------------------------------------------------------------------------------------------------------

past :: Int -> Int -> Int -> Int
past h m s = if (h `elem` [0 .. 23] && m  `elem` [0 .. 59] && s  `elem` [0 .. 59]) 
  then (h * 3600000)+ (m * 60000)+ (s * 1000) 
  else 0


past' :: Int -> Int -> Int -> Int
past' h m s = sum $ zipWith (*) [3600000, 60000, 1000] [h, m, s]


-- Exercise 6.14
-----------------------------------------------------------------------------------------------------------
-- Nathan loves cycling.
--Because Nathan knows it is important to stay hydrated, 
--he drinks 0.5 litres of water per hour of cycling.

-- You get given the time in hours and you need to return the number of litres Nathan will drink, 
-- rounded to the smallest value.

-- For example:
-- time = 3 ----> litres = 1
-- time = 6.7---> litres = 3
-- time = 11.8--> litres = 5
-----------------------------------------------------------------------------------------------------------

litres :: Double -> Integer
litres = floor . (/2)

-- Exercise 6.15
-----------------------------------------------------------------------------------------------------------
-- Write a program that finds the summation of every number from 1 to num. 
-- The number will always be a positive integer greater than 0.

-- For example:
-- summation(2) -> 3
-- 1 + 2

-- summation(8) -> 36
-- 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8
-----------------------------------------------------------------------------------------------------------

summation :: Integer -> Integer 
summation x = sum [1..x]


summation' :: Integer -> Integer
summation' 0 = 0
summation' n = n + summation(n-1)


-- Exercise 6.16
-----------------------------------------------------------------------------------------------------------
-- Write a function which calculates the average of the numbers in a given list
-----------------------------------------------------------------------------------------------------------


avg :: [Float] -> Float
avg [] = 0
avg l = sum l / (fromIntegral $ length l)


avg' :: [Float] -> Float
avg' l = sum l / sum[ 1 | _ <- l]


-- Exercise 6.17
-----------------------------------------------------------------------------------------------------------
-- Given a string of digits, you should replace any digit below 5 with '0' 
-- and any digit 5 and above with '1'. Return the resulting string.

-- Note: input will never be an empty string
-----------------------------------------------------------------------------------------------------------

fakeBin :: String -> String
fakeBin [] = []
fakeBin (x:xs) = (if x < '5' then '0' else '1'):fakeBin xs


-- Exercise 6.18
-----------------------------------------------------------------------------------------------------------
-- Given an list of integers your solution should find the smallest integer.

-- For example:
-- Given [34, 15, 88, 2] your solution will return 2
-- Given [34, -345, -1, 100] your solution will return -345
-- You can assume, for the purpose of this kata, that the supplied list will not be empty.
-----------------------------------------------------------------------------------------------------------

findSmallestInteger' :: [Int] -> Int
findSmallestInteger' = minimum


findSmallestInteger :: [Int] -> Int
findSmallestInteger [] = error "empty list"
findSmallestInteger [x] = x
findSmallestInteger (x:y:xs) = if x < y then findSmallestInteger(x:xs)
                               else findSmallestInteger(y:xs)

-- Exercise 6.19
-----------------------------------------------------------------------------------------------------------
-- Your task is to create a function that does four basic mathematical operations.

--The function should take three arguments - operation(string/char), value1(number), value2(number).
--The function should return result of numbers after applying the chosen operation.

--Examples(Operator, value1, value2) --> output
-- ('+', 4, 7) --> 11
-- ('-', 15, 18) --> -3
-- ('*', 5, 5) --> 25
-- ('/', 49, 7) --> 7
-----------------------------------------------------------------------------------------------------------

basicOp :: Char -> Int -> Int -> Int
basicOp '+' = (+)
basicOp '-' = (-)
basicOp '*' = (*)
basicOp '/' = div


basicOp' :: Char -> Int -> Int -> Int
basicOp' c x y | c == '+'  = x + y
              | c == '-'  = x - y
              | c == '*'  = x * y
              | c == '/'  = x `div` y
              | otherwise = 0


-- Exercise 6.20
-----------------------------------------------------------------------------------------------------------
-- Define a function that removes duplicates from an array of numbers and returns it as a result.

-- The order of the sequence has to stay the same.
-----------------------------------------------------------------------------------------------------------

distinct :: [Int] -> [Int]
distinct [] = []
distinct (x:xs) = x : distinct (filter (/=x) xs)


distinct' :: [Int] -> [Int]
distinct' [] = []
distinct' (x:xs) = x : filter (/= x) (distinct xs)