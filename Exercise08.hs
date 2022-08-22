module Exercise08 where


-- Exercise 8.1
-----------------------------------------------------------------------------------------------------------
{- 
Your task is very simple. 
Just write a function takes an input string of lowercase letters 
and returns true/false depending on whether the string is in alphabetical order or not.

Examples (input -> output)
"kata" -> false ('a' comes after 'k')
"ant" -> true (all characters are in alphabetical order)
Good luck :)
-}
-----------------------------------------------------------------------------------------------------------

alphabetic' :: String -> Bool
alphabetic' [] = True
alphabetic' xs = and $ zipWith (<=) xs (tail xs)


alphabetic :: String -> Bool
alphabetic []  = True
alphabetic [a] = True
alphabetic (a:b:xs) 
  | a > b = False
  | otherwise = alphabetic (b:xs)


-- Exercise 8.2
-----------------------------------------------------------------------------------------------------------
{- 
In a factory a printer prints labels for boxes. 
For one kind of boxes the printer has to use colors which, for the sake of simplicity, are named with letters from a to m.

The colors used by the printer are recorded in a control string. 
For example a "good" control string would be aaabbbbhaijjjm meaning that the printer used 
three times color a, four times color b, one time color h then one time color a...

Sometimes there are problems: lack of colors, technical malfunction and a "bad" control string is produced 
e.g. aaaxbbbbyyhwawiwjjjwwm with letters not from a to m.

You have to write a function printer_error which given a string will return the error rate of the printer 
as a string representing a rational whose numerator is the number of errors and the denominator the length of the control string. 
Don't reduce this fraction to a simpler expression.

The string has a length greater or equal to one and contains only letters from ato z.

Examples:
s="aaabbbbhaijjjm"
printer_error(s) => "0/14"

s="aaaxbbbbyyhwawiwjjjwwm"
printer_error(s) => "8/22"
-}
-----------------------------------------------------------------------------------------------------------

printerError :: [Char] -> [Char]
printerError s = show errNum ++ "/" ++ show totNum
  where
    errNum = length $ filter (`elem` ['n'..'z']) s
    totNum  = length s   


-- Exercise 8.3
-----------------------------------------------------------------------------------------------------------
{-
Digital root is the recursive sum of all the digits in a number.

Given n, take the sum of the digits of n. If that value has more than one digit, 
continue reducing in this way until a single-digit number is produced. 
The input will be a non-negative integer.

Examples
    16  -->  1 + 6 = 7
   942  -->  9 + 4 + 2 = 15  -->  1 + 5 = 6
132189  -->  1 + 3 + 2 + 1 + 8 + 9 = 24  -->  2 + 4 = 6
493193  -->  4 + 9 + 3 + 1 + 9 + 3 = 29  -->  2 + 9 = 11  -->  1 + 1 = 2

https://en.wikipedia.org/wiki/Digital_root#Significance_and_formula_of_the_digital_root
-}
-----------------------------------------------------------------------------------------------------------


digitalRoot :: Integral a => a -> a
digitalRoot 0 = 0
digitalRoot n = 1 + (n - 1) `mod` 9


digitalRoot' :: Integer -> Integer
digitalRoot' x
  | x < 0 = error "digitalRoot: number must be >= 0"
  | x < 10 = x
  | otherwise = digitalRoot' $! sum (map (read . (:[])) (show x))


-- Exercise 8.4
-----------------------------------------------------------------------------------------------------------
{- 
There is an list with some numbers. All numbers are equal except for one. Try to find it!

getUnique [1, 1, 1, 2, 1, 1] -- Result is 2
getUnique [0, 0, 0.55, 0, 0] -- Result is 0.55
It’s guaranteed that list contains at least 3 numbers.

The tests contain some very huge arrays, so think about performance.
-}
-----------------------------------------------------------------------------------------------------------

getUnique :: [Int] -> Int
getUnique (x:xs)
  | x `elem` xs = getUnique $ filter (/=x) xs
  | otherwise = x


-- Exercise 8.5
-----------------------------------------------------------------------------------------------------------
{- The national go-kart racing competition is taking place in your local town 
and you've been called for building the winners podium with the available wooden blocks. 
Thankfully you are in a wood-rich area, number of blocks are always at least 6.

Remember a classic racing podium have three platforms for first, second and third places. 
First place is the highest and second place is higher than third. 
Also notice that platforms are arranged as 2nd - 1st - 3rd.

The organizers want a podium that satisfies:

The first place platform has the minimum height posible
The second place platform has the closest height to first place
All platforms have heights greater than zero.
Task
Given the numbers of blocks available, return an list with the heights of 2nd, 1st, 3rd places platforms.

Examples (input -> output)
11 ->   [4, 5, 2]
6  ->   [2, 3, 1]
10 ->   [4, 5, 1]
-}
-----------------------------------------------------------------------------------------------------------

racePodium :: Int -> (Int, Int, Int)
racePodium 7 = (2, 4, 1)
racePodium n = let x = n `div` 2 in (x-1, x, n-2*x+1)


-- Exercise 8.6
-----------------------------------------------------------------------------------------------------------
{- 
Complete the solution so that it returns true if the first argument(string) passed in ends with the 2nd argument (also a string).

Examples:

solution('abc', 'bc') // returns true
solution('abc', 'd') // returns false
-}
-----------------------------------------------------------------------------------------------------------

solution :: String -> String -> Bool
solution s s2 = drop (length s - length s2) s == s2


-- Exercise 8.7
-----------------------------------------------------------------------------------------------------------
{- 
Given a two-dimensional array of integers, 
return the flattened version of the array with all the integers in the sorted (ascending) order.

Example:

Given [[3, 2, 1], [4, 6, 5], [], [9, 7, 8]], your function should return [1, 2, 3, 4, 5, 6, 7, 8, 9].
-}
-----------------------------------------------------------------------------------------------------------

flatSort :: [[Int]] -> [Int]
flatSort = quickSort.concat

quickSort:: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = lesser xs ++ [x] ++ greater xs
    where 
        lesser = quickSort . (filter (< x) )
        greater = quickSort . (filter (>= x))


-- Exercise 8.8
-----------------------------------------------------------------------------------------------------------
{- 
Your task, is to calculate the minimal number of moves to win the game "Towers of Hanoi", with given number of disks.

What is "Towers of Hanoi"?
Towers of Hanoi, is a simple game consisting of three rods, 
and a number of disks of different sizes which can slide onto any rod. 
The puzzle starts with the disks in a neat stack in ascending order of size on one rod, 
the smallest at the top, thus making a conical shape.

The objective of the puzzle is to move the entire stack to another rod, obeying the following simple rules:

Only one disk can be moved at a time.
Each move consists of taking the upper disk from one of the stacks and placing it on top of another stack 
i.e. a disk can only be moved if it is the uppermost disk on a stack.
No disk may be placed on top of a smaller disk.

https://en.wikipedia.org/wiki/Tower_of_Hanoi
-}
-----------------------------------------------------------------------------------------------------------

hanoi :: Int -> Int
hanoi n = 2 ^ n - 1


hanoi' :: Int -> Int
hanoi' = pred . (2 ^)


-- Exercise 8.9
-----------------------------------------------------------------------------------------------------------
{- 
Create a function with two arguments that will return an array of the first (n) multiples of (x).

Assume both the given number and the number of times to count will be positive numbers greater than 0.

Return the results as an array (or list in Python, Haskell or Elixir).

Examples:

countBy 1 10 `shouldBe` [1,2,3,4,5,6,7,8,9,10]
countBy 2  5 `shouldBe` [2,4,6,8,10]
-}
-----------------------------------------------------------------------------------------------------------

countBy :: Integer -> Int -> [Integer]
countBy x n = take n [x, x + x..]


countBy' :: Integer -> Integer -> [Integer]
countBy' x n = map (*x) [1..n]


-- Exercise 8.10
-----------------------------------------------------------------------------------------------------------
{- 
Count the number of occurrences of each character and return it as a list of tuples in order of appearance. 
For empty output return an empty list.

Example:

orderedCount "abracadabra" == [('a', 5), ('b', 2), ('r', 2), ('c', 1), ('d', 1)]
-}
-----------------------------------------------------------------------------------------------------------

orderedCount :: String -> [(Char, Int)]
orderedCount "" = []
orderedCount (c:xs) = (c,succ $ length $ filter (==c) xs):orderedCount (filter (/=c) xs)


-- Exercise 8.11
-----------------------------------------------------------------------------------------------------------
{- 
Modify the spacify function so that it returns the given string with spaces inserted between each character.

spacify "hello world" -- returns "h e l l o   w o r l d"
-}
-----------------------------------------------------------------------------------------------------------


spacify :: String -> String
spacify [] = []
spacify [x] = [x]
spacify (x:xs) = x : ' ' : spacify xs


-- Exercise 8.12
-----------------------------------------------------------------------------------------------------------
{- 
You are going to be given a word. Your job is to return the middle character of the word. 
If the word's length is odd, return the middle character. 
If the word's length is even, return the middle 2 characters.

#Examples:

getMiddle("test") should return "es"

getMiddle("testing") should return "t"

getMiddle("middle") should return "dd"

getMiddle("A") should return "A"

#Input

A word (string) of length 0 < str < 1000 You do not need to test for this. 
This is only here to tell you that you do not need to worry about your solution timing out.

#Output

The middle character(s) of the word represented as a string.
-}
-----------------------------------------------------------------------------------------------------------


getMiddle :: String -> String
getMiddle "" = ""
getMiddle [a] = [a]
getMiddle [a,b] = [a,b]
getMiddle (_:x) = getMiddle (init x)


getMiddle' :: String -> String
getMiddle' s
  | odd l = [s !! h]
  | otherwise = take 2 $ drop (h - 1) s
  where
    l = length s
    h = l `div` 2


-- Exercise 8.13
-----------------------------------------------------------------------------------------------------------
{- 
You will be given an list and a limit value. 
You must check that all values in the list are below or equal to the limit value. 
If they are, return true. Else, return false.

You can assume all values in the list are numbers.
-}
-----------------------------------------------------------------------------------------------------------


smallEnough :: [Int] -> Int -> Bool
smallEnough xs v = and $ map (<= v) xs


smallEnough' :: [Int] -> Int -> Bool
smallEnough' xs v = all (<= v) xs


-- Exercise 8.14
-----------------------------------------------------------------------------------------------------------
{- 
Deoxyribonucleic acid (DNA) is a chemical found in the nucleus of cells and carries the "instructions" 
for the development and functioning of living organisms.

If you want to know more: http://en.wikipedia.org/wiki/DNA

In DNA strings, symbols "A" and "T" are complements of each other, 
as "C" and "G". Your function receives one side of the DNA (string, except for Haskell); 
you need to return the other complementary side. 
DNA strand is never empty or there is no DNA at all (again, except for Haskell).

Example: (input --> output)

dnaStrand [A,T,G,C] shouldBe [T,A,C,G] 
dnaStrand [G,T,A,T] shouldBe [C,A,T,A] 
dnaStrand [A,A,A,A] shouldBe [T,T,T,T]
-}
-----------------------------------------------------------------------------------------------------------

import Codewars.Kata.DNA.Types

-- data Base = A | T | G | C
type DNA = [Base]

dnaStrand :: DNA -> DNA
dnaStrand = map switch
  where switch A = T
        switch T = A
        switch C = G
        switch G = C


-- Exercise 8.15
-----------------------------------------------------------------------------------------------------------
{- 
You live in the city of Cartesia where all roads are laid out in a perfect grid. 
You arrived ten minutes too early to an appointment, 
so you decided to take the opportunity to go for a short walk. 
The city provides its citizens with a Walk Generating App on their phones 
-- everytime you press the button it sends you an list of one-letter strings representing directions to walk 
(eg. ['n', 's', 'w', 'e']). 
You always walk only a single block for each letter (direction) 
and you know it takes you one minute to traverse one city block, 
so create a function that will return true if the walk the app gives you will take you exactly ten minutes 
(you don't want to be early or late!) and will, of course, return you to your starting point. 
Return false otherwise.

Note: 
you will always receive a valid List containing a random assortment of direction letters ('n', 's', 'e', or 'w' only). 
It will never give you an empty list (that's not a walk, that's standing still!).
-}
-----------------------------------------------------------------------------------------------------------

isValidWalk :: [Char] -> Bool
isValidWalk walk
  |length  (take 11 walk) /= 10 = False
  |(length $ filter (=='n') walk) /= (length $ filter (=='s') walk) = False
  |(length $ filter (=='e') walk )/= (length $ filter (=='w') walk) = False
  |otherwise = True


-- Exercise 8.16
-----------------------------------------------------------------------------------------------------------
{- 
Write a function, persistence, 
that takes in a positive parameter num and returns its multiplicative persistence, 
which is the number of times you must multiply the digits in num until you reach a single digit.

For example (Input --> Output):

39 --> 3 (because 3*9 = 27, 2*7 = 14, 1*4 = 4 and 4 has only one digit)
999 --> 4 (because 9*9*9 = 729, 7*2*9 = 126, 1*2*6 = 12, and finally 1*2 = 2)
4 --> 0 (because 4 is already a one-digit number)
-}
-----------------------------------------------------------------------------------------------------------


persistence :: Int -> Int
persistence n
  | n < 10    = 0
  | otherwise = 1 + persistence (digitProduct n)

digitProduct :: Int -> Int
digitProduct n
  | n < 10    = n
  | otherwise = r * digitProduct q
  where
    (q, r) = n `divMod` 10


-- Exercise 8.17
-----------------------------------------------------------------------------------------------------------
{- 
The fusc function is defined recursively as follows:

1. fusc(0) = 0
2. fusc(1) = 1
3. fusc(2 * n) = fusc(n)
4. fusc(2 * n + 1) = fusc(n) + fusc(n + 1)
The 4 rules above are sufficient to determine the value of fusc for any non-negative input n. 
For example, let's say you want to compute fusc(10).

fusc(10) = fusc(5), by rule 3.
fusc(5) = fusc(2) + fusc(3), by rule 4.
fusc(2) = fusc(1), by rule 3.
fusc(1) = 1, by rule 2.
fusc(3) = fusc(1) + fusc(2) by rule 4.
fusc(1) and fusc(2) have already been computed are both equal to 1.
Putting these results together fusc(10) = fusc(5) = fusc(2) + fusc(3) = 1 + 2 = 3

Your job is to produce the code for the fusc function. 
In this kata, your function will be tested with small values of n, 
so you should not need to be concerned about stack overflow or timeouts.

Hint: Use recursion.
-}
-----------------------------------------------------------------------------------------------------------

fusc :: Int -> Int
fusc 0 = 0
fusc 1 = 1
fusc n = fusc (div n 2) + if odd n then fusc (div n 2 + 1) else 0


-- Exercise 8.18
-----------------------------------------------------------------------------------------------------------
{- 
Check to see if a string has the same amount of 'x's and 'o's. 
The method must return a boolean and be case insensitive. The string can contain any char.

Examples input/output:

XO("ooxx") => true
XO("xooxx") => false
XO("ooxXm") => true
XO("zpzpzpp") => true // when no 'x' and 'o' is present should return true
XO("zzoo") => false
-}
-----------------------------------------------------------------------------------------------------------

xo :: String -> Bool
xo str = countL str "xX" == countL str "oO"

countL :: String -> String -> Int
countL str s = length $ filter (\i -> elem i s )  str


-- Exercise 8.19
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that takes in a string of one or more words, and returns the same string, 
but with all five or more letter words reversed (Just like the name of this Kata). 
Strings passed in will consist of only letters and spaces. 
Spaces will be included only when more than one word is present.

Examples:

spinWords( "Hey fellow warriors" ) => returns "Hey wollef sroirraw" 
spinWords( "This is a test") => returns "This is a test" 
spinWords( "This is another test" )=> returns "This is rehtona test"
-}
-----------------------------------------------------------------------------------------------------------

spinWords :: String -> String
spinWords = unwords . map (\x -> if length x >= 5 then reverse x else x) . words


-- Exercise 8.20
-----------------------------------------------------------------------------------------------------------
{- 
You are given two numbers a and b where 0 ≤ a ≤ b. 
Imagine you construct an list of all the integers from a to b inclusive. 
You need to count the number of 1s in the binary representations of all the numbers in the list.

Example
For a = 2 and b = 7, the output should be 11

Given a = 2 and b = 7 the list is: [2, 3, 4, 5, 6, 7]. 
Converting the numbers to binary, we get [10, 11, 100, 101, 110, 111], 
which contains 1 + 2 + 1 + 2 + 2 + 3 = 11 1s.
-}
-----------------------------------------------------------------------------------------------------------

rangeBitCount :: Int -> Int -> Int
rangeBitCount a b = sum . map (sum . bins) $ [a .. b]

bins :: Integral a => a -> [a]
bins 0 = []
bins n = bins (n `div` 2) ++ [n `mod` 2]