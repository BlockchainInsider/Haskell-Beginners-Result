module Exercise11 where


-- Exercise 11.1
-----------------------------------------------------------------------------------------------------------
{- 
In this kata you're expected to sort an array of 32-bit integers in ascending order of the number of on bits they have.

E.g Given the array [7, 6, 15, 8]

7 has 3 on bits (000...0111)
6 has 2 on bits (000...0011)
15 has 4 on bits (000...1111)
8 has 1 on bit (000...1000)
So the array in sorted order would be [8, 6, 7, 15].

In cases where two numbers have the same number of bits, compare their real values instead.

E.g between 10 (...1010) and 12 (...1100), 
they both have the same number of on bits '2' but the integer 10 is less than 12 so it comes first in sorted order.

Your task is to write the function sortBybit() that takes an array of integers and sort them as described above.

Example:

[3, 8, 3, 6, 5, 7, 9, 1]   =>    [1, 8, 3, 3, 5, 6, 9, 7]
-}
-----------------------------------------------------------------------------------------------------------


import Data.Word (Word32)
import Data.Bits (popCount)
import Data.List (sort, sortOn)

sortByBit :: [Word32] -> [Word32]
sortByBit = sortOn popCount . sort


-- Exercise 11.2
-----------------------------------------------------------------------------------------------------------
{- 
Triangular numbers are so called because of the equilateral triangular shape 
that they occupy when laid out as dots. i.e.

1st (1)   2nd (3)    3rd (6)
*          **        ***
           *         **
                     *
You need to return the nth triangular number. You should return 0 for out of range values:

For example: (Input --> Output)

0 --> 0
2 --> 3
3 --> 6
-10 --> 0
-}
-----------------------------------------------------------------------------------------------------------


triangular :: Integer -> Integer
triangular n = sum [1..n]


-- Exercise 11.3
-----------------------------------------------------------------------------------------------------------
{-
A pangram is a sentence that contains every single letter of the alphabet at least once. 
For example, the sentence "The quick brown fox jumps over the lazy dog" is a pangram, 
because it uses the letters A-Z at least once (case is irrelevant).

Given a string, detect whether or not it is a pangram. 
Return True if it is, False if not. Ignore numbers and punctuation.
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char

isPangram :: String -> Bool
isPangram str = all (`elem` (map toLower str)) ['a'..'z']


-- Exercise 11.4
-----------------------------------------------------------------------------------------------------------
{- 
A Narcissistic Number is a positive number which is the sum of its own digits, 
each raised to the power of the number of digits in a given base. 
In this Exercise, we will restrict ourselves to decimal (base 10).

For example, take 153 (3 digits), which is narcisstic:

    1^3 + 5^3 + 3^3 = 1 + 125 + 27 = 153
and 1652 (4 digits), which isn't:

    1^4 + 6^4 + 5^4 + 2^4 = 1 + 1296 + 625 + 16 = 1938
The Challenge:

Your code must return true or false (not 'true' and 'false') 
depending upon whether the given number is a Narcissistic number in base 10. 

Error checking for text strings or other invalid inputs is not required, 
only valid positive non-zero integers will be passed into the function.
-}
-----------------------------------------------------------------------------------------------------------


narcissistic :: Integral n => n -> Bool
narcissistic n = n == sum (map (^length digits) digits) where 
  digits = (map (`mod`10) . takeWhile (>0) . iterate (`div`10)) n


-- Exercise 11.5
-----------------------------------------------------------------------------------------------------------
{- 
Kiyo has been given a series of problems and she needs your help to solve them!

You will be given a two-dimensional array such as the one below.

a = 
[
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [1, 2, 3, 4, 5, 6, 7, 8, 9],
  [1, 2, 3, 4, 5, 6, 7, 8, 9]
]

Remove everything but odd integers from each sub-array.

Sum the remaining odd integers of each sub-array. 

Sum of odds ( a[0] = 1 + 3 + 5 + 7 + 9 ) = 25
       
Find the Least common multiple of the arrays.

 (25, 25, 25, 25, 25, 25, 25, 25, 25)
  ^                                ^ 
  |                                |
a[0]-----------------------------a[8]

example : lcm( 25, 25, 25, 25, 25, 25, 25, 25, 25 ) = 25

example : lcm( 37, 29, 19, 38, 31, 28, 15, 24, 9 ) = 1592632440
Integers are between 0 and 9. Sub-array size is always 9. 
The number of sub-arrays varies between 9 and 18.

Watch out for non-integers mixed in the arrays. If all arrays are empty return 0.

https://en.wikipedia.org/wiki/Least_common_multiple
-}
-----------------------------------------------------------------------------------------------------------


import Data.Either (rights)

kiyoLcm :: [[Either Char Int]] -> Int
kiyoLcm = foldr1 lcm . map (sum . filter odd . rights)



-- Exercise 11.6
-----------------------------------------------------------------------------------------------------------
{- 
Your task is to make a function that can take any non-negative integer as an argument 
and return it with its digits in descending order. 
Essentially, rearrange the digits to create the highest possible number.

Examples:
Input: 42145 Output: 54421

Input: 145263 Output: 654321

Input: 123456789 Output: 987654321
-}
-----------------------------------------------------------------------------------------------------------


import Data.List (sort)

descendingOrder :: Integer -> Integer
descendingOrder = read . reverse . sort . show


-- Exercise 11.7
-----------------------------------------------------------------------------------------------------------
{-
Your goal in this kata is to implement a difference function, 
which subtracts one list from another and returns the result.

It should remove all values from list a, which are present in list b keeping their order.

difference [1,2] [1] == [2]
If a value is present in b, all of its occurrences must be removed from the other:

difference [1,2,2,2,3] [2] == [1,3]
-}
-----------------------------------------------------------------------------------------------------------

difference :: Eq a => [a] -> [a] -> [a]
difference a b = filter (`notElem` b) a


-- Exercise 11.8
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that takes an array of consecutive (increasing) letters as input 
and that returns the missing letter in the array.

You will always get an valid array. 
And it will be always exactly one letter be missing. 
The length of the array will always be at least 2.
The array will always contain letters in only one case.

Example:

['a','b','c','d','f'] -> 'e' ['O','Q','R','S'] -> 'P'

["a","b","c","d","f"] -> "e"
["O","Q","R","S"] -> "P"
-}
-----------------------------------------------------------------------------------------------------------


findMissingLetter :: [Char] -> Char
findMissingLetter (x:xs) | head xs == next = findMissingLetter xs
                         | otherwise       = next
                           where next = succ x



-- Exercise 11.9
-----------------------------------------------------------------------------------------------------------
{- 
Complete the solution so that the function will break up camel casing, using a space between words.

Example
"camelCasing"  =>  "camel Casing"
"identifier"   =>  "identifier"
""             =>  ""
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char(isUpper)

solution :: String -> String
solution (x:[]) = [x]
solution (x:xs) = x : c ++ solution xs 
  where c = if isUpper (head xs) then " " else ""


-- Exercise 11.10
-----------------------------------------------------------------------------------------------------------
{- 
A twin prime is a prime number that differs from another prime number by 2. 
Write a function named is_twin_prime which takes an int parameter and returns true if it is a twin prime, else false.

Examples
given 5, which is prime
5 + 2 = 7, which is prime 
5 - 2 = 3, which is prime
hence, 5 has two prime twins and it is a Twin Prime.

given 7, which is prime
7 - 2 = 5, which is prime
7 + 2 = 9. which is not prime
hence, 7 has one prime twin, and it is a Twin Prime.
given 9, which is not prime 
hence, 9 is not a Twin Prime

given 953, which is prime
953 - 2 = 951, which is not prime
953 + 2 = 955, which is not prime 
hence, 953 is not a Twin Prime
-}
-----------------------------------------------------------------------------------------------------------


isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = n > 2 && (length [x | x <- [2 .. round (sqrt (fromIntegral n))], mod n x == 0]) == 0
          
isTwinPrime :: Integer -> Bool
isTwinPrime n = isPrime n && (isPrime (n+2)||isPrime (n-2))


-- Exercise 11.11
-----------------------------------------------------------------------------------------------------------
{- 
Write a generic function chainer that takes a starting value, 
and an array of functions to execute on it (array of symbols for Ruby).

The input for each function is the output of the previous function (except the first function, 
which takes the starting value as its input). Return the final value after execution is complete.

add = (+ 1)
mul = (* 30)

chain 2 [add, mult] -> 90
-}
-----------------------------------------------------------------------------------------------------------


chain :: x -> [ x -> x ] -> x
chain x [] = x
chain x functions = chain (head functions x) (tail functions)


-- Exercise 11.12
-----------------------------------------------------------------------------------------------------------
{- 
Given a number return the closest number to it that is divisible by 10.

Example input:

22
25
37
Expected output:

20
30
40
-}
-----------------------------------------------------------------------------------------------------------


closestMultiple10 :: Int -> Int
closestMultiple10 = (*10) . (`div` 10) . (+5)


-- Exercise 11.13
-----------------------------------------------------------------------------------------------------------
{- 
In the following 6 digit number:

283910
91 is the greatest sequence of 2 consecutive digits.

In the following 10 digit number:

1234567890
67890 is the greatest sequence of 5 consecutive digits.

Complete the solution so that it returns the greatest sequence of five consecutive digits found within the number given. 
The number will be passed in as a string of only digits. 
It should return a five digit integer. 
The number passed may be as large as 1000 digits.
-}
-----------------------------------------------------------------------------------------------------------


digit5 :: String -> Int
digit5 [] = 0
digit5 xs = (read $ take 5 xs) `max` (digit5 $ tail xs)


-- Exercise 11.14
-----------------------------------------------------------------------------------------------------------
{- 
In a small town the population is p0 = 1000 at the beginning of a year. 
The population regularly increases by 2 percent per year 
and moreover 50 new inhabitants per year come to live in the town. 
How many years does the town need to see its population greater or equal to p = 1200 inhabitants?

At the end of the first year there will be: 
1000 + 1000 * 0.02 + 50 => 1070 inhabitants

At the end of the 2nd year there will be: 
1070 + 1070 * 0.02 + 50 => 1141 inhabitants (** number of inhabitants is an integer **)

At the end of the 3rd year there will be:
1141 + 1141 * 0.02 + 50 => 1213

It will need 3 entire years.
More generally given parameters:

p0, percent, aug (inhabitants coming or leaving each year), p (population to surpass)

the function nb_year should return n number of entire years needed to get a population greater or equal to p.

aug is an integer, percent a positive or null floating number, p0 and p are positive integers (> 0)

Examples:
nb_year(1500, 5, 100, 5000) -> 15
nb_year(1500000, 2.5, 10000, 2000000) -> 10
Note:
Don't forget to convert the percent parameter as a percentage in the body of your function: 
if the parameter percent is 2 you have to convert it to 0.02.
-}
-----------------------------------------------------------------------------------------------------------


nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p 
  | p0 >= p = 0
  | otherwise = 1 + nbYear nextYear percent aug p
  where nextYear = p0 + floor ((fromIntegral p0) * (percent / 100)) + aug


-- Exercise 11.15
-----------------------------------------------------------------------------------------------------------
{- 
Write a function, factory, that takes a number as its parameter and returns another function.

The returned function should take an array of numbers as its parameter, 
and return an array of those numbers multiplied by the number that was passed into the first function.

In the example below, 5 is the number passed into the first function. 
So it returns a function that takes an array and multiplies all elements in it by five.

Translations and comments (and upvotes) welcome!

Example
let fives = factory 5      -- returns a function - fives
fives [1, 2, 3]            -- returns [5, 10, 15]
-}
-----------------------------------------------------------------------------------------------------------


factory :: Int -> [Int] -> [Int]
factory = map . (*)


-- Exercise 11.16
-----------------------------------------------------------------------------------------------------------
{- 
Given a string, turn each character into its ASCII character code and join them together to create a number 
- let's call this number total1:

'ABC' --> 'A' = 65, 'B' = 66, 'C' = 67 --> 656667
Then replace any incidence of the number 7 with the number 1, and call this number 'total2':

total1 = 656667
              ^
total2 = 656661
              ^
Then return the difference between the sum of the digits in total1 and total2:

  (6 + 5 + 6 + 6 + 6 + 7)
- (6 + 5 + 6 + 6 + 6 + 1)
-------------------------
                       6
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char (ord)

calc :: String -> Int
calc x = sum [6 | i <- concatMap (show . ord) x, i == '7']


-- Exercise 10.17
-----------------------------------------------------------------------------------------------------------
{- 
You are given three integer inputs: length, minimum, and maximum.

Return a string that:

Starts at minimum
Ascends one at a time until reaching the maximum, then
Decends one at a time until reaching the minimum
repeat until the string is the appropriate length
Examples:

 length: 5, minimum: 1, maximum: 3   ==>  "12321"
 length: 14, minimum: 0, maximum: 2  ==>  "01210121012101"
 length: 11, minimum: 5, maximum: 9  ==>  "56789876567"
Notes:

length will always be non-negative
negative numbers can appear for minimum and maximum values
hyphens/dashes ("-") for negative numbers do count towards the length
the resulting string must be truncated to the exact length provided
return an empty string if maximum < minimum or length == 0
minimum and maximum can equal one another and result in a single number repeated for the length of the string
-}
-----------------------------------------------------------------------------------------------------------


ascendDescend :: Int -> Int -> Int -> String
ascendDescend len minBound maxBound
  | minBound > maxBound = ""
  | otherwise = take len (concatMap show (cycle ([minBound..maxBound] ++ [maxBound - 1, maxBound - 2..minBound + 1])))


-- Exercise 11.18
-----------------------------------------------------------------------------------------------------------
{- 
You have to search all numbers from inclusive 1 to inclusive a given number x, 
that have the given digit d in it.
The value of d will always be 0 - 9.
The value of x will always be greater than 0.

You have to return as an array

the count of these numbers,
their sum
and their product.

For example:
x = 11
d = 1
->
Numbers: 1, 10, 11
Return: [3, 22, 110]

If there are no numbers, which include the digit, return [0,0,0].

-}
-----------------------------------------------------------------------------------------------------------


numbersWithDigitInside :: Int -> Int -> [Int]
numbersWithDigitInside x d = res
  where l = filter (elem (head (show d)) . show) [1..x]
        x1 = length l
        x2 = sum l
        x3 = product l
        res | null l = [0,0,0]
            | otherwise = [x1,x2,x3]


-- Exercise 11.19
-----------------------------------------------------------------------------------------------------------
{- 
Given an array (arr) as an argument complete the function countSmileys that should return the total number of smiling faces.

Rules for a smiling face:

Each smiley face must contain a valid pair of eyes. Eyes can be marked as : or ;
A smiley face can have a nose but it does not have to. Valid characters for a nose are - or ~
Every smiling face must have a smiling mouth that should be marked with either ) or D
No additional characters are allowed except for those mentioned.

Valid smiley face examples: :) :D ;-D :~)
Invalid smiley faces: ;( :> :} :]

Example
countSmileys([':)', ';(', ';}', ':-D']);       // should return 2;
countSmileys([';D', ':-(', ':-)', ';~)']);     // should return 3;
countSmileys([';]', ':[', ';*', ':$', ';-D']); // should return 1;
Note
In case of an empty array return 0. You will not be tested with invalid input (input will always be an array). 
Order of the face (eyes, nose, mouth) elements will always be the same.
-}
-----------------------------------------------------------------------------------------------------------



countSmileys :: [String] -> Int
countSmileys = length . filter correct

correct [e,m] = e `elem` eyes && m `elem` mouth
correct [e,n,m] = n `elem` nose && correct [e,m]
correct _ = False

eyes = ":;"
nose = "-~"
mouth = ")D"


-- Exercise 11.20
-----------------------------------------------------------------------------------------------------------
{- 
Given an array (or list) of scores, return the array of ranks for each value in the array. 
The largest value has rank 1, the second largest value has rank 2, and so on. 
Ties should be handled by assigning the same rank to all tied values. For example:

ranks([9,3,6,10]) = [2,4,3,1]
and

ranks([3,3,3,3,3,5,1]) = [2,2,2,2,2,1,7]
because there is one 1st place value, a five-way tie for 2nd place, and one in 7th place.
-}
-----------------------------------------------------------------------------------------------------------


import Data.List
import Data.Maybe

ranks :: (Eq a, Ord a) => [a] -> [Int]
ranks xs = map (succ . fromJust . flip elemIndex sortd) xs
  where sortd = reverse $ sort xs
