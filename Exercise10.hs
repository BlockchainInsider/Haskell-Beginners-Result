module Exercise10 where


-- Exercise 10.1
-----------------------------------------------------------------------------------------------------------
{- 
Some numbers have the property to be divisible by 2 and 3. 
Other smaller subset of numbers have the property to be divisible by 2, 3 and 5. 
Another subset with less abundant numbers may be divisible by 2, 3, 5 and 7. 
These numbers have something in common: their prime factors are contiguous primes.

Implement a function that finds the amount of numbers that have the first N primes as factors below a given limit.

Let's see some cases:

count_specMult(3, 200)  =>  6 

The first 3 primes are: 2, 3 and 5.

And the found numbers below 200 are: 30, 60, 90, 120, 150, 180.
-}
-----------------------------------------------------------------------------------------------------------

primes :: [Integer]
primes = 2:filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime n = go primes
  where go (x:xs)
          | x*x>n        = True
          | mod n x == 0 = False
          | otherwise    = go xs

countSpecMult :: Int -> Integer -> Integer
countSpecMult n maxval = div maxval . product $ take n primes


-- Exercise 10.2
-----------------------------------------------------------------------------------------------------------
{- 
Your car is old, it breaks easily. 
The shock absorbers are gone and you think it can handle about 15 more bumps before it dies totally.

Unfortunately for you, your drive is very bumpy! 
Given a string showing either flat road (_) or bumps (n). 
If you are able to reach home safely by encountering 15 bumps or less, return Woohoo!, 
otherwise return Car Dead
-}
-----------------------------------------------------------------------------------------------------------

bump :: String -> String

bump str 
  | length (filter ('n'==) str) > 15 = "Car Dead"
  | otherwise                        = "Woohoo!"


-- Exercise 10.3
-----------------------------------------------------------------------------------------------------------
{-
The number 89 is the first integer with more than one digit 
that fulfills the property partially introduced in the title of this exercise. 
What's the use of saying "Eureka"? Because this sum gives the same number.

In effect: 89 = 8^1 + 9^2

The next number in having this property is 135.

See this property again: 135 = 1^1 + 3^2 + 5^3

We need a function to collect these numbers, 
that may receive two integers a, b that defines the range [a, b] (inclusive) 
and outputs a list of the sorted numbers in the range that fulfills the property described above.

Let's see some cases:

sum_dig_pow(1, 10) == [1, 2, 3, 4, 5, 6, 7, 8, 9]

sum_dig_pow(1, 100) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 89]
If there are no numbers of this kind in the range [a, b] the function should output an empty list.

sum_dig_pow(90, 100) == []
-}
-----------------------------------------------------------------------------------------------------------


sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter verify intList 
  where intList = [a..b] 

verify :: Int -> Bool
verify x = applyPowers (toDigit x) == x 

toDigit :: Int -> [Int] 
toDigit 0 = []
toDigit n = toDigit (n `div` 10) ++ [n `mod` 10] 

applyPowers :: [Int] -> Int
applyPowers xs = sum (zipWith (^) xs [1..])


-- Exercise 10.4
-----------------------------------------------------------------------------------------------------------
{- 
Build a pyramid-shaped tower given a positive integer number of floors. 
A tower block is represented with "*" character.

For example, a tower with 3 floors looks like this:


  "  *  ",
  " *** ", 
  "*****"

And a tower with 6 floors looks like this:

  "     *     ", 
  "    ***    ", 
  "   *****   ", 
  "  *******  ", 
  " ********* ", 
  "***********"
-}
-----------------------------------------------------------------------------------------------------------

buildTower :: Int -> [String]
buildTower n = [(sp x++ st x++ sp x) | x <- [1..n]]
    where sp x = replicate (n-x) ' '
          st x = replicate (2*x-1) '*'


-- Exercise 10.5
-----------------------------------------------------------------------------------------------------------
{- 
Take 2 strings s1 and s2 including only letters from a to z. 
Return a new sorted string, the longest possible, containing distinct letters - each taken only once - coming from s1 or s2.

Examples:
a = "xyaabbbccccdefww"
b = "xxxxyyyyabklmopq"
longest(a, b) -> "abcdefklmopqwxy"

a = "abcdefghijklmnopqrstuvwxyz"
longest(a, a) -> "abcdefghijklmnopqrstuvwxyz"
-}
-----------------------------------------------------------------------------------------------------------


longest :: [Char] -> [Char] -> [Char]
longest a b = filter (`elem` a++b) ['a'..'z']



-- Exercise 10.6
-----------------------------------------------------------------------------------------------------------
{- 
Given a string made up of letters a, b, and/or c, 
switch the position of letters a and b (change a to b and vice versa). Leave any incidence of c untouched.

Example:

'acb' --> 'bca'
'aabacbaa' --> 'bbabcabb'
-}
-----------------------------------------------------------------------------------------------------------


switcheroo :: String -> String
switcheroo = map switch
  where switch 'a' = 'b'
        switch 'b' = 'a'
        switch c = c


-- Exercise 10.7
-----------------------------------------------------------------------------------------------------------
{-
We have the following sequence:

f(0) = 0
f(1) = 1
f(2) = 1
f(3) = 2
f(4) = 4;
f(n) = f(n-1) + f(n-2) + f(n-3) + f(n-4) + f(n-5);
Your task is to give the number of total values for the odd terms of the sequence up to the n-th term (included). 
(The number n (of n-th term) will be given as a positive integer)

The values 1 (one) is the only that is duplicated in the sequence and should be counted only once.

E.g.

count_odd_pentaFib(5) -----> 1 # because the terms up to 5 are: 0, 1, 1, 2, 4, 8 (only 1 is odd and counted once)
Other examples:

 count_odd_pentaFib(10) ------> 3 #because the odds terms are: [1, 1, 31, 61] (three different values)

count_odd_pentaFib(15) ------> 5 # beacause the odd terms are: [1, 1, 31, 61, 1793, 3525] (five different values)
-}
-----------------------------------------------------------------------------------------------------------

countOddPentaFib :: Int -> Int
countOddPentaFib 0 = 0
countOddPentaFib 1 = 1
countOddPentaFib n = (n - 1) `div` 6 + (n - 2) `div` 6 + 1


-- Exercise 10.8
-----------------------------------------------------------------------------------------------------------
{- 
Given an array/list [] of integers , 
Construct a product array Of same size Such That prod[i] is equal to The Product of all the elements of Arr[] except Arr[i].

Notes
Array/list size is at least 2 .

Array/list's numbers Will be only Positives

Repetition of numbers in the array/list could occur.

Input >> Output Examples
productArray ({12,20}) ==>  return {20,12}
Explanation:
The first element in prod [] array 12 is the product of all array's elements except the first element

The second element 20 is the product of all array's elements except the second element .

productArray ({1,5,2}) ==> return {10,2,5}
Explanation:
The first element 10 is the product of all array's elements except the first element 1

The second element 2 is the product of all array's elements except the second element 5

The Third element 5 is the product of all array's elements except the Third element 2.

productArray ({10,3,5,6,2}) return ==> {180,600,360,300,900}
Explanation:
The first element 180 is the product of all array's elements except the first element 10

The second element 600 is the product of all array's elements except the second element 3

The Third element 360 is the product of all array's elements except the third element 5

The Fourth element 300 is the product of all array's elements except the fourth element 6

Finally ,The Fifth element 900 is the product of all array's elements except the fifth element 2
-}
-----------------------------------------------------------------------------------------------------------


productArray :: [Integer] -> [Integer]
productArray xs = map (div (product xs)) xs



-- Exercise 10.9
-----------------------------------------------------------------------------------------------------------
{- 
You've arrived at a carnival and head straight for the duck shooting tent. Why wouldn't you?

You will be given a set amount of ammo, and an aim rating of between 1 and 0. 
No your aim is not always perfect - hey maybe someone fiddled with the sights on the gun...

Anyway your task is to calculate how many successful shots 
you will be able to make given the available ammo and your aim score, 
then return a string representing the pool of ducks, 
with those ducks shot marked with 'X' and those that survived left unchanged. 
You will always shoot left to right.

Example of start and end duck string with two successful shots:

Start ---> |~~~~~22~2~~~~~|

Bang!! Bang!!

End ---> |~~~~~XX~2~~~~~|

All inputs will be correct type and never empty.
-}
-----------------------------------------------------------------------------------------------------------


shootDucks :: Int -> String -> String
shootDucks 0 xs = xs
shootDucks _ [] = []
shootDucks n ('2':xs) = 'X' : shootDucks (n-1) xs
shootDucks n (x:xs) = x : shootDucks n xs

duckShoot :: Int -> Float->  String -> String 
duckShoot ammo aim ducks = shootDucks (floor $ fromIntegral ammo * aim) ducks


-- Exercise 10.10
-----------------------------------------------------------------------------------------------------------
{- 
Give the summation of all even numbers in a Fibonacci sequence up to, but not including, 
the number passed to your function. 
Or, in other words, sum all the even Fibonacci numbers that are lower than the given number n 
(n is not the nth element of Fibonnacci sequence) without including n.

The Fibonacci sequence is a series of numbers where the next value is the addition of the previous two values. 
The series starts with 0 and 1:

0 1 1 2 3 5 8 13 21...

For example:

fibSum 0 -> 0
fibSum 33 -> 10
fibSum 25997544 -> 19544084
-}
-----------------------------------------------------------------------------------------------------------


fibSum :: Int -> Int
fibSum n = sum $ filter even $ takeWhile (< n) fibs'
  where
    fibs' = 1:1:(zipWith (+) fibs' (tail fibs'))


-- Exercise 10.11
-----------------------------------------------------------------------------------------------------------
{- 
Create function fib that returns n'th element of Fibonacci sequence (classic programming task).
-}
-----------------------------------------------------------------------------------------------------------


fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-2) + fib (n-1)


-- Exercise 10.12
-----------------------------------------------------------------------------------------------------------
{- 
And here is Fibonacci again. This time we want to go one step further. 
Our fib() function must be faster! Can you do it?

In case you don't know, what the Fibonacci number is:

The nth Fibonacci number is defined by the sum of the two previous Fibonacci numbers. 
In our case: fib(1) := 0 and fib(2) := 1. 
With these initial values you should be able to calculate each following Fibonacci number.

Examples:

fib(1) // === 0
fib(2) // === 1
fib(3) // === 1
fib(4) // === 2
fib(5) // === 3
-}
-----------------------------------------------------------------------------------------------------------


fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

fib :: Int -> Integer
fib n = fibs!!(n-1)


-- Exercise 10.13
-----------------------------------------------------------------------------------------------------------
{- 
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
The sum of these multiples is 23.

Finish the solution so that it returns the sum of all the multiples of 3 or 5 below the number passed in. 
Additionally, if the number is negative, return 0 (for languages that do have them).

Note: If the number is a multiple of both 3 and 5, only count it once.
-}
-----------------------------------------------------------------------------------------------------------


solution :: Integer -> Integer
solution number = sum [n | n <- [1..number - 1], n `mod` 3 == 0 || n `mod` 5 == 0]


-- Exercise 10.14
-----------------------------------------------------------------------------------------------------------
{- 
Given an array/list [] of n integers , find maximum triplet sum in the array Without duplications .

Notes :
Array/list size is at least 3 .

Array/list numbers could be a mixture of positives , negatives and zeros .

Repetition of numbers in the array/list could occur , So (duplications are not included when summing).

Input >> Output Examples
1- maxTriSum ({3,2,6,8,2,3}) ==> return (17)
Explanation:
As the triplet that maximize the sum {6,8,3} in order , their sum is (17)

Note : duplications are not included when summing , (i.e) the numbers added only once .

2- maxTriSum ({2,1,8,0,6,4,8,6,2,4}) ==> return (18)
Explanation:
As the triplet that maximize the sum {8, 6, 4} in order , their sum is (18) ,

Note : duplications are not included when summing , (i.e) the numbers added only once .

3- maxTriSum ({ -7,12,-7,29,-5,0,-7,0,0,29}) ==> return (41)
Explanation:
As the triplet that maximize the sum {12 , 29 , 0} in order , their sum is (41) ,

Note : duplications are not included when summing , (i.e) the numbers added only once .
-}
-----------------------------------------------------------------------------------------------------------


import Data.List

maxTriSum :: [Integer] -> Integer
maxTriSum = sum . take 3 . sortBy (flip compare) . nub


-- Exercise 10.15
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that will return the count of distinct case-insensitive alphabetic characters 
and numeric digits that occur more than once in the input string. 
The input string can be assumed to contain only alphabets (both uppercase and lowercase) and numeric digits.

Example
"abcde" -> 0 # no characters repeats more than once
"aabbcde" -> 2 # 'a' and 'b'
"aabBcde" -> 2 # 'a' occurs twice and 'b' twice (`b` and `B`)
"indivisibility" -> 1 # 'i' occurs six times
"Indivisibilities" -> 2 # 'i' occurs seven times and 's' occurs twice
"aA11" -> 2 # 'a' and '1'
"ABBA" -> 2 # 'A' and 'B' each occur twice
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char
import Data.List

duplicateCount :: String -> Int
duplicateCount = length . filter ((> 1) . length) . group . sort . map toLower


-- Exercise 10.16
-----------------------------------------------------------------------------------------------------------
{- 
A rectangle is can be defined by two factors: height and width.

Its area is defined as the multiplication of the two: height * width.

Its perimeter is the sum of its four edges: height + height + width + width.

It is possible to create rectangles of the same area but different perimeters. 
For example, given an area of 45, the possible heights, widths and resultant perimeters would be:

(1, 45) = 92

(9, 5) = 28

(15, 3) = 36

Note that (6, 7.5) has an area of 45 too, but is discarded in this kata because its width is non integral.

The task is to write a function that, given an area as a positive integer, 
returns the smallest perimeter possible of a rectangle with integral side lengths.

Input range:
1 <= area <= 5 x 10 ^ 10
-}
-----------------------------------------------------------------------------------------------------------


perimeters :: Integer -> [Integer]
perimeters area = [2 * i + 2 * (div area i) | i <- [1 .. rootArea], mod area i == 0]
  where
    rootArea = floor $ sqrt $ fromIntegral area

minimumPerimeter :: Integer -> Integer
minimumPerimeter = minimum . perimeters


-- Exercise 10.17
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that returns the smallest distance between two factors of a number. 
The input will always be a number greater than one.

Example:

13013 has factors: [ 1, 7, 11, 13, 77, 91, 143, 169, 1001, 1183, 1859, 13013]

Hence the asnwer will be 2 (=13-11)
-}
-----------------------------------------------------------------------------------------------------------


minDistance :: Integer -> Integer
minDistance n = minimum $ zipWith (-) (tail ds) ds
  where ds = [d | d <- [1..n], n `mod` d == 0]


-- Exercise 10.18
-----------------------------------------------------------------------------------------------------------
{- 
Write a function partlist that gives all the ways to divide a list (an array) 
of at least two elements into two non-empty parts.

Each two non empty parts will be in a pair 
(or an array for languages without tuples or a structin C - C: see Examples test Cases - )
Each part will be in a string
Elements of a pair must be in the same order as in the original array.

Examples of returns:
a = ["az", "toto", "picaro", "zone", "kiwi"] -->
[["az", "toto picaro zone kiwi"], ["az toto", "picaro zone kiwi"], 
["az toto picaro", "zone kiwi"], ["az toto picaro zone", "kiwi"]]
-}
-----------------------------------------------------------------------------------------------------------


partlist :: [String] -> [(String, String)]
partlist arr = [(unwords (take n arr), unwords (drop n arr)) | n <- [1..(length arr) - 1]]


-- Exercise 10.19
-----------------------------------------------------------------------------------------------------------
{- 
Write a function toWeirdCase (weirdcase in Ruby) that accepts a string, 
and returns the same string with all even indexed characters in each word upper cased, 
and all odd indexed characters in each word lower cased. 
The indexing just explained is zero based, so the zero-ith index is even, 
therefore that character should be upper cased and you need to start over for each word.

The passed in string will only consist of alphabetical characters and spaces(' '). 
Spaces will only be present if there are multiple words. Words will be separated by a single space(' ').

Examples:
toWeirdCase "String"            `shouldBe` "StRiNg"
toWeirdCase "Weird string case" `shouldBe` "WeIrD StRiNg CaSe"
-}
-----------------------------------------------------------------------------------------------------------


import Data.Char(toLower, toUpper)

toWeirdCase :: String -> String
toWeirdCase = unwords . map (zipWith ($) (cycle [toUpper, toLower])) . words


-- Exercise 10.20
-----------------------------------------------------------------------------------------------------------
{- 
Alice and Bob were on a holiday. 
Both of them took many pictures of the places they've been, 
and now they want to show Charlie their entire collection. 
However, Charlie doesn't like these sessions, since the motif usually repeats. 
He isn't fond of seeing the Eiffel tower 40 times.
He tells them that he will only sit for the session if they show the same motif at most N times. 
Luckily, Alice and Bob are able to encode the motif as a number. 
Can you help them to remove numbers such that their list contains each number only up to N times, without changing the order?

Task
Given a list and a number, create a new list that contains each number of list at most N times, without reordering.
For example if the input number is 2, and the input list is [1,2,3,1,2,1,2,3], 
you take [1,2,3,1,2], drop the next [1,2] since this would lead to 1 and 2 being in the result 3 times, 
and then take 3, which leads to [1,2,3,1,2,3].
With list [20,37,20,21] and number 1, the result would be [20,37,21].
-}
-----------------------------------------------------------------------------------------------------------


deleteNth :: [Int] -> Int -> [Int]
deleteNth lst n = foldl f [] lst
  where f acc x = if length (filter (==x) acc) >= n then acc else acc ++ [x]
