module Exercise09 where


-- Exercise 9.1
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


-- Exercise 9.2
-----------------------------------------------------------------------------------------------------------
{- 
If　a = 1, b = 2, c = 3 ... z = 26

Then l + o + v + e = 54

and f + r + i + e + n + d + s + h + i + p = 108

So friendship is twice stronger than love :-)

The input will always be in lowercase and never be empty.
-}
-----------------------------------------------------------------------------------------------------------

wordsToMarks :: String -> Int
wordsToMarks = sum . map ((+ (1 - fromEnum 'a')) . fromEnum)


-- Exercise 9.3
-----------------------------------------------------------------------------------------------------------
{-
Write a function that takes an integer as input, 
and returns the number of bits that are equal to one in the binary representation of that number. 
You can guarantee that input is non-negative.

Example: The binary representation of 1234 is 10011010010, so the function should return 5 in this case
-}
-----------------------------------------------------------------------------------------------------------


countBits :: Int -> Int
countBits 0 = 0
countBits n = n`mod`2 + countBits (n`div`2)


-- Exercise 9.4
-----------------------------------------------------------------------------------------------------------
{- 
Given a random non-negative number, 
you have to return the digits of this number within an array in reverse order.

Example(Input => Output):
348597 => [7,9,5,8,4,3]
0 => [0]
-}
-----------------------------------------------------------------------------------------------------------

digitize :: Int -> [Int]
digitize s = reverse [read [x] | x <- show s]


-- Exercise 9.5
-----------------------------------------------------------------------------------------------------------
{- 
You're saying good-bye your best friend , See you next happy year .

Happy Year is the year with only distinct digits , (e.g) 2018

Task
Given a year, Find The next happy year or The closest year You'll see your best friend!alt!alt

Notes
Year Of Course always Positive .
Have no fear , It is guaranteed that the answer exists .
It's not necessary that the year passed to the function is Happy one .
Input Year with in range (1000 ≤ y ≤ 9000)
Input >> Output Examples:
nextHappyYear (7712) ==> return (7801)
Explanation:
As the Next closest year with only distinct digits is 7801 .

nextHappyYear (8989) ==> return (9012)
Explanation:
As the Next closest year with only distinct digits is 9012 .

nextHappyYear (1001) ==> return (1023)
Explanation:
As the Next closest year with only distinct digits is 1023 .
-}
-----------------------------------------------------------------------------------------------------------


nextHappyYear :: Int -> Int
nextHappyYear n
        | test (x:xs)     = n+1
        | otherwise       = nextHappyYear (n+1)
        where (x:xs)      = show (n+1)
              test []     = True
              test (x:xs) = notElem x xs && test xs



-- Exercise 9.6
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


-- Exercise 9.7
-----------------------------------------------------------------------------------------------------------
{-
Given an array/list [] of integers , Find the product of the k maximal numbers

Notes
Array/list size is at least 3 .

Array/list's numbers Will be mixture of positives , negatives and zeros

Repetition of numbers in the array/list could occur.

Input >> Output Examples

maxProduct ({4, 3, 5}, 2) ==>  return (20)
Explanation:
Since the size (k) equal 2 , then the subsequence of size 2 whose gives product of maxima is 5 * 4 = 20 .

maxProduct ({8, 10 , 9, 7}, 3) ==>  return (720)
Explanation:
Since the size (k) equal 3 , then the subsequence of size 3 whose gives product of maxima is  8 * 9 * 10 = 720 .

maxProduct ({10, 8, 3, 2, 1, 4, 10}, 5) ==> return (9600)
Explanation:
Since the size (k) equal 5 , then the subsequence of size 5 whose gives product of maxima is  10 * 10 * 8 * 4 * 3 = 9600 .

maxProduct ({ -4, -27, -15, -6, -1}, 2) ==> return (4)
Explanation:
Since the size (k) equal 2 , then the subsequence of size 2 whose gives product of maxima is  -4 * -1 = 4 .

maxProduct ({10, 3, -1, -27} , 3)  return (-30)
Explanation:
Since the size (k) equal 3 , then the subsequence of size 3 whose gives product of maxima is 10 * 3 * -1 = -30 .
-}
-----------------------------------------------------------------------------------------------------------

maxProduct :: [Integer] -> Int -> Integer
maxProduct list x = product (take x (reverse (quickSort list)))
 where
  quickSort :: (Ord a) => [a] -> [a]
  quickSort [] = []
  quickSort (x:xs) = quickSort a ++ [x] ++ quickSort b 
    where
     a = filter (<= x) xs 
     b = filter (> x) xs


-- Exercise 9.8
-----------------------------------------------------------------------------------------------------------
{- 
Given a list of digits, return the smallest number that could be formed from these digits, 
using the digits only once (ignore duplicates).

Notes:
Only positive integers will be passed to the function (> 0 ), no negatives or zeros.
Input >> Output Examples
minValue ({1, 3, 1})  ==> return (13)
Explanation:
(13) is the minimum number could be formed from {1, 3, 1} , Without duplications

minValue({5, 7, 5, 9, 7})  ==> return (579)
Explanation:
(579) is the minimum number could be formed from {5, 7, 5, 9, 7} , Without duplications

minValue({1, 9, 3, 1, 7, 4, 6, 6, 7}) return  ==> (134679)
Explanation:
(134679) is the minimum number could be formed from {1, 9, 3, 1, 7, 4, 6, 6, 7} , Without duplications
-}
-----------------------------------------------------------------------------------------------------------

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b >= x]

clean [] = []
clean (x:xs) = x: clean [y | y<- xs, y/=x]

getmin [] = 0
getmin (x:xs) = x*10^(length xs) + getmin xs

minValue :: [Int] -> Int
minValue xs = getmin( clean (qsort xs))



-- Exercise 9.9
-----------------------------------------------------------------------------------------------------------
{- 
You might know some pretty large perfect squares. But what about the NEXT one?

Complete the findNextSquare method that finds the next integral perfect square after the one passed as a parameter. 
Recall that an integral perfect square is an integer n such that sqrt(n) is also an integer.

If the parameter is itself not a perfect square then -1 should be returned. 
You may assume the parameter is non-negative.

Examples:(Input --> Output)

121 --> 144
625 --> 676
114 --> -1 since 114 is not a perfect square
-}
-----------------------------------------------------------------------------------------------------------

findNextSquare :: Integer -> Integer
findNextSquare x = if isSquare x then nextSquare else -1
  where
    nextSquare = (isqrt x + 1) ^ 2

isSquare :: Integer -> Bool
isSquare x = r * r == x
  where
    r = isqrt x

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromInteger


-- Exercise 9.10
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that generates factors for a given number.

The function takes an integer on the standard input and returns a list of integers 
(ObjC: array of NSNumbers representing integers). 
That list contains the prime factors in numerical sequence.

Examples
1  ==>  []
3  ==>  [3]
8  ==>  [2, 2, 2]
9  ==>  [3, 3]
12 ==>  [2, 2, 3]
-}
-----------------------------------------------------------------------------------------------------------


primeFactors :: Integer -> [Integer]
primeFactors 0 = []
primeFactors 1 = []
primeFactors n = first : primeFactors (div n first)
  where
    first = head $ [ x | x <- [2..n], mod n x == 0]


-- Exercise 9.11
-----------------------------------------------------------------------------------------------------------
{- 
Define a function that takes an integer argument and returns a logical value true or false depending on if the integer is a prime.

Per Wikipedia, a prime number ( or a prime ) is a natural number greater than 1 that has no positive divisors other than 1 and itself.

Requirements
You can assume you will be given an integer input.
You can not assume that the integer will be only positive. You may be given negative numbers as well ( or 0 ).
NOTE on performance: There are no fancy optimizations required, but still the most trivial solutions might time out. 
Numbers go up to 2^31 ( or similar, depending on language ). Looping all the way up to n, or n/2, will be too slow.
Example
is_prime(1)  /* false */
is_prime(2)  /* true  */
is_prime(-1) /* false */
-}
-----------------------------------------------------------------------------------------------------------


isPrime :: Integer -> Bool
isPrime x | x <= 1 = False
isPrime x = null [d | d <- [2..iSqrt x], x `mod` d == 0]
  where iSqrt = floor . sqrt . fromInteger


-- Exercise 9.12
-----------------------------------------------------------------------------------------------------------
{- 
Well met with Fibonacci bigger brother, AKA Tribonacci.

As the name may already reveal, 
it works basically like a Fibonacci, 
but summing the last 3 (instead of 2) numbers of the sequence to generate the next. 
And, worse part of it, regrettably I won't get to hear non-native Italian speakers trying to pronounce it :(

So, if we are to start our Tribonacci sequence with [1, 1, 1] as a starting input (AKA signature), 
we have this sequence:

[1, 1 ,1, 3, 5, 9, 17, 31, ...]
But what if we started with [0, 0, 1] as a signature? 
As starting with [0, 1] instead of [1, 1] basically shifts the common Fibonacci sequence by once place, 
you may be tempted to think that we would get the same sequence shifted by 2 places, 
but that is not the case and we would get:

[0, 0, 1, 1, 2, 4, 7, 13, 24, ...]
Well, you may have guessed it by now, 
but to be clear: you need to create a fibonacci function that given a signature array/list, 
returns the first n elements - signature included of the so seeded sequence.

Signature will always contain 3 numbers; 
n will always be a non-negative number; if n == 0, 
then return an empty array (except in C return NULL) and be ready for anything else which is not clearly specified ;)
-}
-----------------------------------------------------------------------------------------------------------


tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci _ n | n < 1 = []
tribonacci (a, b, c) n = a : tribonacci (b, c, a+b+c) (n-1)


-- Exercise 9.13
-----------------------------------------------------------------------------------------------------------
{- 
Return the most profit from stock quotes.

Stock quotes are stored in an array in order of date. 
The stock profit is the difference in prices in buying and selling stock. 
Each day, you can either buy one unit of stock, sell any number of stock units you have already bought, or do nothing. 
Therefore, the most profit is the maximum difference of all pairs in a sequence of stock prices.

Example:

 [ 1, 2, 3, 4, 5, 6 ]        => 15  (buy at 1,2,3,4,5 and then sell all at 6)
 [ 6, 5, 4, 3, 2, 1 ]        => 0   (nothing to buy for profit)
 [ 1, 6, 5, 10, 8, 7 ]       => 18  (buy at 1,6,5 and sell all at 10)
 [ 1, 2, 10, 3, 2, 7, 3, 2 ] => 26  (buy at 1,2 and sell them at 10. Then buy at 3,2 and sell them at 7)
-}
-----------------------------------------------------------------------------------------------------------


profit :: [Int] -> Int
profit xs = snd (acc xs) 
  where acc :: [Int] -> (Int, Int)
        acc [] = (0, 0)
        acc (x:xs) 
          | high > x = (high, total + high - x)
          | otherwise = (x, total)
            where (high, total) = acc xs


-- Exercise 9.14
-----------------------------------------------------------------------------------------------------------
{- 
Count the number of divisors of a positive integer n.

Random tests go up to n = 500000.

Examples (input --> output)
4 --> 3 (1, 2, 4)
5 --> 2 (1, 5)
12 --> 6 (1, 2, 3, 4, 6, 12)
30 --> 8 (1, 2, 3, 5, 6, 10, 15, 30)
-}
-----------------------------------------------------------------------------------------------------------


divisors :: Integral a => a -> Int
divisors x = length [k | k <- [1..x], x `mod` k == 0]


-- Exercise 9.15
-----------------------------------------------------------------------------------------------------------
{- 
Given the triangle of consecutive odd numbers:

             1
          3     5
       7     9    11
   13    15    17    19
21    23    25    27    29
...
Calculate the sum of the numbers in the nth row of this triangle (starting at index 1) e.g.: (Input --> Output)

1 -->  1
2 --> 3 + 5 = 8
-}
-----------------------------------------------------------------------------------------------------------


{-
Consider a triangle of consecutive indices into the odd numbers
           1
         2   3
       4   5   6
     7   8   9  10
  11  12  13  14  15

Observe the series of first numbers and their deltas:
  1    2    4    7    11
    +1   +2   +3   +4

Remember Gauss:
gauss n = sum [1 .. n]
        = (n^2 + n) / 2
Deduce:
gauss (a + b) = ((a + b)^2 + (a + b)) / 2
              = (a^2 + 2*a*b + b^2 + a + b) / 2
              = (2*(gauss a) + 2*(gauss b) + 2*a*b) / 2
              = gauss a + gauss b + a * b

Now simplify:
rowSumOddNumbers n = sum [odd i | i <- [start n .. start n + n - 1]]
  where start n = sum [1 .. n - 1] + 1
        odd i = 2 * i - 1

rowSumOddNumbers n = sum [2 * i - 1 | i <- [gauss (n - 1) + 1 .. gauss (n - 1) + n]]
                   = 2 * (sum [i | i <- [gauss (n - 1) + 1 .. gauss (n - 1) + n]]) - sum [1 | i <- [gauss (n - 1) + 1 .. gauss (n - 1) + n]]
                   = 2 * (gauss (gauss (n - 1) + n) - gauss (gauss (n - 1))) - (gauss (n - 1) + n - gauss (n - 1))
                   = 2 * (gauss (gauss (n - 1)) + gauss n + gauss (n - 1) * n - gauss (gauss n - 1))) - n
                   = 2 * (gauss n + n * gauss (n - 1)) - n
                   = 2 * ((n^2 + n) / 2 + n * ((n - 1)^2 + n - 1) / 2) - n
                   = 2 * ((n^2 + n) / 2 + n * (n^2 - 2*n + 1 + n - 1) / 2) - n
                   = 2 * (n^2/2 + n/2 + n^3/2 - n^2/2) - n
                   = 2 * (n/2 + n^3/2) - n
                   = n + n^3 - n
                   = n^3
-}
rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers = (^ 3)


-- Exercise 9.16
-----------------------------------------------------------------------------------------------------------
{- 
Yet another staple for the functional programmer. 
You have a sequence of values and some predicate for those values. 
You want to remove the longest prefix of elements such that the predicate is true for each element. 
We'll call this the dropWhile function. It accepts two arguments. 
The first is the sequence of values, and the second is the predicate function. 
The function does not change the value of the original sequence.

dropWhile [2,4,6,8,1,2,5,4,3,2] even -- -> [1,2,5,4,3,2]
Your task is to implement the dropWhile function. 
If you've got a span function lying around, this is a one-liner! Alternatively, 
if you have a takeWhile function on your hands, then combined with the dropWhile function, 
you can implement the span function in one line. This is the beauty of functional programming: 
there are a whole host of useful functions, many of which can be implemented in terms of each other.
-}
-----------------------------------------------------------------------------------------------------------


import Prelude hiding (dropWhile, span, break)

dropWhile :: [a] -> (a -> Bool) -> [a]
dropWhile xs p = drop (length $ takeWhile p xs) xs


-- Exercise 9.17
-----------------------------------------------------------------------------------------------------------
{- 
you get the start number and the end number of a region 
and should return the count of all numbers except numbers with a 5 in it. 
The start and the end number are both inclusive!

Examples:

1,9 -> 1,2,3,4,6,7,8,9 -> Result 8
4,17 -> 4,6,7,8,9,10,11,12,13,14,16,17 -> Result 12
The result may contain fives. ;-)
The start number will always be smaller than the end number. Both numbers can be also negative!
-}
-----------------------------------------------------------------------------------------------------------


dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = length $ filter (not . elem '5' . show) [start..end]


-- Exercise 9.18
-----------------------------------------------------------------------------------------------------------
{- 
Haskell has some useful functions for dealing with lists:

$ ghci
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
λ head [1,2,3,4,5]
1
λ tail [1,2,3,4,5]
[2,3,4,5]
λ init [1,2,3,4,5]
[1,2,3,4]
λ last [1,2,3,4,5]
5
Your job is to implement these functions in your given language. 
Make sure it doesn't edit the array; that would cause problems! Here's a cheat sheet:

| HEAD | <----------- TAIL ------------> |
[  1,  2,  3,  4,  5,  6,  7,  8,  9,  10]
| <----------- INIT ------------> | LAST |

head [x] = x
tail [x] = []
init [x] = []
last [x] = x
Here's how I expect the functions to be called in your language:

head [1,2,3,4,5] => 1
tail [1,2,3,4,5] => [2,3,4,5]
-}
-----------------------------------------------------------------------------------------------------------


import Prelude hiding (head, tail, init, last)

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

init :: [a] -> [a]
init [x] = []
init (x:xs) = x : init xs

last :: [a] -> a
last [x] = x
last (_:xs) = last xs


-- Exercise 9.19
-----------------------------------------------------------------------------------------------------------
{- 
Given two numbers and an arithmetic operator (the name of it, as a string), 
return the result of the two numbers having that operator used on them.

a and b will both be positive integers, and a will always be the first number in the operation, 
and b always the second.

The four operators are "add", "subtract", "divide", "multiply".

In Haskell:

The operation is defined as
data Operation = Add | Divide | Multiply | Subtract deriving (Eq, Show, Enum, Bounded)
The arithmetic function as
arithmetic :: Double -> Double -> Operation -> Double
arithmetic :: Fractional a => a -> a -> Operation -> a
Try to do it without using if statements!
-}
-----------------------------------------------------------------------------------------------------------

data Operation = Add | Divide | Multiply | Subtract deriving (Eq, Show, Enum, Bounded)

arithmetic :: Fractional a => a -> a -> Operation -> a
arithmetic a b Add = a + b
arithmetic a b Subtract = a - b
arithmetic a b Multiply = a * b
arithmetic a b Divide = a / b


-- Exercise 9.20
-----------------------------------------------------------------------------------------------------------
{- 
In this little assignment you are given a string of space separated numbers, 
and have to return the highest and lowest number.

Examples
highAndLow "1 2 3 4 5")  # return "5 1"
highAndLow "1 2 -3 4 5") # return "5 -3"
highAndLow "1 9 3 4 -5") # return "9 -5"
Notes
All numbers are valid Int32, no need to validate them.
There will always be at least one number in the input string.
Output string must be two numbers separated by a single space, and highest number is first.
-}
-----------------------------------------------------------------------------------------------------------

highAndLow :: String -> String
highAndLow xs = show (maximum ns) ++ " " ++ show (minimum ns)
  where ns = (map read $ words xs) :: [Int]
