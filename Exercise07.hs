module Exercise07 where


-- Exercise 7.1
-----------------------------------------------------------------------------------------------------------
{- 
Write a function that will check if two given characters are the same case.

If either of the characters is not a letter, return -1
If both characters are the same case, return 1
If both characters are letters, but not the same case, return 0
Examples
'a' and 'g' returns 1

'A' and 'C' returns 1

'b' and 'G' returns 0

'B' and 'g' returns 0

'0' and '?' returns -1 
-}
-----------------------------------------------------------------------------------------------------------

import Data.Char
sameCase :: Char -> Char -> Int
sameCase x y 
  | not (isLetter x) || not (isLetter y)  = -1
  | isUpper x == isUpper y = 1 
  | otherwise = 0


-- Exercise 7.2
-----------------------------------------------------------------------------------------------------------
-- Write a function revR which reverses a given list. Try not to cheat and provide a recursive solution.
-----------------------------------------------------------------------------------------------------------

revR :: [Int] -> [Int]
revR [] = []
revR (x:xs) = revR xs ++ [x]


-- Exercise 7.3
-----------------------------------------------------------------------------------------------------------
{-
Return the number (count) of vowels in the given string.

We will consider a, e, i, o, u as vowels for this Kata (but not y).

The input string will only consist of lower case letters and/or spaces.
-}
-----------------------------------------------------------------------------------------------------------


getCount :: String -> Int
getCount = length . filter (`elem` "aeiouAEIOU")


-- Exercise 7.4
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


-- Exercise 7.5
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


-- Exercise 7.6
-----------------------------------------------------------------------------------------------------------
{- Given an integer n and two other values, build an array of size n filled with these two values alternating.

Examples
5, true, false     -->  [true, false, true, false, true]
10, "blue", "red"  -->  ["blue", "red", "blue", "red", "blue", "red", "blue", "red", "blue", "red"]
0, "one", "two"    -->  [] 
-}
-----------------------------------------------------------------------------------------------------------

alternate :: Int -> a -> a -> [a]
alternate n x y = take n $ cycle [x,y]


-- Exercise 7.7
-----------------------------------------------------------------------------------------------------------
{- Create a function that takes a number as an argument and returns a grade based on that number.

Score Grade
Anything greater than 1 or less than 0.6  "F"
0.9 or greater  "A"
0.8 or greater  "B"
0.7 or greater  "C"
0.6 or greater  "D"
Examples:

grader(0)   should be "F"
grader(1.1) should be "F"
grader(0.9) should be "A"
grader(0.8) should be "B"
grader(0.7) should be "C"
grader(0.6) should be "D"
-}
-----------------------------------------------------------------------------------------------------------

grader :: Double -> Char
grader n
  | n > 1 || n < 0.6 = 'F'
  | n >= 0.9         = 'A'
  | n >= 0.8         = 'B'
  | n >= 0.7         = 'C'
  | otherwise        = 'D'

grader' :: Double -> Char
grader' n | n > 1 || n < 0 = 'F'
         | otherwise = "FFFFFFDCBAA" !! truncate (n * 10)


-- Exercise 7.8
-----------------------------------------------------------------------------------------------------------
{- You and a friend have decided to play a game to drill your statistical intuitions. The game works like this:

You have a bunch of red and blue marbles. 
To start the game you grab a handful of marbles of each color and put them into the bag, 
keeping track of how many of each color go in. You take turns reaching into the bag, guessing a color, 
and then pulling one marble out. You get a point if you guessed correctly. 
  The trick is you only have three seconds to make your guess, so you have to think quickly.

You've decided to write a function, guessBlue() to help automatically calculate whether you should guess "blue" or "red". 
The function should take four arguments:

the number of blue marbles you put in the bag to start
the number of red marbles you put in the bag to start
the number of blue marbles pulled out so far (always lower than the starting number of blue marbles)
the number of red marbles pulled out so far (always lower than the starting number of red marbles)
guessBlue() should return the probability of drawing a blue marble, expressed as a float. 

For example, guessBlue(5, 5, 2, 3) should return 0.6.
-}
-----------------------------------------------------------------------------------------------------------

guessBlue :: Int -> Int -> Int -> Int -> Double
guessBlue bin rin bout rout = (b) / (b+r)
  where b = fromIntegral (bin - bout)
        r = fromIntegral (rin - rout)


-- Exercise 7.9
-----------------------------------------------------------------------------------------------------------
{- Time to test your basic knowledge in functions! Return the odds from a list:

[1, 2, 3, 4, 5]  -->  [1, 3, 5]
[2, 4, 6]        -->  []
-}
-----------------------------------------------------------------------------------------------------------

odds :: [Int] -> [Int]
odds = filter odd

odds' :: [Int] -> [Int]
odds' l = [x | x <- l, mod x 2 == 1]


-- Exercise 7.10
-----------------------------------------------------------------------------------------------------------
{- Create a function that takes a string and an integer (n).

The function should return a string that repeats the input string n number of times.

If anything other than a string is passed in you should return "Not a string"

Example
"Hi", 2 --> "HiHi"
1234, 5 --> "Not a string"
-}
-----------------------------------------------------------------------------------------------------------

repeatIt :: String -> Int -> String
repeatIt str n = concat $ replicate n str


-- Exercise 7.11
-----------------------------------------------------------------------------------------------------------
{- This function takes two numbers as parameters, the first number being the coefficient, 
and the second number being the exponent.

Your function should multiply the two numbers, and then subtract 1 from the exponent. 
Then, it has to print out an expression (like 28x^7). "^1" should not be truncated when exponent = 2.

For example:
derive 7 8

In this case, the function should multiply 7 and 8, and then subtract 1 from 8. 
It should output "56x^7", the first number 56 being the product of the two numbers, 
and the second number being the exponent minus 1.

derive 7 8 == "56x^7"
derive 5 9 == "45x^8"
Notes:

The output of this function should be a string
The exponent will never be 1, and neither number will ever be 0
-}
-----------------------------------------------------------------------------------------------------------


derive :: Int -> Int -> String
derive x y = show(x * y) ++ "x^" ++ show(y - 1)


-- Exercise 7.12
-----------------------------------------------------------------------------------------------------------
{- You're laying out a rad pixel art mural to paint on your living room wall in homage to Paul Robertson.

You want your work to be perfect down to the millimeter. 
You haven't decided on the dimensions of your piece, how large you want your pixels to be, 
or which wall you want to use. You just know that you want to fit an exact number of pixels.

To help decide those things you've decided to write a function, is_divisible() 
that will tell you whether a wall of a certain length can exactly fit an integer number of pixels of a certain length.

Your function should take two arguments: the size of the wall in millimeters and the size of a pixel in millimeters. 
It should return True if you can fit an exact number of pixels on the wall, otherwise it should return False. 
For example is_divisible(4050, 27) should return True, but is_divisible(4066, 27) should return False.
-}
-----------------------------------------------------------------------------------------------------------


go :: Int -> Int -> Bool

go m n = mod m n == 0 


-- Exercise 7.13
-----------------------------------------------------------------------------------------------------------
{- Each number should be formatted that it is rounded to two decimal places. 
You don't need to check whether the input is a valid number because only valid numbers are used in the tests.

Example:    
5.5589 is rounded 5.56   
3.3424 is rounded 3.34
-}
-----------------------------------------------------------------------------------------------------------


twoDecimalPlaces :: Double -> Double
twoDecimalPlaces = (/100). fromIntegral . round . (*100)


-- Exercise 7.14
-----------------------------------------------------------------------------------------------------------
{- Create a method sayHello/say_hello/SayHello that takes as input a name, city, and state to welcome a person. 
Note that name will be an array consisting of one or more values that should be joined together with one space between each, 
and the length of the name array in test cases will vary.

Example:

sayHello(['John', 'Smith'], 'Phoenix', 'Arizona')
This example will return the string Hello, John Smith! Welcome to Phoenix, Arizona!
-}
-----------------------------------------------------------------------------------------------------------

sayhello :: [String] -> String -> String -> String
sayhello names city state = "Hello, " ++ unwords names ++ "! Welcome to " ++ city ++ ", " ++ state ++ "!"


-- Exercise 7.15
-----------------------------------------------------------------------------------------------------------
{- Create a combat function that takes the player's current health and the amount of damage recieved, 
and returns the player's new health. Health can't be less than 0.
-}
-----------------------------------------------------------------------------------------------------------

updateHealth :: Double -> Double -> Double
updateHealth health damage = max 0 (health - damage)


-- Exercise 7.16
-----------------------------------------------------------------------------------------------------------
{- Replace all vowel to exclamation mark in the sentence. aeiouAEIOU is vowel.

Examples
replace("Hi!") === "H!!"
replace("!Hi! Hi!") === "!H!! H!!"
replace("aeiou") === "!!!!!"
replace("ABCDE") === "!BCD!"
-}
-----------------------------------------------------------------------------------------------------------


replace :: String -> String
replace = map (\x -> if x `elem` "aeiouAEIOU" then '!' else x)


-- Exercise 7.17
-----------------------------------------------------------------------------------------------------------
{- Numbers ending with zeros are boring.

They might be fun in your world, but not here.

Get rid of them. Only the ending ones.

1450 -> 145
960000 -> 96
1050 -> 105
-1050 -> -105
Zero alone is fine, don't worry about it. Poor guy anyway
-}
-----------------------------------------------------------------------------------------------------------

noBoringZeros :: Int -> Int
noBoringZeros n
    | n == 0          = 0
    | n `mod` 10 == 0 = noBoringZeros (n `div` 10)
    | otherwise       = n


-- Exercise 7.18
-----------------------------------------------------------------------------------------------------------
{- Christmas is coming and many people dreamed of having a ride with Santa's sleigh. 
But, of course, only Santa himself is allowed to use this wonderful transportation. 
And in order to make sure, that only he can board the sleigh, there's an authentication mechanism.

Your task is to implement the authenticate() method of the sleigh, which takes the name of the person, 
who wants to board the sleigh and a secret password. 
If, and only if, the name equals "Santa Claus" and the password is "Ho Ho Ho!" 
(yes, even Santa has a secret password with uppercase and lowercase letters and special characters :D), 
the return value must be true. Otherwise it should return false.

Examples:

authenticate "Santa Claus" "Ho Ho Ho!" -- True
authenticate "Santa"       "Ho Ho Ho!" -- False
authenticate "Santa Claus" "Ho Ho!"    -- False
-}
-----------------------------------------------------------------------------------------------------------

authenticate :: String -> String -> Bool
authenticate "Santa Claus" "Ho Ho Ho!" = True
authenticate _             _           = False


authenticate' :: String -> String -> Bool
authenticate' someName somePassword = someName == "Santa Claus" && somePassword == "Ho Ho Ho!"


-- Exercise 7.19
-----------------------------------------------------------------------------------------------------------
{- Create a function called _if which takes 3 arguments: 
a boolean value bool and 2 functions (which do not take any parameters): func1 and func2

When bool is truth-ish, func1 should be called, otherwise call the func2.

Example:
main = _if True (putStrLn "You spoke the truth") (putStrLn "liar")
-- puts "You spoke the truth" to the console.

_if False "Hello" "Goodbye" -- "Goodbye"
-}
-----------------------------------------------------------------------------------------------------------

_if :: Bool -> a -> a -> a
_if True x _ = x
_if False _ y = y


_if' :: Bool -> a -> a -> a
_if' b x y = if b then x else y


-- Exercise 7.20
-----------------------------------------------------------------------------------------------------------
{- Define a function that removes duplicates from an array of numbers and returns it as a result.

The order of the sequence has to stay the same.
-}
-----------------------------------------------------------------------------------------------------------

distinct :: [Int] -> [Int]
distinct [] = []
distinct (x:xs) = x : distinct (filter (/=x) xs)