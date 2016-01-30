-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (8/9 Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
    | x `mod` 2 == 0 = [x `div` 2] ++ (halveEvensRec xs)
    | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = (halveEvens xs) == (halveEvensRec xs)


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x && x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs)
    | lo <= x && x <= hi = [x] ++ (inRangeRec lo hi xs)
    | otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = (inRange lo hi xs) == (inRangeRec lo hi xs)



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length([x | x <- xs, x > 0])

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
    | x > 0 = 1 + (countPositivesRec xs)
    | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = (countPositives xs) == (countPositivesRec xs)

-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round((fromIntegral x) - (fromIntegral x) * 0.1)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher prices = sum([discount price | price <- prices, discount price <= 19900])

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (price:otherPrices)
    | discount price <= 19900 = discount price + pennypincherRec otherPrices
    | otherwise = pennypincherRec otherPrices

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher prices = (pennypincher prices) == (pennypincherRec prices)

-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits s = product([digitToInt x | x <- s, isDigit x == True])

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec[] = 1
multDigitsRec (x:xs)
    | isDigit x = digitToInt x * multDigitsRec xs
    | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits s = (multDigits s) == (multDigitsRec s)

-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (x:[]) = [toUpper x]
capitalise (x:xs) = [toUpper x] ++ [toLower c | c <- xs]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec [] = []
capitaliseRec (x:[]) = [toUpper x]
capitaliseRec s@(x:xs) = capitaliseRec (init s) ++ [toLower (last s)]

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise s = (capitalise s) == (capitaliseRec s)
                                                                                                                                     
-- 7. title

-- List-comprehension version

adjustWord :: String -> String
adjustWord s
    | (length s) >= 4 = capitalise s
    | otherwise = [toLower c | c <- s]

title :: [String] -> [String]
title [] = []
title ([word]) = [capitalise word]
title (firstWord:otherWords) = [capitalise firstWord] ++ [adjustWord word | word <- otherWords]

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec ([word]) = [capitalise word]
titleRec words@(firstWord:otherWords) = titleRec(init words) ++ [adjustWord(last words)]

-- mutual test
prop_title :: [String] -> Bool
prop_title words = (title words) == (titleRec words)

-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter inPosition len words = [word | word <- words, inPosition >= 0 && inPosition < length word && word !! inPosition == letter && length word == len]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec letter inPosition len []  = []
crosswordFindRec letter inPosition len (word:otherWords)
    | inPosition >= 0 && inPosition < length word && word !! inPosition == letter && length word == len = [word] ++ crosswordFindRec letter inPosition len otherWords
    | otherwise = crosswordFindRec letter inPosition len otherWords

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind letter inPosition len words = ((crosswordFind letter inPosition len words) == (crosswordFindRec letter inPosition len words))

-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search word letter = [x | x <- [0..length word - 1], word !! x == letter]

-- Recursive version

getIndex :: String -> Int
getIndex [] = -1
getIndex (x:xs) = 1 + getIndex xs

searchRec :: String -> Char -> [Int]
searchRec [] letter = []
searchRec word@(firstLetter:otherLetters) letter
    | (last word) == letter = searchRec (init word) letter ++ [getIndex word]
    | otherwise = searchRec (init word) letter

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search word letter = (search word letter == searchRec word letter)

-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains text pattern = length [x | x <- [0..length text], isPrefixOf pattern (drop x text)] > 0

-- Recursive version
containsRec :: String -> String -> Bool
containsRec [] pattern
    | pattern == [] = True
    | otherwise = False
containsRec text@(firstLetter:otherLetters) pattern
    | isPrefixOf pattern text = True
    | otherwise = containsRec otherLetters pattern

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains text pattern = (contains text pattern == containsRec text pattern)
