-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 22/23 Oct.

import Data.Char
import Test.QuickCheck
import Data.List



-- 1. Map
-- a.
uppers :: String -> String
uppers s = map toUpper s

-- b.
doubles :: [Int] -> [Int]
doubles xs = map (* 2) xs

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (/ 100) (map fromIntegral xs)

-- d.
uppers' :: String -> String
uppers' s = [toUpper c | c <- s]

prop_uppers :: String -> Bool
prop_uppers s = (uppers s) == (uppers' s)



-- 2. Filter
-- a.
alphas :: String -> String
alphas s = filter isAlpha s

-- b.
rmChar ::  Char -> String -> String
rmChar c s = filter (/= c) s

-- c.
above :: Int -> [Int] -> [Int]
above limit xs = filter (> limit) xs

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
-- unequals pairs = filter (\(x, y) -> x /= y) pairs
unequals pairs = filter unequalPair pairs
                 where unequalPair (x, y) = x /= y
-- e.
rmCharComp :: Char -> String -> String
rmCharComp c s = [x | x <- s, x /= c]

prop_rmChar :: Char -> String -> Bool
prop_rmChar c s = (rmChar c s) == (rmCharComp c s)



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' s = map toUpper (filter isAlpha s)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (* 2) (filter (> 3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter (\s -> even (length s)) strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1 xs

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs)
    | x == False = False
    | otherwise = andRec xs

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] s2 = s2
rmCharsRec (x:xs) s2 = rmCharsRec xs (rmChar x s2)

rmCharsFold :: String -> String -> String
-- rmCharsFold s1 s2 = foldr (checkAndTake) [] s2
--                    where checkAndTake c
--                              | c `elem` s1 = ([] ++)
--                              | otherwise = ([c] ++)
rmCharsFold s1 s2 = foldr (rmChar) s2 s1

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform [] = True
uniform v = all (\x -> (x == (v !! 0))) v

-- b.
valid :: Matrix -> Bool
valid a = (uniform lenRows) && ((length a) > 0) && (length (a !! 0) > 0)
    where lenRows = [length r | r <- a]

-- 6.

-- b.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [f x y | (x, y) <- zip xs ys]

-- c.
myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith' f xs ys = map(uncurry f) (zip xs ys)

-- 7.

plusRow :: [Int] -> [Int] -> [Int]
plusRow a b = zipWith (+) a b

plusM :: Matrix -> Matrix -> Matrix
plusM a b
    | not (valid a) || not (valid b) || rowsA /= rowsB || columnsA /= columnsB = error "Invalid input"
    | otherwise = zipWith plusRow a b
    where rowsA = length a
          columnsA = if((length a) > 0) then length (a !! 0) else -1
          rowsB = length b
          columnsB = if((length b) > 0) then length (b !! 0) else -2

-- 8.

multRowColumn :: Matrix -> Matrix -> Int -> Int -> Int
multRowColumn a b r c = sum(zipWith (*) row column)
                      where row = (a !! r)
                            column = ((transpose b) !! c)

timesM :: Matrix -> Matrix -> Matrix
timesM a b
    | columnsA /= rowsB = error "Invalid input"
    | otherwise = [[multRowColumn a b r c | c <- [0..columnsB - 1]] | r <- [0..rowsA - 1]]
    where rowsA = length a
          columnsA = if((length a) > 0) then length (a !! 0) else -1
          rowsB = length b
          columnsB = if((length b) > 0) then length (b !! 0) else -2
 
-- Optional material
-- 9.

-- written in optional.hs
