-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 15/16 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n xs
    | n < 0 || n > length xs = error "the first parameter is either negative or too large"
    | otherwise = (drop n xs) ++ (take n xs)

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey n = zip alphabet (rotate n alphabet)
    where alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c pairs = head findPos
                 where findPos = [y | (x, y) <- pairs, x == c] ++ [c] 

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec c [] = c
lookUpRec c (x:xs)
    | fst x == c = snd x
    | otherwise = lookUpRec c xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c pairs = (lookUp c pairs) == (lookUpRec c pairs)

-- 5.
encipher :: Int -> Char -> Char
encipher n c = lookUp c (makeKey n)

-- 6.
normalize :: String -> String
normalize [] = []
normalize (firstLetter:otherLetters)
    | firstLetter `elem` ['A'..'Z'] || firstLetter `elem` ['0'..'9'] = (firstLetter) : (normalize otherLetters)
    | firstLetter `elem` ['a'..'z'] = (toUpper firstLetter) : (normalize otherLetters)
    | otherwise = normalize otherLetters

-- 7.
encipherStr :: Int -> String -> String
encipherStr n s = [encipher n c | c <- normalized_s]
                      where normalized_s = normalize s

-- 8.
swapPair :: (a, b) -> (b, a)
swapPair (x, y) = (y, x)

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey pairs = [swapPair pair | pair <- pairs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (firstPair:otherPairs) = (swapPair firstPair) : reverseKeyRec otherPairs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey pairs = (reverseKey pairs) == (reverseKeyRec pairs)

-- 9.
decipher :: Int -> Char -> Char
decipher n c
    | c `elem` ['A'..'Z'] = lookUp c (reverseKey (makeKey n))
    | otherwise = c

decipherStr :: Int -> String -> String
decipherStr n s = [decipher n c | c <- s, c `elem` ['A'..'Z'] || c `elem` ['1'..'9'] || c == ' ']

-- 10.
contains :: String -> String -> Bool
contains [] [] = True
contains [] pattern = False
contains s@(firstLetter:otherLetters) pattern
    | isPrefixOf pattern s == True = True
    | otherwise = contains otherLetters pattern

-- 11.
candidates :: String -> [(Int, String)]
candidates s = [(n, decipherStr n s) | n <- [0..25], let deciphered = (decipherStr n s), (contains deciphered "THE") || (contains deciphered "AND")]

-- Optional Material

-- 12.

getInterval :: Int -> Int -> String -> String
getInterval x y s = [s !! i | i <- [x..y]]

splitEachFive :: String -> [String]
splitEachFive s
    | (length s) `mod` 5 == 0 = [getInterval i (i + 4) s | i <- [0, 5..length s - lenMod - 1]]
    | otherwise = [getInterval i (i + 4) s | i <- [0, 5..length s - lenMod - 1]] ++ [getInterval (length s - lenMod) (length s - 1) s ++ replicate (5 - lenMod) 'X']
    where lenMod = length s `mod` 5

-- 13.
prop_transpose :: String -> Bool
prop_transpose s = (splitEachFive s) == (transpose(transpose(splitEachFive s)))

-- 14.
getString :: [String] -> String
getString [] = []
getString (firstString:otherStrings) = firstString ++ getString otherStrings

encrypt :: Int -> String -> String
encrypt n s = getString(transpose(splitEachFive(encipherStr n (normalize s))))  

-- 15.
uniteList :: [String] -> String
uniteList [] = []
uniteList (firstString:otherStrings) = firstString ++ uniteList otherStrings

splitEach_n :: Int -> String -> [String]
splitEach_n n s
    | lenMod `mod` n == 0 = [getInterval i (i + n - 1) s | i <- [0, n..length s - lenMod - 1]]
    | otherwise = [getInterval i (i + n - 1) s | i <- [0, n..length s - lenMod - 1]] ++ [getInterval (length s - lenMod) (length s - 1) s ++ replicate (n - lenMod) 'X']
    where lenMod = length s `mod` n

decrypt :: Int -> String -> String
decrypt n s = uniteList(transpose(splitEach_n l (decipherStr n s)))
              where l = length s `div` 5

-- Challenge (Optional)

-- 16.
countFor :: Char -> String -> Int
countFor c [] = 0
countFor c (firstChar:otherChars)
    | firstChar == c = 1 + countFor c otherChars
    | otherwise = countFor c otherChars

countFreqs :: String -> [(Char, Int)]
countFreqs s = [(s !! i, countFor (s !! i) s) | i <- [0..length s - 1], ((s !! i) `elem` (take i s)) == False] 

-- 17
searchForE :: [(Char, Int)] -> Int
searchForE [] = 0
searchForE (firstPair:otherPairs)
    | fst firstPair == 'E' = snd firstPair
    | otherwise = searchForE otherPairs

getFreqE :: String -> Int
getFreqE s = searchForE(countFreqs s)

freqDecipher :: String -> [String]
freqDecipher s = reverse [candidate | (_, candidate) <- sort[(getFreqE code, code) | code <- [decrypt n s | n <- [0..25]]]]
