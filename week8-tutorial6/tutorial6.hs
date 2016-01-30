-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 12/13 November

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen [] = 0
longestProductLen (x:xs) = max (length name) (longestProductLen xs)
                         where name = fst item
                               item = snd x

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (barcode, item) = barcode ++ "..." ++ name ++ (replicate (n - len) '.') ++ "..." ++ unit
                             where name = fst item
                                   unit = snd item
                                   len  = length name

showCatalogue :: Catalogue -> String
showCatalogue catalogue = concat [formatLine n (barcode, item) ++ "\n" | (barcode, item) <- list]
                        where n = longestProductLen [(barcode, item) | (barcode, item) <- list]
                              list = toList catalogue
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just val):xs) = val : catMaybes xs
catMaybes ((Nothing):xs) = catMaybes xs

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems barcodes catalogue = catMaybes maybeItems
                           where maybeItems = [lookup barcode list | barcode <- barcodes]
                                 list       = toList catalogue






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
