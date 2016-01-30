-- Informatics 1 - Functional Programming
-- Tutorial 4
--
-- Due: the tutorial of week 6 (29/30 Oct)

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString [] [] = True
sameString [] (y:ys) = False
sameString (x:xs) [] = False
sameString (x:xs) (y:ys)
    | (toLower x) == (toLower y) = sameString xs ys
    | otherwise = False

-- 2.
prefix :: String -> String -> Bool
prefix [] [] = True
prefix s1 [] = False
prefix [] s2 = True
prefix s1 s2
    | (sameString s1 (take len_s1 s2)) == True = True
    | otherwise = prefix s1 (take (len_s2 - 1) s2)
    where len_s1 = length s1
          len_s2 = length s2

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
		         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str


-- 3.
contains :: String -> String -> Bool
contains text [] = True
contains [] pattern = False
contains text pattern
    | (prefix pattern text) == True = True
    | otherwise = contains (drop 1 text) pattern

prop_contains :: String -> Int -> Int -> Bool
prop_contains str st end = contains str substr
                        where substr = [str !! i | i <- [max 0 st..min end n]]
                              n = (length str) - 1


-- 4.
takeUntil :: String -> String -> String
takeUntil [] [] = []
takeUntil s1 [] = []
takeUntil [] s2 = []
takeUntil s1 s2
    | prefix s1 s2 = []
    | otherwise = (head s2) : takeUntil s1 (drop 1 s2)

dropUntil :: String -> String -> String
dropUntil [] [] = []
dropUntil s1 [] = []
dropUntil [] s2 = s2
dropUntil s1 s2
    | prefix s1 s2 = drop len_s1 s2
    | otherwise = dropUntil s1 (drop 1 s2)
    where len_s1 = length s1


-- 5.
split :: String -> String -> [String]
split [] [] = []
split s1 [] = []
split [] s2 = [s2]
split s1 s2 = takeUntil s1 s2 : split s1 (dropUntil s1 s2)

reconstruct :: String -> [String] -> String
reconstruct s [] = []
reconstruct s [x] = x
reconstruct s (x:xs) = x ++ s ++ reconstruct s xs

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML html = split sep (dropUntil sep html)
              where sep = "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails [] = []
takeEmails (link:otherLinks) = if(length email > 0) then (sep ++ email) : (takeEmails otherLinks) else (takeEmails otherLinks)
                             where email = dropUntil sep link
                                   sep = "mailto:"


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link = (name, email)
               where name = takeUntil "</a>" (dropUntil "\">" link)
                     email = takeUntil "\"" (dropUntil "mailto:" link)


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = [link2pair email | email <- takeEmails (linksFromHTML html)]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail wantedName list = nub [(name, email) | (name, email) <- list, contains name wantedName]

-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name (emailsFromHTML html)


-- Optional Material

-- 12.
hasInitials :: String -> Name -> Bool
hasInitials initials name = sameString initials nameInitials
                          where nameInitials = [head x | x <- split " " name]

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML f html = [(name, email) | (name, email) <- emailsFromHTML html, f name == True]

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML initials html = emailsByMatchFromHTML (hasInitials initials) html

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria name = (toLower lastLetter) `elem` vowels
                where lastLetter = last name
                      vowels = "aeiou"

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML html = emailsByMatchFromHTML myCriteria html

-- 15
getFirstName :: Name -> Name
getFirstName name = takeUntil " " name

getLastName :: Name -> Name
getLastName name = dropUntil " " name

ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [(getLastName name) ++ ", " ++ (getFirstName name) ++ (replicate (maxLen - length name + 3) ' ') ++ email | (name, email) <- addr]
                where maxLen = maximum [length name | (name, _) <- addr]
                
-- ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
