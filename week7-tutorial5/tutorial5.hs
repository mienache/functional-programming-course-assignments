-- Informatics 1 - Functional Programming
-- Tutorial 5
--
-- Due: the tutorial of week 7 (5/6 November)

import Control.Monad( liftM, liftM2 )
import Data.List( nub, sort )
import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )

-- Warmup exercises

-- The datatype 'Fruit'
data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange (Orange s _) = s `elem` ["Tarocco", "Moro", "Sanguinello"]
isBloodOrange (Apple _ _) = False

-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments list = sum[segments |(Orange _ segments) <- filter (isBloodOrange) list]

-- 3.
worms :: [Fruit] -> Int
worms list = length $ filter (\(Apple _ w) -> w) list

-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  not (eval e p) || eval e q
eval e (p :<->: q)    =  eval e p == eval e q

-- retrieves the names of variables from a proposition -
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- Exercises ------------------------------------------------------------

-- 4.
p1 = ((Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q"))
p2 = ((Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q")))
p3 = ((Var "P" :&: (Var "Q" :|: Var "R")) :&: (((Not (Var "P") :|: Not (Var "Q")) :&: ((Not (Var "P")) :|: (Not (Var "R"))))))


-- 5.
tautology :: Prop -> Bool
tautology prop = and values
               where values = [eval env prop | env <- (envs $ names prop)]

prop_taut1 :: Prop -> Bool
prop_taut1 prop = (tautology prop) || (satisfiable (Not prop))

prop_taut2 :: Prop -> Bool
prop_taut2 prop = ((satisfiable prop) == False) || (tautology (Not prop) == False)


-- 6.
p4 = ((Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q"))
p5 = ((Var "P" :->: Var "Q") :&: (Var "P" :&: Not (Var "Q")))
p6 = ((Var "P" :<->: Var "Q") :&: ((Var "P" :&: Not (Var "Q")) :|: ((Not (Var "P")) :&: Var "Q")))


-- 7.
equivalent :: Prop -> Prop -> Bool
equivalent prop1 prop2
    | sort (names prop1) /= sort (names prop2) = False
    | otherwise = (and sameEvaluation)
                where sameEvaluation = [eval env prop1 == eval env prop2 | env <- (envs $ names prop1)]

equivalent' :: Prop -> Prop -> Bool
equivalent' prop1 prop2
    | sort (names prop1) /= sort (names prop2) = False
    | otherwise = tautology (prop1 :<->: prop2)

prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent prop1 prop2 = (equivalent prop1 prop2) == (equivalent' prop1 prop2)


-- 8.
subformulas :: Prop -> [Prop]
subformulas (Var name) = [Var name]
subformulas (T) = [T]
subformulas (F) = [F]
subformulas (Not (prop)) = nub $ [Not (prop)] ++ subformulas prop
subformulas (prop1 :|: prop2) = nub $ [prop1 :|: prop2] ++ subformulas prop1 ++ subformulas prop2
subformulas (prop1 :&: prop2) = nub $ [prop1 :&: prop2] ++ subformulas prop1 ++ subformulas prop2
subformulas (prop1 :->: prop2) = nub $ [prop1 :->: prop2] ++ subformulas prop1 ++ subformulas prop2
subformulas (prop1 :<->: prop2) = nub $ [prop1 :<->: prop2] ++ subformulas prop1 ++ subformulas prop2

-- Optional Material

-- 9.
-- check for negation normal form
isNNF :: Prop -> Bool
isNNF (Var _) = True
isNNF F = True
isNNF T = True
isNNF (Not (F)) = True
isNNF (Not (T)) = True
isNNF (Not (Var _)) = True
isNNF ((p :|: q)) = (isNNF p) && (isNNF q)
isNNF ((p :&: q)) = (isNNF p) && (isNNF q)
isNNF _  = False

-- 10.
-- convert to negation normal form
toNNF :: Prop -> Prop
toNNF (Not (Not (p))) = toNNF p
toNNF (Not (p :&: q)) = (toNNF (Not (p))) :|: (toNNF (Not (q)))
toNNF (Not (p :|: q)) = (toNNF (Not (p))) :&: (toNNF (Not (q)))
toNNF (p :->: q) = (toNNF (Not (p))) :|: (toNNF q)
toNNF (p :<->: q) = (toNNF (p :->: q)) :&: (toNNF (q :->: p))
toNNF prop@(Not p)
    | isNNF prop == True = prop
    | otherwise = toNNF (Not (toNNF p))
toNNF prop@(p :&: q)
    | isNNF prop == True = prop
    | otherwise = toNNF((toNNF p) :&: (toNNF q))
toNNF prop@(p :|: q)
    | isNNF prop == True = prop
    | otherwise = toNNF((toNNF p) :|: (toNNF q))
toNNF prop = prop

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Prop -> Bool
prop_NNF1 p  =  isNNF (toNNF p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Prop -> Bool
prop_NNF2 p  =  equivalent p (toNNF p)


-- 11.
-- check whether a formula is in conj. normal form
notAndIn :: Prop -> Bool
notAndIn (_ :&: _) = False
notAndIn (p :|: q) = notAndIn p && notAndIn q
notAndIn prop = True

isCNF :: Prop -> Bool
isCNF F = True
isCNF T = True
isCNF (Var _) = True
isCNF (Not (F)) = False
isCNF (Not (T)) = False
isCNF (F :|: q) = False
isCNF (p :|: F) = False
isCNF (T :|: q) = False
isCNF (p :|: T) = False
isCNF (F :&: q) = False
isCNF (p :&: F) = False
isCNF (T :&: q) = False
isCNF (p :&: T) = False
isCNF (Not (Var _)) = True
isCNF (Not (_)) = False
isCNF (p :&: q) = isCNF p && isCNF q
isCNF (p :|: q) = notAndIn p && notAndIn q && isCNF p && isCNF q
isCNF prop      = False


-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Prop]] -> Prop
listsToCNF [] = error "invalid expression"
listsToCNF [x]
    | null x == True = error "invalid expression"
    | otherwise = foldl (:|:) (head x) (tail x)
listsToCNF (x:xs)
    | null x == True = error "invalid expression"
    | otherwise = (foldl (:|:) (head x) (tail x)) :&: listsToCNF xs


-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Prop -> [[Prop]]
listsFromCNF (T) = [[T]]
listsFromCNF (F) = [[F]]
listsFromCNF (Var x) = [[Var x]]
listsFromCNF (Not (T)) = [[F]]
listsFromCNF (Not (F)) = [[T]]
listsFromCNF (Not (Var x)) = [[Not (Var x)]]
listsFromCNF (p :|: q) = [foldr(\x list -> x :list) (head (listsFromCNF q)) (head (listsFromCNF p   ))]
listsFromCNF (p :&: q) = listsFromCNF p ++ listsFromCNF q
listsFromCNF (Not (Not p)) = listsFromCNF p

-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Prop -> [[Prop]]
toCNFList T = [[T]]
toCNFList F = [[F]]
toCNFList (Var x) = [[Var x]]
toCNFList (Not (T)) = [[F]]
toCNFList (Not (F)) = [[T]]
toCNFList (Not (Var x)) = [[Not (Var x)]]
toCNFList (p :|: q) = [foldr(\x list -> nub (x :list)) pj qj | pj <- pCNF, qj <- qCNF]
                    where pCNF = toCNFList p
                          qCNF = toCNFList q
toCNFList (p :&: q) = (toCNFList p) ++ (toCNFList q)
toCNFList (p :->: q) = toCNFList ((Not (p)) :|: q)
toCNFList (p :<->: q) = toCNFList ((p:->:q) :&: (q:->:p))
toCNFList (Not (p :|: q)) = toCNFList ((Not (p)) :&: (Not (q)))
toCNFList (Not (p :&: q)) = toCNFList ((Not (p)) :|: (Not (q)))
toCNFList (Not (p :->: q)) = toCNFList (p :&: (Not (q)))
toCNFList (Not (p :<->: q)) = toCNFList ((Not ((p :->: q))) :|: ((Not (q :->: p))))
toCNFList (Not (Not p)) = toCNFList p

-- convert to conjunctive normal form
toCNF :: Prop -> Prop
toCNF p  =  listsToCNF (toCNFList p)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Prop -> Bool
prop_CNF p  =  equivalent p (toCNF p)




-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"
