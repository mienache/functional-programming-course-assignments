-- INF 1 Functional Programming
--
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT
                  )

where

-- Modules for testing

import Test.QuickCheck
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ leftSon rightSon) = 1 + max (depth leftSon) (depth rightSon)

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node key value left right) = (toList left) ++ [(key, value)] ++ (toList right)

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get _ Leaf = Nothing
get key (Node k v leftSon rightSon)
    | key == k  = Just v
    | key < k   = get key leftSon
    | otherwise = get key rightSon

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10
fromList :: Ord k => [(k,a)] -> Keymap k a
fromList list = foldr (\(key, value) acc -> set key value acc) (Leaf) list


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT key (Node k v leftSon rightSon)
    | key > k = (Node k v leftSon (filterLT key rightSon))
    | key == k = leftSon
    | otherwise = filterLT key leftSon

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT key (Node k v leftSon rightSon)
    | key > k = filterGT key rightSon
    | key == k = rightSon
    | otherwise = (Node k v (filterGT key leftSon) rightSon)

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge tree1 tree2 = foldr (\(key, val) acc -> set key val acc) Leaf mergedList
            where mergedList = list1 ++ list2
                  list1 = toList tree1
                  list2 = toList tree2

prop_merge :: (Ord k, Ord v) => [(k, v)] -> [(k, v)] -> Bool
prop_merge list1 list2 = compareKeys (toList (merge tree1 tree2)) (eliminateDoubleKeys $ sort (list1 ++ list2))
                       where
                             tree1 = fromList list1
                             tree2 = fromList list2
                             compareKeys [] []         = True
                             compareKeys [] _          = False
                             compareKeys _ []          = False
                             compareKeys (x:xs) (y:ys) = (fst x == fst y) && compareKeys xs ys
                            --  eliminateDoubleKeys [] = []
                            --  eliminateDoubleKeys xs = head xs : [xs !! i | i <- [1..n - 1], fst (xs !! i) /= fst (xs !! (i - 1))]
                                                    -- where n = length xs
                             eliminateDoubleKeys [] = []
                             eliminateDoubleKeys [x] = [x]
                             eliminateDoubleKeys (x1:x2:xs) = if fst x1 == fst x2
                                                                  then eliminateDoubleKeys (x2:xs)
                                                              else x1 : eliminateDoubleKeys (x2:xs)
-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del _ Leaf = Leaf
del key (Node k v leftSon rightSon)
    | key > k = merge (Node k v leftSon Leaf) (del key rightSon)
    | key == k = merge leftSon rightSon
    | otherwise = merge (del key leftSon) (Node k v Leaf rightSon)

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select f tree = fromList filteredList
              where filteredList = [x | x <- list, f (snd x)]
                    list = toList tree
