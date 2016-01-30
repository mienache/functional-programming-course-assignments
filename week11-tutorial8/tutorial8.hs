-- Informatics 1 - Functional Programming
-- Tutorial 8
--
-- Week 11 - due: 3/4 Dec.

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int]
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (ret, _, _, _, _) = ret
alph   (_, ret, _, _, _) = ret
start  (_, _, ret, _, _) = ret
final  (_, _, _, ret, _) = ret
trans  (_, _, _, _, ret) = ret


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm s c = [y | (x, e, y) <- trans fsm, x == s && e == c]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts fsm s  = acceptsFrom (start fsm) fsm s
               where acceptsFrom x fsm [] = if x `elem` (final fsm) then True
                                            else False
                     acceptsFrom x fsm s  = or [acceptsFrom y fsm next_s | y <- delta fsm x (head s)]
                                          where next_s = tail s


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical list = nub $ sort $ list


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta fsm source c = canonical target
                    where target = concat [delta fsm x c | x <- source]


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm sources = nub $ sort $ [ddelta fsm source c | source <- sources, c <- alphabet] ++ sources
                 where alphabet = alph fsm

-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm sources
    | length sources == length newSources = sources
    | otherwise                           = reachable fsm newSources
    where newSources = next fsm sources


-- 8.

isFinalSuperstate :: (Ord q) => FSM q -> [q] -> Bool
isFinalSuperstate fsm states = or[state `elem` finalStates | state <- states]
                             where finalStates = final fsm

dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm states = [state | state <- states, isFinalSuperstate fsm state]

-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm sources = [(x, c, ddelta fsm x c) | x <- sources, c <- alphabet]
                   where alphabet = alph fsm


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic fsm = (newStates, alphabet, startState, newfinalStates, newTransitions)
                  where source         = start fsm
                        newStates      = reachable fsm [[source]]
                        alphabet       = alph fsm
                        startState     = [source]
                        newfinalStates = dfinal fsm newStates
                        newTransitions = dtrans fsm newStates


-- Optional Material
--11.
aut :: String -> FSM Int
aut str = (newStates, alphabet, startState, finalStates, transitions)
        where n = length str
              newStates = [0..n + 1]
              alphabet = ['a'..'z']
              startState = 0
              finalStates = [n]
              transitions = getTransitions str newStates
              getTransitions [] (x:xs)     = [(x, t, n + 1) | t <- alphabet] ++ [(n + 1, t, n + 1) | t <- alphabet]
              getTransitions (c:cs) (x:xs) = [(x, c, x + 1)] ++ [(x, t, n + 1) | t <- alphabet, t /= c]
                                          ++ getTransitions cs xs


-- For quickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

prop_aut1 n = accepts (aut n') n'
      where n' = safeString n
prop_aut2 n m = (m' == n') || (not $ accepts (aut n') m')
                where m' = safeString m
                      n' = safeString n

-- 12.
complement :: (Ord q) => FSM q -> FSM q
complement fsm = (s, alph fsm, start fsm, otherFinalStates, trans fsm)
               where otherFinalStates = [x | x <- s, not (x `elem` f)]
                     s                = states fsm
                     f                = final fsm


prop_complement :: String -> String -> Bool
prop_complement n m = (n' == m')
                      || accepts (complement $ aut n') m'
                      && (not $ accepts (complement $ aut n') n)
                      where n' = safeString n
                            m' = safeString m
-- 13.
unionFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
unionFSM fsm1 fsm2 = (newStates, alphabet, startState, finalStates, newTransitions)
                   where newStates      = [(x, y) | x <- states1, y <- states2]
                         states1        = states fsm1
                         states2        = states fsm2
                         alphabet       = alph fsm1
                         startState     = (start fsm1, start fsm2)
                         finalStates    = filter (\(x, y) -> x `elem` final1 || y `elem` final2) newStates
                         final1         = final fsm1
                         final2         = final fsm2
                         newTransitions = [((x1, x2), c1, (y1, y2)) | (x1, c1, y1) <- trans1, (x2, c2, y2) <- trans2, c1 == c2]
                         trans1         = trans fsm1
                         trans2         = trans fsm2

prop_union n m l =  accepts (unionFSM (aut n') (aut m')) l' == (accepts (aut n') l' || accepts (aut m') l') &&
                    accepts (unionFSM (aut n') (aut m')) n' && accepts (unionFSM (aut n') (aut m')) m'
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l
-- 14.
intersectFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
intersectFSM fsm1 fsm2 = (newStates, alphabet, startState, finalStates, newTransitions)
                   where newStates      = [(x, y) | x <- states1, y <- states2]
                         states1        = states fsm1
                         states2        = states fsm2
                         alphabet       = alph fsm1
                         startState     = (start fsm1, start fsm2)
                         finalStates    = filter (\(x, y) -> x `elem` final1 && y `elem` final2) newStates
                         final1         = final fsm1
                         final2         = final fsm2
                         newTransitions = [((x1, x2), c1, (y1, y2)) | (x1, c1, y1) <- trans1, (x2, c2, y2) <- trans2, c1 == c2]
                         trans1         = trans fsm1
                         trans2         = trans fsm2

prop_intersect n m l = accepts (intersectFSM (aut n') (aut m')) l' == (accepts (aut n') l' && accepts (aut m') l')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l

-- 15.
intFSM :: Ord q => FSM q -> FSM Int
intFSM fsm = (newStates, alphabet, startState, finalStates, newTransitions)
           where newStates = [0..n]
                 alphabet = alph fsm
                 startState = snd (head listStateToInt)
                 finalStates = [snd x | x <- listStateToInt, fst x `elem` finalOfFsm]
                 finalOfFsm = final fsm
                 newTransitions = [(find x, c, find y) | (x, c, y) <- transitions]
                 transitions = trans fsm
                 find x = snd (head [y | y <- listStateToInt, fst y == x])
                 listStateToInt = zip (states fsm) [0..n]
                 n = length (states fsm) - 1


prop_intFSM a b = intFSM (aut a') `accepts` b' == (aut a') `accepts` b' &&
                  (intFSM (complement (aut a')) `accepts` b' == (complement (aut a') `accepts` b'))
  where a' = safeString a
        b' = safeString b

-- 16.
-- Doesn't work on empty strings
concatFSM :: Ord q => Ord q' => FSM q -> FSM q' -> FSM Int
concatFSM fsm1 fsm2 = (newStates, alphabet, startState, finalStates, newTransitions)
                    where a = intFSM fsm1
                          b = intFSM fsm2
                          newStates = (states a) ++ (map (+ na) $ states b)
                          alphabet = nub $ alph a ++ alph b
                          startState = start a
                          finalStates = final1 ++ (map (+ na) (final b))
                          newTransitions = trans1 ++ [(x + na, c, y + na) | (x, c, y) <- trans2, x /= sb]
                                           ++ [(x, c, y + na) | x <- final1, (z, c, y) <- trans2, z == sb]
                          trans1 = trans a
                          trans2 = trans b
                          final1 = final a
                          sb = start b
                          na = length (states a)

prop_concat n m l = accepts (concatFSM (aut n') (aut m')) (n'++m') &&
                    accepts (concatFSM (aut n') (aut m')) l' == (l' == n'++m')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l
-- 16.
star :: (Ord q) => FSM q -> FSM q
star a = undefined

prop_star a n = (star $ aut a') `accepts` (concat [a' | x <- [0..n]]) &&
                (star $ aut a') `accepts` ""
      where a' = safeString a


prop1 a b = star ((aut a') `unionFSM` (aut b')) `accepts` (a'++b'++a'++a')
 where a' = safeString a
       b' = safeString b

prop2 a b = ((aut a') `intersectFSM` (intFSM ((aut b') `unionFSM` (aut a')))) `accepts` a'
             where a' = safeString a
                   b' = safeString b
