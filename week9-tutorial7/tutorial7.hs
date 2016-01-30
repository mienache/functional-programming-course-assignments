-- Informatics 1 - Functional Programming
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (command1 :#: command2) = (split command1) ++ (split command2)
split Sit = []
split command = [command]

-- 1b. join
join :: [Command] -> Command
-- join [] = Sit
join [] = Go 0 -- this is for the purpose of exercise 4 (optimise) which should not return any Sit
join [x] = x
join (x:xs)
    | x == Sit = join xs
    | otherwise = x :#: join xs

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent command1 command2 = (split command1) == (split command2)

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join command = split ((join.split) command) == split command

prop_split :: Command -> Bool
prop_split command = and [verifyCommand x | x <- list]
                   where list = split command
                         verifyCommand Sit = False
                         verifyCommand (c1 :#: c2) = False
                         verifyCommand _ = True


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n command = foldr (\x y -> x :#: y) command list
               where list = replicate (n - 1) command

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon side = copy 5 (Go side :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon side n = copy n (Go side :#: Turn angle)
            where angle = 360.0 / (fromIntegral n)



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _ 0 _ _ = Sit
spiral side 1 _ angle = (Go side :#: Turn angle)
spiral side n step angle = (Go side :#: Turn angle) :#: spiral (side + step) (n - 1) step angle


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise command = compress list
                 where list = split command
                       compress xs = if len_res == length xs || len_res == 0 then join result else optimise (join result)
                                   where result = deleteZeroes [sumCommands y | y <- splitInSameCommands list]
                                         len_res = length result
                       sumCommands [x] = x
                       sumCommands (x:xs) = sumParameters x (sumCommands xs)
                       sumParameters (Go x) (Go y) = (Go (x + y))
                       sumParameters (Turn x) (Turn y) = (Turn (x + y))
                       sumParameters _ _ = error "invalid input for sumCommands"

deleteZeroes :: [Command] -> [Command]
deleteZeroes [] = []
deleteZeroes (x:xs)
    | getParameter x == 0 = deleteZeroes xs
    | otherwise = x : deleteZeroes xs

getParameter :: Command -> Float
getParameter (Go x) = x
getParameter (Turn x) = x
getParameter _ = error "invalid input for getParameter"

splitInSameCommands :: [Command] -> [[Command]]
splitInSameCommands [] = []
splitInSameCommands xs = (takeWhile (sameConstructor (head xs)) xs) : splitInSameCommands (dropWhile (sameConstructor (head xs)) xs)

sameConstructor :: Command -> Command -> Bool
sameConstructor (Go _) (Go _) = True
sameConstructor (Turn _) (Turn _) = True
sameConstructor _ _ = False


-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
            where f 0 = GrabPen red :#: Go 10
                  f x = g (x - 1) :#: p :#: f (x - 1) :#: p :#: g (x - 1)
                  g 0 = GrabPen blue :#: Go 10
                  g x = f (x - 1) :#: n :#: g (x - 1) :#: n :#: f (x - 1)
                  n   = Turn (60)
                  p   = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f (x - 1) :#: p :#: p :#: f (x - 1) :#: p :#: p :#: f (x - 1) :#: p :#: p
            where f 0 = GrabPen red :#: Go 10
                  f x = f (x - 1) :#: n :#: f (x - 1) :#: p :#: p :#: f (x - 1) :#: n :#: f (x - 1)
                  n   = Turn 60
                  p   = Turn (-60)


-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l (x - 1)
          where l 0 = GrabPen black :#: Go 10
                l x = p :#: r (x - 1) :#: f (x - 1) :#: n :#: l (x - 1) :#: f (x - 1) :#: l (x - 1) :#: n :#: f (x - 1)
                        :#: r (x - 1) :#: p
                r 0 = GrabPen black :#: Go 10
                r x = n :#: l (x - 1) :#: f (x - 1) :#: p :#: r (x - 1) :#: f (x - 1) :#: r (x - 1) :#: p :#: f (x - 1)
                        :#: l (x - 1) :#: n
                f 0 = GrabPen black :#: Go 10
                f x = f (x - 1)
                n   = Turn 90
                p   = Turn (-90)

peano_gosper :: Int -> Command
peano_gosper x = f (x - 1)
               where f 0 = GrabPen blue :#: Go 10
                     f x = f (x - 1) :#: p :#: g (x - 1) :#: p :#: p :#: g (x - 1) :#: n :#: f (x - 1) :#: n :#: n
                                     :#: f (x - 1) :#: f (x - 1) :#: n :#: g (x - 1) :#: p
                     g 0 = GrabPen blue :#: Go 10
                     g x = n :#: f (x - 1) :#: p :#: g (x - 1) :#: g (x - 1) :#: p :#: p :#: g (x - 1) :#: p :#: f(x - 1)
                             :#: n :#: n :#: f (x - 1) :#: n :#: g (x - 1)
                     n = Turn 60
                     p = Turn (-60)

cross :: Int -> Command
cross x = f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: n
        where f 0 = GrabPen red :#: Go 10
              f x = f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: n
                              :#: f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1)
              n = Turn 90
              p = Turn (-90)

branch :: Int -> Command
branch x = g (x - 1)
         where g 0 = GrabPen green :#: Go 10
               g x = f (x - 1) :#: n :#: Branch (Branch (g (x - 1)) :#: p :#: g (x - 1)) :#: p :#: f (x - 1) :#: Branch (p :#: f (x - 1) :#: g (x - 1)) :#: n :#: g (x - 1)
               f 0 = GrabPen green :#: Go 10
               f x = f (x - 1) :#: f (x - 1)
               n = Turn 22.5
               p = Turn (-22.5)

segment_32 :: Int -> Command
segment_32 x = f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1)
             where f 0 = GrabPen green :#: Go 10
                   f x = n :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1)
                           :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: f (x - 1) :#: p
                           :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1) :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1)
                           :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: p
                   n = Turn 90
                   p = Turn (-90)

randomFractal :: Int -> Command
randomFractal x = f (x - 1) :#: g (x - 1) :#: p :#: f(x - 1) :#: n :#: g (x - 1) :#: f (x - 1) :#: p
              where f 0 = GrabPen red :#: Go 10
                    f x = f (x - 1) :#: n :#: g (x - 1) :#: f (x - 1) :#: p :#: g (x - 1)
                    g 0 = GrabPen blue :#: Go 10
                    g x = g (x - 1) :#: n :#: f (x - 1) :#: n :#: g (x - 1)
                    n = Turn 30
                    p = Turn (-60)
