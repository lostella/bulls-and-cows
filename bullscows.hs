module Main where

import System.Random
import System.IO
import System.Environment
import Data.Char

-- define the Guess data type
data Guess = Guess [Int] | Empty
-- the Guess data type is showable
instance Show Guess where
  show (Guess (x:xs)) = (show x)++","++(show (Guess xs))
  show (Guess []) = ""
  show Empty = "error"
-- the Guess data type is comparable
instance Eq Guess where
  (Guess (x:xs)) == (Guess (y:ys)) = (x == y) && ((Guess xs) == (Guess ys))
  (Guess []) == (Guess []) = True
  Empty == Empty = True
  _ == _ = False

-- define the Answer data type, which is again showable and comparable
data Answer = Answer Int Int
instance Show Answer where
  show (Answer s b) = show (s, b)
instance Eq Answer where
  (Answer s1 b1) == (Answer s2 b2) = s1 == s2 && b1 == b2

combosnorep :: Int -> Int -> [Int] -> [[Int]]
combosnorep 0 _ _ = [[]]
combosnorep l d rs = [(x:xs) | x <- [0..(d-1)], (elem x rs) == False, xs <- (combosnorep (l-1) d (x:rs))]

combos :: Int -> Int -> Bool -> [[Int]]
combos 0 _ True = [[]]
combos l d True = [(x:xs) | x <- [0..(d-1)], xs <- (combos (l-1) d True)]
combos l d False = combosnorep l d []

guesses :: Int -> Int -> Bool -> [Guess]
guesses l d r = [Guess x | x <- (combos l d r)]

booltoint :: Bool -> Int
booltoint True = 1
booltoint _ = 0

-- the list of possible replies
answers :: Int -> [Answer]
answers l = [Answer b c | b <- [0..l], c <- [0..l], b+c <= l]

-- computes the index of the first occurence of the first argument in the list
-- provided as second argument
idx :: Eq a => a -> [a] -> Int
idx y (x:xs)
  | x == y = 0
  | x /= y = 1 + (idx y xs)
idx _ [] = 0

-- removes the first instance of an element from a list (if there is one)
rmlist :: Eq a => a -> [a] -> [a]
rmlist x (y:ys)
  | x == y = ys
  | x /= y = (y:(rmlist x ys))
rmlist x [] = []

-- counts on how many positions two lists contain the same integer
cslist :: [Int] -> [Int] -> Int
cslist (x:xs) (y:ys)
  | x == y = 1 + (cslist xs ys)
  | x /= y = (cslist xs ys)
cslist [] [] = 0

-- self explanatory
countsquares :: Guess -> Guess -> Int
countsquares (Guess xs) (Guess ys) = cslist xs ys

-- counts how many elements in each of the two lists appear also in the other
-- (but in a different position)
cblist :: [Int] -> [Int] -> [Int] -> [Int] -> Int
cblist (x:xs) (y:ys) rxs rys
  | x == y = cblist xs ys rxs rys
  | x /= y && (elem x rys) && (elem y rxs) = 2 + (cblist xs ys (rmlist y rxs) (rmlist x rys))
  | x /= y && (elem y rxs) = 1 + (cblist xs ys (x:(rmlist y rxs)) rys)
  | x /= y && (elem x rys) = 1 + (cblist xs ys rxs (y:(rmlist x rys)))
  | x /= y = (cblist xs ys (x:rxs) (y:rys))
cblist [] [] _ _ = 0

-- self explanatory
countballs :: Guess -> Guess -> Int
countballs (Guess xs) (Guess ys) = cblist xs ys [] []

-- self explanatory
feedback :: Guess -> Guess -> Answer
feedback t g = Answer (countsquares t g) (countballs t g)

-- self explanatory
test :: Guess -> Answer -> Guess -> Bool
test g a t = (feedback g t) == a && g /= t

minmax :: [Guess] -> [Answer] -> Guess
minmax gs as = mini gs gs as (length gs) Empty

mini :: [Guess] -> [Guess] -> [Answer] -> Int -> Guess -> Guess
mini gs (x:xs) as mv mg =
  if v < mv
    then mini gs xs as v x
    else mini gs xs as mv mg
  where v = maxi gs x as mv
mini _ [] _ _ mg = mg

maxi :: [Guess] -> Guess -> [Answer] -> Int -> Int
maxi gs g (a:as) t =
  if len >= t
    then t
    else max len (maxi gs g as t)
  where len = length (filter (test g a) gs)
maxi _ _ [] _ = 0

choose :: Int -> [Guess] -> [Answer] -> IO Guess
choose k gs as =
  if k < 1
    then do
      i <- randomRIO (0, (length gs)-1)
      return (gs !! i)
    else do
      return (minmax gs as)

gameLoop :: Int -> Int -> [Guess] -> [Answer] -> IO ()
gameLoop k l gs as =
  do
    g <- choose k gs as
    hPrint stdout g
    hFlush stdout
    s <- hGetLine stdin
    let a = Answer ((digitToInt . head) s) ((digitToInt . head . tail . tail) s)
    if (digitToInt . head) s /= l
      then gameLoop (k+1) l (filter (test g a) gs) as
      else hPutStr stdout ""

main :: IO ()
main =
  do
    args <- getArgs
    hSetBuffering stdin LineBuffering
    gameLoop 0 (read (args !! 0) :: Int) (guesses (read (args !! 0) :: Int) (read (args !! 1) :: Int) ((read (args !! 2) :: Int) == 1)) (answers (read (args !! 0) :: Int))

