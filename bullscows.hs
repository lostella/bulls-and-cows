module Main where

import System.Random
import System.IO
import System.Environment
import Data.Char

-- define the Guess data type
data Guess = Guess [Int] | Empty
-- the Guess data type is showable
instance Show Guess where
  show (Guess (x:xs)) = (show x)++" "++(show (Guess xs))
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

cslist :: [Int] -> [Int] -> [Int] -> [Int] -> (Int, [Int], [Int])
cslist (x:xs) (y:ys) rx ry
  | x == y = let (reccs, recrx, recry) = (cslist xs ys rx ry)
    in (1 + reccs, recrx, recry)
  | otherwise = let (reccs, recrx, recry) = (cslist xs ys (x:rx) (y:ry))
    in (reccs, recrx, recry)
cslist [] [] rx ry = (0, rx, ry)

cblist :: [Int] -> [Int] -> Int
cblist (x:xs) ys
  | (elem x ys) == True = 1 + (cblist xs (rmlist x ys))
  | otherwise = cblist xs ys
cblist [] _ = 0

-- self explanatory
feedback :: Guess -> Guess -> Answer
feedback (Guess x) (Guess y) = Answer s b
  where (s, rx, ry) = cslist x y [] []
        b = cblist rx ry

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
    let bcs = words s
        b = read (head bcs) :: Int
        c = read ((head . tail) bcs) :: Int
        a = Answer b c
    if b /= l
      then gameLoop (k+1) l (filter (test g a) gs) as
      else hPutStr stdout ""

main :: IO ()
main =
  do
    args <- getArgs
    hSetBuffering stdin LineBuffering
    let l = read (args !! 0) :: Int
        d = read (args !! 1) :: Int
        r = read (args !! 2) :: Int
    gameLoop 0 l (guesses l d (r == 1)) (answers l)

