module Main where

import System.Random
import Data.Char
import System.IO

data Guess = Guess Int Int Int Int | Empty
instance Show Guess where
  show (Guess w x y z) = (show w)++","++(show x)++","++(show y)++","++(show z)
  show Empty = "error"
instance Eq Guess where
  (Guess x1 x2 x3 x4) == (Guess y1 y2 y3 y4) = (x1,x2,x3,x4) == (y1,y2,y3,y4)
  Empty == Empty = True
  _ == _ = False

data Answer = Answer Int Int
instance Show Answer where
  show (Answer s b) = show (s, b)
instance Eq Answer where
  (Answer s1 b1) == (Answer s2 b2) = s1 == s2 && b1 == b2

list :: Guess -> [Int]
list (Guess w x y z) = w:x:y:z:[]
list Empty = []

combos :: [Guess]
combos = [Guess w x y z | w <- [0..9], x <- [0..9], y <- [0..9], z <- [0..9],
  w /= x, w /= y, w /= z, x /= y, x /= z, y /= z]

booltoint :: Bool -> Int
booltoint True = 1
booltoint _ = 0

answers :: [Answer]
answers = [Answer 0 1, Answer 0 2, Answer 1 1, Answer 1 0, Answer 0 0, Answer 2 0, Answer 0 3, Answer 1 2,
  Answer 2 1, Answer 3 0, Answer 0 4, Answer 1 3, Answer 2 2]

idx :: Eq a => a -> [a] -> Int
idx y (x:xs)
  | x == y = 0
  | x /= y = 1 + (idx y xs)
idx _ [] = 0

cslist :: [Int] -> [Int] -> Int
cslist (x:xs) (y:ys)
  | x == y = 1 + (cslist xs ys)
  | x /= y = (cslist xs ys)
cslist [] [] = 0

countsquares :: Guess -> Guess -> Int
countsquares x y = cslist (list x) (list y)

cblist :: [Int] -> [Int] -> Int
cblist (x:xs) (y:ys) = (booltoint (elem x ys)) + (booltoint (elem y xs)) + (cblist xs ys) 
cblist [] [] = 0

countballs :: Guess -> Guess -> Int
countballs x y = cblist (list x) (list y)

feedback :: Guess -> Guess -> Answer
feedback t g = Answer (countsquares t g) (countballs t g)

test :: Guess -> Answer -> Guess -> Bool
test g a t = (feedback g t) == a && g /= t

minmax :: [Guess] -> Guess
minmax cs = mini cs cs (length cs) Empty

mini :: [Guess] -> [Guess] -> Int -> Guess -> Guess
mini cs (x:xs) mv mc =
  if v < mv
    then mini cs xs v x
    else mini cs xs mv mc
  where v = maxi cs x answers mv
mini _ [] _ mc = mc

maxi :: [Guess] -> Guess -> [Answer] -> Int -> Int
maxi cs c (a:as) t =
  if len >= t
    then t
    else max len (maxi cs c as t)
  where len = length (filter (test c a) cs)
maxi _ _ [] _ = 0

choose :: Int -> [Guess] -> IO Guess
choose k cs =
  if k < 1
    then do
      i <- randomRIO (0, (length cs)-1)
      return (cs !! i)
    else do
      return (minmax cs)

gameLoop :: Int -> [Guess] -> IO ()
gameLoop k set =
  do
    g <- choose k set
    hPrint stdout g
    hFlush stdout
    s <- hGetLine stdin
    let a = Answer ((digitToInt . head) s) ((digitToInt . head . tail . tail) s)
    if (digitToInt . head) s /= 4
      then gameLoop (k+1) (filter (test g a) set)
      else hPutStr stdout ""

main :: IO ()
main =
  do
    hSetBuffering stdin LineBuffering
    gameLoop 0 combos

