-- *****************************************************************************
--
-- bullscows.hs - Interactive bulls-and-cows (a.k.a. balls-and-squares) solver
--
-- Compile: ghc -O3 bullscows.hs (assuming GHC is used)
--
-- Usage: ./bullscows l d r
--
--  Where   l is the lenght of the sequence to guess
--          d is the number of symbols with which the sequence is composed
--          r is zero if repetitions are not allowed, nonzero otherwise
--  
--  The program will start making guesses, to which the user should reply with
--  feedback strings of the form "b c\n" where b is the number of right symbols
--  in the right position, and c is the number of right symbols in the wrong
--  position. Note that a whitespace is between b and c in the feedback string.
--
--  The game ends when the feedback string "l 0\n" is given by the user (in
--  which case the program has found the right sequence) or an inconsistency
--  among user feedbacks is detected.
--
--  When the game is over the program terminates.
--
-- *****************************************************************************

module Main where

import System.Random
import System.IO
import System.Environment
import Data.Char
import Data.List

-- Define the Guess data type
data Guess = Guess [Int] | Empty

-- The Guess data type is showable
instance Show Guess where
  show (Guess (x:xs)) = (show x) ++ " " ++ (show (Guess xs))
  show (Guess []) = ""
  show Empty = "error"
  
-- The Guess data type is comparable
instance Eq Guess where
  (Guess (x:xs)) == (Guess (y:ys)) = (x == y) && ((Guess xs) == (Guess ys))
  (Guess []) == (Guess []) = True
  Empty == Empty = True
  _ == _ = False

-- Define the Answer data type, which is again showable and comparable
data Answer = Answer Int Int
instance Show Answer where
  show (Answer s b) = show (s, b)
instance Eq Answer where
  (Answer s1 b1) == (Answer s2 b2) = s1 == s2 && b1 == b2
  
--------------------------------------------------------------------------------
-- GENERATING ALL POSSIBLE SEQUENCES FOR THE GAME ------------------------------
--------------------------------------------------------------------------------

-- Generates combinations of symbols without repetitions
combosNoRep :: Int      -- ^ the length of the combinations to be generated
            -> Int      -- ^ the number of available symbols
            -> [Int]    -- ^ list of already used symbols
            -> [[Int]]  -- ^ resulting list of combinations
combosNoRep 0 _ _ = [[]]
combosNoRep l d rs = [(x:xs) | x <- [0..(d-1)], (elem x rs) == False, xs <- (combosNoRep (l-1) d (x:rs))]

-- Generates combinations of symbols
combos :: Int       -- ^ the length of the combinations to be generated
       -> Int       -- ^ the number of available symbols
       -> Bool      -- ^ whether to allow repetitions (True) or not (False)
       -> [[Int]]   -- ^ resulting list of combinations
combos 0 _ True = [[]]
combos l d True = [(x:xs) | x <- [0..(d-1)], xs <- (combos (l-1) d True)]
combos l d False = combosNoRep l d []

-- Generates guesses using combos
guesses :: Int       -- ^ the length of the guesses to be generated
        -> Int       -- ^ the number of available symbols
        -> Bool      -- ^ whether to allow repetitions (True) or not (False)
        -> [Guess]   -- ^ resulting list of guesses
guesses l d r = [Guess x | x <- (combos l d r)]

--------------------------------------------------------------------------------
-- GENERATING ALL POSSIBLE REPLIES TO GUESSES ----------------------------------
--------------------------------------------------------------------------------

-- Generates the list of possible answers
answers :: Int      -- ^ length of the combination to guess
        -> [Answer] -- ^ resulting list of answers
answers l = [Answer b c | b <- [0..l], c <- [0..l], b+c <= l]

countSquaresList :: [Int] -> [Int] -> [Int] -> [Int] -> (Int, [Int], [Int])
countSquaresList (x:xs) (y:ys) rx ry
  | x == y = let (reccs, recrx, recry) = (countSquaresList xs ys rx ry)
    in (1 + reccs, recrx, recry)
  | otherwise = let (reccs, recrx, recry) = (countSquaresList xs ys (x:rx) (y:ry))
    in (reccs, recrx, recry)
countSquaresList [] [] rx ry = (0, rx, ry)

countBallsList :: [Int] -> [Int] -> Int
countBallsList (x:xs) ys
  | (elem x ys) == True = 1 + (countBallsList xs (delete x ys))
  | otherwise = countBallsList xs ys
countBallsList [] _ = 0

-- self explanatory
feedback :: Guess -> Guess -> Answer
feedback (Guess x) (Guess y) = Answer s b
  where (s, rx, ry) = countSquaresList x y [] []
        b = countBallsList rx ry

-- self explanatory
test :: Guess -> Answer -> Guess -> Bool
test g a t = (feedback g t) == a && g /= t

-- self explanatory, maybe better than length (filter p xs), should verify this
count :: (a -> Bool) -> [a] -> Int
count p [] = 0
count p (x:xs)
  | (p x) == True = 1 + (count p xs)
  | otherwise = (count p xs)

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
    then t -- here we prune the search
    else max len (maxi gs g as t)
  where len = (count (test g a) gs)
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
    gameLoop 0 l (guesses l d (r /= 0)) (answers l)

