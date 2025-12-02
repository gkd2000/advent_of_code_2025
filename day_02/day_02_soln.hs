#!/usr/bin/env cabal
{- cabal:
build-depends:
  base ^>=4.18.3.0,
  containers ^>= 0.8,
  split ^>= 0.2.5
-}

import Data.List.Split
import System.Environment

{-
  To run a compiled executable, pass the filename with the input as a command line argument.
  To run the main function in GHCI, run :main <filename>.
-}
main :: IO ()
main = do fileName <- getArgs
          contents <- readFile (head fileName)
          let ranges = splitOn "," contents
          let pt1 = sum . filter isDoubledNum $ concatMap enumerateNumsInRange ranges
          let pt2 = sum . filter checkSequences $ concatMap enumerateNumsInRange ranges
          putStrLn (show pt1)
          putStrLn (show pt2)

-- Given a string of the form X-Y, where X and Y are numbers, return the list
-- of all numbers between X and Y, inclusive
enumerateNumsInRange :: String -> [Int]
enumerateNumsInRange str = [read $ head startAndEnd .. read $ last startAndEnd]
  where
    startAndEnd = splitOn "-" str

-- Given a number, determine whether it is a sequence of numbers which has been repeated exactly twice
isDoubledNum :: Int -> Bool
isDoubledNum x
  | odd (countDigits x) = False
  | otherwise = (fst splitX) == (snd splitX)
  where
    splitX = divMod x (10 ^ ((countDigits x) `div` 2))

-- Given a number, determine whether it is a sequence of numbers which has been repeated at least twice
checkSequences :: Int -> Bool
checkSequences x = go (countDigits x) 1 x
  where
    go numDigits subSeqLen x
      | subSeqLen * 2 > numDigits = False
      | numDigits `mod` subSeqLen /= 0 = go numDigits (subSeqLen + 1) x
      | otherwise = (x == (repeatDigits x subSeqLen (numDigits `div` subSeqLen))) ||
                go numDigits (subSeqLen + 1) x

-- Given a number x, a length m, and a number n, repeats the first m digits of x n times.
-- Returns the result as an int.
repeatDigits :: Int -> Int -> Int -> Int
repeatDigits x lenToRepeat numRepetitions =
    read . concat . replicate numRepetitions . take lenToRepeat $ show x

countDigits :: Int -> Int
countDigits = length . show