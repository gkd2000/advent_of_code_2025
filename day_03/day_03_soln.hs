import System.Environment
import Data.Char

main :: IO ()
main = do fileName <- getArgs
          contents <- readFile (head fileName)
          let batteryBanks = map (map (toInteger . digitToInt)) $ lines contents
          let pt1 = sum $ map (getJoltage 2) batteryBanks
          let pt2 = sum $ map (getJoltage 12) batteryBanks
          putStrLn (show pt1)
          putStrLn (show pt2)

-- This is unused; it was for part 1, but the part 2 generalizes easily. I can't bring myself to delete it
getJoltagePt1 :: [Integer] -> Integer
getJoltagePt1 xs = digit * 10 + (maximum (drop (idx + 1) xs))
  where (digit, idx) = getMaxAndIdx (init xs)

{-
  Given a the number of digits needed for our joltage and a list of Integers, return the joltage.
  We do this greedily: find the biggest number you can in the first part of the list, leaving
  enough numbers left in the list to obtain the desired number of digits. Then chop off the list at
  where you picked your big number, and repeat with the rest of the list for the next digit.
  The length of the list is passed to the go function (which takes the list, the length, and the
  number of digits remaining to be found) to avoid re-computing the length.

  You might think there are too few base cases here. If we are careful, we hould only need the one (I think)
-}
getJoltage :: Int -> [Integer] -> Integer
getJoltage digitsLeft xs = go xs (length xs) digitsLeft
  where
    go _ _ 0 = 0 -- base case
    go xs listLen digitsLeft = 10 ^ (digitsLeft - 1) * digit +
                               (go (drop (idx + 1) xs) (listLen - (idx + 1)) (digitsLeft - 1))
      where (digit, idx) = getMaxAndIdx (take (1 + listLen - digitsLeft) xs)

{-
  Given a list of numbers, return the maximum element in the list and the index (starting at 0) of the maximum
-}
getMaxAndIdx :: Real a => [a] -> (a, Int)
getMaxAndIdx xs = go xs (0, 0) 0
  where
    go [] max _ = max
    go (x:xs) (curMax, maxIdx) curIdx
      | x > curMax = go xs (x, curIdx) (curIdx + 1)
      | otherwise = go xs (curMax, maxIdx) (curIdx + 1)