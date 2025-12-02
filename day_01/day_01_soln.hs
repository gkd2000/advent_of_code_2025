import Debug.Trace

_dialSize = 100
_startPos = 50

main :: IO ()
main = do contents <- readFile "input.txt"
          let turns = map lineToNum (lines contents)
        --   putStrLn (show turns)
          let pt1 = countZeros turns
          let pt2 = countZerosPassed turns
          putStrLn ("Part 1: " ++ show pt1)
          putStrLn ("Part 2: " ++ show pt2)

-- Convert a line of input into a number representing a turn of the dial. Right
-- is positive, left is negative
lineToNum :: String -> Int
lineToNum ('R':num) = read num
lineToNum ('L':num) = -1 * (read num)

-- Given a sequence of turns, assuming the dial starts at 50, count the number
-- of times the dial is on 0 at the end of a turn
countZeros :: [Int] -> Int
countZeros nums = go nums _startPos 0
  where
    go [] curPos count = count
    go (x:xs) curPos count =
      if ((curPos + x) `mod` _dialSize == 0)
        then go xs 0 (count + 1)
        else go xs ((curPos + x) `mod` _dialSize) count

-- Given a sequence of turns, assuming the dial starts at 50, count the number
-- of times the dial passes 0
countZerosPassed :: [Int] -> Int
countZerosPassed nums = go nums _startPos 0
  where
    go [] curPos count = count
    go (x:xs) curPos count
      -- Current position is not 0 and we are at or have passed 0 going left.
      -- Count the number of zeros passed, plus the one we are currently at (if we are at 0).
      | (curPos /= 0) && (newPos <= 0) = go xs newPosAdjusted (1 + (count - (newPos `quot` _dialSize)))
      -- Current position is 0 and we have passed 0 going left.
      -- Do not count the 0 we started on as "passing 0"; it has already been counted in previous call.
      | (curPos == 0) && (newPos < 0) = go xs newPosAdjusted (count - (newPos `quot` _dialSize))
      -- We passed 0 going right.
      | newPos >= _dialSize = go xs newPosAdjusted (count + (newPos `quot` _dialSize))
      -- No 0s were passed.
      | otherwise = go xs newPosAdjusted count
        where newPos = curPos + x
              newPosMod = newPos `mod` _dialSize
              newPosAdjusted = if newPosMod < 0 then (newPosMod + _dialSize) else newPosMod
