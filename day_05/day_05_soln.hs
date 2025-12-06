import System.Environment
import qualified Data.Array as A
import Data.List

main :: IO ()
main = do fileName <- getArgs
          contents <- readFile (head fileName)
          let rangesList = parseRanges $ lines contents
          let rangesArray = A.listArray (0, (length rangesList) - 1) rangesList
          let ingredients::[Integer] = map read . tail . dropWhile (/= "") $ lines contents
          let pt1 = sum $ map (binarySearchRange rangesArray) ingredients
          let pt2 = sum $ map ((+1) . snd) rangesList
          putStrLn (show pt1)
          putStrLn (show pt2)

{-
  Read the ranges from the input and generate a list of ordered pairs, where the
  first number in the ordered pair is the lower bound, and the second is the size
  of the interval (upper bound - lower bound). Then sort and collapse the ranges
  (see collapseRanges function comment) to obtain a list which represents disjoint
  intervals.
-}
parseRanges :: [String] -> [(Integer, Integer)]
parseRanges inputLines = collapseRanges . sort $ go inputLines
  where
    go [] = []
    go ("":lines) = []
    go (thisLine:lines) = (read lowNum, read (tail highNum) - read lowNum) : go lines
      where (lowNum, highNum) = span (/= '-') thisLine

{-
  Turn a sorted list of intervals of the form (lower bound, interval size), 
  ordered lexicographically into a sorted list of disjoint intervals of the same
  form.
-}
collapseRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
collapseRanges [] = []
collapseRanges [x] = [x]
collapseRanges (x:y:ys)
  -- The range given by x completely contains y. Simply remove y
  | (fst x + snd x >= fst y) && (fst x + snd x >= fst y + snd y) = collapseRanges ((fst x, snd x):ys)
  -- The range given by x can be extended by taking the right endpoint of y.
  | fst x + snd x >= fst y = collapseRanges ((fst x, (fst y - fst x) + snd y):ys)
  -- The ranges given by x and y do not overlap.
  | otherwise = (x : collapseRanges (y:ys))


-- Given an array of disjoint ranges of the form (lower bound, size of range) ordered
-- lexicographically and an integer, determine whether the integer falls in any of the ranges
binarySearchRange :: A.Array Int (Integer, Integer) -> Integer -> Integer
binarySearchRange arr x = go (A.bounds arr) arr x
  where
    go lims@(lowSearch, highSearch) arr x
      | lowSearch > highSearch = 0
      | thisLow <= x && x <= thisLow + thisHigh = 1
      | lowSearch == highSearch = 0
      | thisLow < x = go (midPt + 1, highSearch) arr x
      | otherwise = go (lowSearch, midPt - 1) arr x
        where  midPt = (lowSearch + highSearch) `div` 2
               (thisLow, thisHigh) = arr A.! midPt