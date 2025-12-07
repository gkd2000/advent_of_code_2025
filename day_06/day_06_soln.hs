import System.Environment
import Data.List
import Data.Maybe
import Debug.Trace

-- To get an updated version of ghc, run
-- ghcup install ghc latest (or specify version)
-- ghcup set ghc <version>

main :: IO ()
main = do fileName <- getArgs
          contents <- readFile (head fileName)
          let (inputNums,inputOps) = fromJust . unsnoc $ lines contents
          let nums = parseNums inputNums
          let cols = transpose nums
          let ops = parseOps $ words inputOps
          let pt1 = sum $ applyOps ops cols
          let colWidths = map (maximum . (map (length . show))) cols
          let alignmentByNum = map (parseLineWithAlignment colWidths) inputNums
          let alignmentByDigit = map (map addDigitOffsets) $ transpose alignmentByNum
          let pt2Nums = map getRealNums alignmentByDigit
          let pt2 = sum $ applyOps ops pt2Nums
          putStrLn (show pt1)
          putStrLn (show pt2)

{-
  For part 2: Take a list of widths for each column and a single line of input,
  and extract each number, also saving its offset from the right of the column,
  starting at 0 if the number is right-justified.
-}
parseLineWithAlignment :: [Int] -> String -> [(Integer, Int)]
parseLineWithAlignment _ [] = []
parseLineWithAlignment (width:widths) str = (read numWithSpaces, length . takeWhile (== ' ') $ reverse numWithSpaces) : parseLineWithAlignment widths (drop (width + 1) str)
  where numWithSpaces = take width str


{-
  For part 2: Given a number and its offset from the right, decompose it into its
  digits and each of their offsets from the right. For example, the number 45 with
  an offset of 0 is right-justified, so the function would return [(5,0), (4,1)].
-}
addDigitOffsets :: (Integer, Int) -> [(Integer, Int)]
addDigitOffsets (0, _) = []
addDigitOffsets (num, offset) = (num `mod` 10, offset) : addDigitOffsets (num `div` 10, offset + 1)

{-
  For part 2: takes a list of lists of tuples. Each inner list is one number
  (read regularly/horizontally), and each tuple contains a digit and its offset
  from the right as returned by addDigitOffsets. Within the outermost list, the
  numbers are  ordered from top to bottom as they appear in the input, so most
  significant  digits appear first in the list. Extract the numbers by column by
  looking at all of the digits of the same offset and combining them into a single
  number.
-}
getRealNums :: [[(Integer, Int)]] -> [Integer]
getRealNums lst = go lst 0
  where
    go lst curOffset
      | null numsAtOffset = []
      | otherwise = (foldl (\x y -> x * 10 + y) 0 $ numsAtOffset) : go lst (curOffset + 1)
        where
          numsAtOffset = map fst $ concatMap (filter (\x -> snd x == curOffset)) lst

parseNums :: [String] -> [[Integer]]
parseNums [] = []
parseNums (str:strs) = (map read $ words str) : parseNums strs

parseOps :: [String] -> [Integer -> Integer -> Integer]
parseOps [] = []
parseOps (op:ops) = (if op == "+" then (+) else (*)) : parseOps ops

applyOps :: [Integer -> Integer -> Integer] -> [[Integer]] -> [Integer]
applyOps [] _ = []
applyOps _ [] = []
applyOps (op:ops) (lst:lsts) = ((foldr1 op lst) : (applyOps ops lsts))