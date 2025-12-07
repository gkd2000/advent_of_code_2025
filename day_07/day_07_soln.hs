import System.Environment
import qualified Data.Set as S
import Data.List

type Coord = (Int, Int)

main :: IO ()
main = do fileName <- getArgs
          contents <- readFile (head fileName)
          let (startPos, splitters) = parseInput $ lines contents
          let numSplits = iterateSplitting (snd startPos) splitters (length $ lines contents)
          let numPaths = countPaths (snd startPos) splitters (length $ lines contents)
          putStrLn (show numSplits)
          putStrLn (show numPaths)

{-
  Given the input tachyon manifold diagram, extract the start position (denoted by 'S')
  and the locations of each of the splitters (denoted by '^').
-}
parseInput :: [String] -> (Coord, S.Set Coord)
parseInput input = handleChar 0 0 input (-1, -1) S.empty
  where
    handleChar _ _ [] start splitters = (start, splitters)
    handleChar row col (ln:lns) start splitters
      | null ln = handleChar (row + 1) 0 lns start splitters
      | otherwise = case (head ln) of
          'S' -> handleChar row (col + 1) ((tail ln):lns) (row, col) splitters
          '^' -> handleChar row (col + 1) ((tail ln):lns) start (S.insert (row, col) splitters)
          '.' -> handleChar row (col + 1) ((tail ln):lns) start splitters

{-
  Parameters:
    Int         - start column of tachyon beam (we are assuming it starts at row 0)
    S.Set Coord - positions of all the splitters
    Int         - total number of rows in the input grid
  Iteratively build a list of tachyon beam locations for each row, keeping track
  of the number of times the beam is split. Return the total number of splits.
-}
iterateSplitting :: Int -> S.Set Coord -> Int -> Int
iterateSplitting tachyonCol splitters numRows = go [tachyonCol] 0
  where
    go :: [Int] -> Int -> Int
    go curTachyonCols row
      | row == numRows = 0
      | otherwise = 
        let (numSplits, newTachys) = splitOneRow curTachyonCols splitters row
        in numSplits + go newTachys (row + 1)

{-
  Parameters:
    Int         - columns containing tachyon beams for the current row
    S.Set Coord - positions of all the splitters
    Int         - current row
  For each position containing a tachyon beam, determine whether that position also
  contains a splitter. If so, then split the beam. Otherwise, keep the beam in the
  same column. Return an ordered pair consisting of the number of splits performed
  and the list of columns where tachyon beams appear after splitting.
-}
splitOneRow :: [Int] -> S.Set Coord -> Int -> (Int, [Int])
splitOneRow [] _ _ = (0, [])
splitOneRow (col:cols) splitters row =
  if S.member (row, col) splitters
    then (splitOneRow cols splitters row) # (1, [col - 1, col + 1])
    else (splitOneRow cols splitters row) # (0, [col])

{-
  Combine two pairs of the form (number of splits, positions of beams) by adding
  the number of splits and combining the number of beams, removing duplicates.
-}
(#) :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int])
(#) (a, bs) (c, ds) = (a + c, dedup (bs ++ ds))

dedup :: [Int] -> [Int]
dedup = map head . group . sort

{-
  Parameters:
    Int          - start column of tachyon beam (we are assuming it starts at row 0)
    S.Set Coord  - positions of all the splitters
    Int          - total number of rows in the input grid
  For each position containing a tachyon beam, determine whether that position also
  contains a splitter. If so, then split the beam, keeping track of its multiplicity,
  which is the number of beams that would be in that position if they were not
  combined, or equivalently, the number of ways to reach that position. If the
  position does not contain a splitter, then keep the beam in the same column.
  Return the total number of paths through the manifold, which is given by
  summing the multiplicities of all the beams in the final row.
-}
countPaths :: Int -> S.Set Coord -> Int -> Int
countPaths tachyonCol splitters numRows = go [(tachyonCol, 1)] 0
  where
    go :: [(Int, Int)] -> Int -> Int
    go curTachyonData row
      | row == numRows = sum $ (map snd) curTachyonData
      | otherwise = go (splitWithMultiplicity curTachyonData splitters row) (row + 1)

{-
  Parameters:
    [(Int, Int)] - list of pairs (column, multiplicity), each representing one beam
                   in the current row
    S.Set Coord  - positions of all the splitters
    Int          - current row
  For each position containing a tachyon beam, determine whether that position also
  contains a splitter. If so, then split the beam. Otherwise, keep the beam in the
  same column. If after doing this, there are several beams in the same position,
  then instead create one entry for that position with multiplicity the sum of the
  multiplicities of each of the constituent beams. Return the list of pairs
  (column, multiplicity) after performing the splitting and condensing.
-}
splitWithMultiplicity :: [(Int, Int)] -> S.Set Coord -> Int -> [(Int, Int)]
splitWithMultiplicity [] _ _ = []
splitWithMultiplicity ((col,mult):beams) splitters row =
  if S.member (row, col) splitters
    then combineMults [((col - 1), mult), ((col + 1), mult)] (splitWithMultiplicity beams splitters row)
    else combineMults [(col, mult)] (splitWithMultiplicity beams splitters row)

{-
  Given two lists with elements of the form (col, multiplicity), assuming that each
  list individually has no duplicate columns, combine the lists. If there are any
  elements which are the same column between the two lists, combine them into one
  element and make its new multiplicity the sum of the original multiplicities.
-}
combineMults :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
combineMults as bs = map combineElts (groupBy (\a b -> fst a == fst b) $ sort (as ++ bs))

combineElts :: [(Int, Int)] -> (Int, Int)
combineElts [x] = x
combineElts [(colA, multA), (colB, multB)] = (colA, multA + multB)

{-
  Idea: I think that if you remove the dedup from the (#) function above, this gives
  you one less than the number you want for part 2, because it counts the number
  of splits if you don't collapse any beams, but that number of splits creates
  n+1 distinct beams at the end, each representing a unique path through the
  manifold. However, just removing the dedup takes a long time.
  Instead, keep track of the "multiplicity" of each beam. This is how many beams
  would be there, if we hadn't collapsed them. So in a small example input:

  .......S.......
  .......|.......  // this beam has multiplicity 1
  ......|^|......
  ......|.|......  // these beams each bave mutiplicity 1
  .....|^|^|.....
  .....|.|.|.....  // these beams have multiplicities 1, 2, and 1 (in order)
  ....|^|^|^|....
  ....|.|.|.|....  // these beams have mutiplicities 1, 3, 3, and 1 (in order)
                   // the beams of multiplicity 3 have that because you can either
                   // get to them from a beam of multiplicity 1 (at the edge), or
                   // from a beam of multiplicity 2 (at the center)

  To calculate the multiplicity of each beam, count the number of beams at that
  position (before deduping) and add their multiplicities.
-}