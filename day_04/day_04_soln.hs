import System.Environment
import qualified Data.Map as M

main :: IO ()
main = do fileName <- getArgs
          contents <- readFile (head fileName)
          let contentsByLine = lines contents
          let neighborMap = interpretInput 0 contentsByLine (createEmptyMap (length contentsByLine) ((length . head) contentsByLine))
          let pt1 = M.size $ M.filter (< 4) neighborMap
          let pt2 = (M.size neighborMap) - (M.size $ removeAllPapers neighborMap)
          putStrLn (show pt1)
          putStrLn (show pt2)

{-
  Given the number of rows and columns, create a map where the keys are the
  positions in an matrix with the specified number of rows and columns, and
  the values are all 0.
-}
createEmptyMap :: Int -> Int -> M.Map (Int, Int) Int
createEmptyMap rows cols = M.fromAscList [((i, j), 0) | i <- [0..(rows - 1)], j <- [0..(cols - 1)]]

{-
  Given the current row (which starts at 0 and is incremented in subsequent recursive calls),
  a grid of characters from the input, and a map, populate the map so that the keys are the
  positions of paper in the grid, and the values are the number of paper neighbors for that
  key.
-}
interpretInput :: Int -> [[Char]] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
interpretInput _ [] neighborMap = neighborMap
interpretInput rowNum paperPos neighborMap = go rowNum 0 paperPos neighborMap
  where
    go rowNum colNum (thisRow:otherRows) neighborMap
      | colNum == (length (head paperPos)) = interpretInput (rowNum + 1) otherRows neighborMap
      | head thisRow == '.' = go rowNum (colNum + 1) ((tail thisRow) : otherRows) (M.delete (rowNum, colNum) neighborMap)
      | otherwise = go rowNum (colNum + 1) ((tail thisRow) : otherRows) (updateNeighbors rowNum colNum (+ 1) neighborMap)

{-
  Given a row, column, function telling you how to update a value, and a map, find the keys
  corresponding to the neighbors of the row and column, then update the values of those keys
  according to the function. In practice, we use this to increment or decrement the number
  of neighbors for a given position.
-}
-- parameters: row, col, how to update a single entry, input map
updateNeighbors :: Int -> Int -> (Int -> Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
updateNeighbors row col updateFunc thisMap =
  let neighbors = (tail [(row + i, col + j) | i <- [0, -1, 1], j <- [0, -1, 1]]) in
    go neighbors updateFunc thisMap
      where
        go [] _ thisMap = thisMap
        go (key:keys) updateFunc thisMap = go keys updateFunc (M.adjust updateFunc key thisMap)

-- ves: this is what I was doing before that was horribly slow
-- MStrict.unionsWith combineFunc $ map (\k -> MStrict.adjust updateFunc k thisMap) neighbors

{-
  Given a map of (position, number of neighbors) pairs, remove all of the keys which have
  fewer than 4 neighbors and update the counts of their remaining neighbors appropriately.
  Continue doing this until no more keys can be removed.
-}
removeAllPapers :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
removeAllPapers neighbors
  | null removable = neighbors
  | otherwise = removeAllPapers (removeOnePaperBatch (M.keys removable) notRemovable)
    where (removable, notRemovable) = M.partition (< 4) neighbors

{-
  Given a list of positions where papers can be removed and a map, update the neighbors
  of all of these positions by decrementing their neighbor count.
-}
removeOnePaperBatch :: [(Int, Int)] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
removeOnePaperBatch [] neighbors = neighbors
removeOnePaperBatch ((row, col):keys) neighbors = removeOnePaperBatch keys (updateNeighbors row col (+ (-1)) neighbors)