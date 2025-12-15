import System.Environment
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Debug.Trace

-- Part 2 was right, but it did take 1 - 1.5 hours to run. Obviously there are
-- improvements that could be made. Also note that my part 2 solution assumes
-- the interior of the shape is on the right as you walk around it. If this is
-- not the case, then the order of arguments to the isCcwTurn function should be
-- reversed.

data Rect = Rect
  { rectArea   :: Int
  , rectP1     :: (Int, Int)
  , rectP2     :: (Int, Int)
  } deriving (Eq, Ord, Show)

data Border = Border
  { borderX   :: Int
  , borderY   :: Int
  , borderDir :: Char
  } deriving (Eq, Ord)

instance Show Border where
  show (Border x y dir) = " " ++ show dir ++ "-(" ++ show x ++ ", " ++ show y ++ ")"

main :: IO ()
main = do fileName <- getArgs
          contents <- readFile (head fileName)
          let pts = map readPt $ lines contents
          let areas = sortBy (flip compare) $ map (\(x,y) -> Rect (getArea (x,y)) x y) [(p1, p2) | p1 <- pts, p2 <- pts, p1 < p2]
          let pt1 = rectArea $ head areas
          let bigBorder = S.fromList $ getBorder pts
          let bestInteriorRect = head . filter (isInterior bigBorder) $ map (\x -> (getRectangleBorder x, x)) (drop 19000 areas)
          -- let test = isInterior bigBorder $ (getRectangleBorder (Rect 0 (2,3) (9,5)), (Rect 0 (2,3) (9,5)))
          putStrLn (show pt1)
          -- putStrLn (show $ getBorder pts)
          putStrLn (show bestInteriorRect)
          putStrLn (show $ length areas)

readPt :: String -> (Int, Int)
readPt str = (read x, read $ tail rest)
  where
    (x, rest) = span (/= ',') str

getArea :: ((Int, Int), (Int, Int)) -> Int
getArea ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

getBorder :: [(Int, Int)] -> [Border]
getBorder pts = go pts (head pts)
  where
    go [x] firstPt = getLinePts x firstPt
    go (pt:pts) firstPt = getLinePts pt (head pts) ++ (go pts firstPt)

getRectangleBorder :: Rect -> [Border]
getRectangleBorder (Rect area (x1, y1) (x2, y2)) = getBorder [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

-- returns all the points on the line between p1 and p2. Also returns the direction
-- of the line being walked
getLinePts :: (Int, Int) -> (Int, Int) -> [Border]
getLinePts (x1', y1') (x2', y2') = [Border x1' y1' 'C'] ++ go (x1', y1') (x2', y2')
  where
    go (x1, y1) (x2, y2)
      | x1 == x2 && y1 < y2 = [Border x1 y 'N' | y <- [y1 + 1 .. y2 - 1]]
      | x1 == x2            = [Border x1 y 'S' | y <- reverse [y2 + 1 .. y1 - 1]]
      | y1 == y2 && x1 < x2 = [Border x y1 'E' | x <- [x1 + 1 .. x2 - 1]]
      | otherwise           = [Border x y1 'W' | x <- reverse [x2 + 1 .. x1 - 1]]

isInterior :: S.Set Border -> ([Border], Rect) -> Bool
isInterior bigBorder ([], _) = (trace ("returning true")) True
isInterior bigBorder ((rectPt:rectBorders), savedRect)
  | isNothing borderPt = {-(trace ("not on a border point: " ++ show rectPt))-} isInterior bigBorder (rectBorders, savedRect)
  | areParallel myBorderDir myRectDir = {-(trace ("on a parallel pt: " ++ show rectPt))-} isInterior bigBorder (rectBorders, savedRect)
  | arePerp myBorderDir myRectDir = (trace ("on a perpendicular point: " ++ show rectPt)) False
  -- at this point, we should only have corner cases (haha) to deal with
  | otherwise = {-(trace ("doing lookahead: " ++ show rectPt))-} (lookAhead bigBorder rectPt (head rectBorders)) && (isInterior bigBorder (rectBorders, savedRect))
    where
      borderPt = getPtFromSet bigBorder rectPt
      myBorderDir = borderDir $ fromJust borderPt
      myRectDir = borderDir rectPt

areParallel :: Char -> Char -> Bool
areParallel a b
  | a == 'C' || b == 'C' = False
  | (a == 'N' || a == 'S') && (b == 'N' || b == 'S') = True
  | (a == 'E' || a == 'W') && (b == 'E' || b == 'W') = True
  | otherwise = False

arePerp :: Char -> Char -> Bool
arePerp a b
  | a == 'C' || b == 'C' = False
  | (a == 'N' || a == 'S') && (b == 'N' || b == 'S') = False
  | (a == 'E' || a == 'W') && (b == 'E' || b == 'W') = False
  | otherwise = True

-- Determine whether a point is in the big shape, given that the point is one away
-- from a corner point on the big shape 
lookAhead :: S.Set Border -> Border -> Border -> Bool
lookAhead bigBorder intersectPt ptToCheck
  | isJust borderPt = {-(trace ("  there is an adjacent border point. ptToCheck: " ++ show ptToCheck ++ ", adj pt: " ++ show borderPt))-} True
  | otherwise = {-(trace ("  checking ccw turn from " ++ show adjacentBorderPts ++ " to " ++ show ptToCheck))-} isCcwTurn (borderDir adjacentBorderPts) (borderDir ptToCheck)
    where
      borderPt = getPtFromSet bigBorder ptToCheck
      adjacentBorderPts = head . filter (\x -> borderDir x /= borderDir ptToCheck) . map fromJust . filter isJust $ map (getPtFromSet bigBorder) (allDirs intersectPt)

allDirs :: Border -> [Border]
allDirs (Border x y dir) = [Border (x + i) (y) dir | i <- [-1, 1]] ++ [Border (x) (y + i) dir | i <- [-1, 1]]

isCcwTurn :: Char -> Char -> Bool
isCcwTurn d1 d2
  | d1 == 'N' = d2 == 'W'
  | d1 == 'W' = d2 == 'S'
  | d1 == 'S' = d2 == 'E'
  | d1 == 'E' = d2 == 'N'
  | otherwise = False

getPtFromSet :: S.Set Border -> Border -> Maybe Border
getPtFromSet borderSet (Border x y dir) =
  if null lst then Nothing else Just (head lst)
    where
      lst = S.toList $ S.intersection borderSet
                        (S.fromList [(Border x y newDir) | newDir <- ['N', 'E', 'S', 'W', 'C']])

{-
  At a crossing:
    - If the one from the big border is NOT a corner and the rectangle one is NOT a corner,
      then return false
    - If the one from the big border is NOT a corner and the rectangle one is a corner,
      then check if the next point in the rectangle is in the interior. If there is
      no next point, then idk
    - If the one from the big border is a corner and the rectangle one is NOT a corner,
      then look on either side of the rectangle border in the direction that line is
      going. One of these points should be on the border, and the other one we can
      tell whether it's interior or exterior based on the corner directions
    - If the one from the big border is a corner and the rectangle one is a corner,
      then treat it as if the one from the big border isn't a corner
-}

{-
  Idea: store the border of the shape in a set. Maybe construct it as a list
  first, and then put them all in a set. Not sure this part will be the bottleneck,
  so it might not matter.
  Then, make a list of (area, p1, p2), and sort by area. Check that the border
  of p1, p2 never crosses the border of the shape at a perpendicular angle (i.e.,
  the borders of our little shape and the big border can match, as long as they
  are both vertical or both horizontal.)
-}
