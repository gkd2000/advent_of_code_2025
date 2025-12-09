#!/usr/bin/env cabal
{- cabal:
build-depends:
  base ^>=4.18.3.0,
  containers ^>= 0.7,
  pqueue ^>= 1.6.0.0
-}

import System.Environment
import qualified Data.Map as M
import Data.List
import qualified Data.PQueue.Prio.Min as Q
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

type Coord = (Int, Int, Int)
type CompMap = M.Map Coord (S.Set Coord)
type DistanceQueue = Q.MinPQueue Int (Coord, Coord)

_NUMITERATIONS = 1000
_NUMPTS = 1000

main :: IO ()
main = do fileName <- getArgs
          contents <- readFile (head fileName)
          let points = map extractPoint $ lines contents
          let distQueue = queueDistances points
          let componentsMap = makeMap points
          let newComponentsMap = makeConnections _NUMITERATIONS distQueue componentsMap
          let pt1 = product . take 3 . sortBy (flip compare) . map length . dedup . map S.elems $ M.elems newComponentsMap
          let pt2 = let pts = makeAllConnections distQueue componentsMap in (getX $ fst pts) * (getX $ snd pts)
          putStrLn (show pt1)
          putStrLn (show pt2)

getX :: (a, a, a) -> a
getX (x,_,_) = x

dedup :: Ord a => [a] -> [a]
dedup = map head . group . sort

extractPoint :: String -> Coord
extractPoint str = (read x, read y, read $ tail almostZ)
  where
    (x, rest) = span (/= ',') str
    (y, almostZ) = span (/= ',') (tail rest)

queueDistances :: [Coord] -> DistanceQueue
queueDistances pts = go [(getDist p1 p2, p1, p2) | p1 <- pts, p2 <- pts, p1 < p2] Q.empty
  where
    go [] queue = queue
    go ((dist,p1,p2):elts) queue = go elts (Q.insert dist (p1, p2) queue)

getDist :: Coord -> Coord -> Int
getDist (x,y,z) (x',y',z') = (x-x')^2 + (y-y')^2 + (z-z')^2

makeMap :: [Coord] -> CompMap
makeMap pts = go pts M.empty
  where
    go [] componentMap = componentMap
    go (pt:pts) componentMap = go pts (M.insert pt (S.singleton pt) componentMap)

makeConnections :: Int -> DistanceQueue -> CompMap -> CompMap
makeConnections 0 _ components = components
makeConnections n queue components = makeConnections (n - 1) (Q.deleteMin queue) (updateComponents components p1 p2)
  where (minDist, (p1, p2)) = Q.findMin queue

updateComponents :: CompMap -> Coord -> Coord -> CompMap
updateComponents components p1 p2 =
  let newComponent = S.union (fromJust $ M.lookup p1 components) (fromJust $ M.lookup p2 components)
  in go (S.elems newComponent) components newComponent
    where
      go [] curMap _ = curMap
      go (pt:pts) curMap val = go pts (M.insert pt val curMap) val

makeAllConnections :: DistanceQueue -> CompMap -> (Coord, Coord)
makeAllConnections queue components = go queue (Just components) ((0, 0, 0), (0, 0, 0))
  where
    go curQ maybeComponents pts
      | isNothing maybeComponents = pts
      | otherwise = go (Q.deleteMin curQ) (updateComponentsPt2 (fromJust maybeComponents) p1 p2) (p1, p2)
         where (minDist, (p1, p2)) = Q.findMin curQ

updateComponentsPt2 :: CompMap -> Coord -> Coord -> Maybe CompMap
updateComponentsPt2 components p1 p2 =
  let newComponent = S.union (fromJust $ M.lookup p1 components) (fromJust $ M.lookup p2 components)
  in go (S.elems newComponent) components newComponent
    where
      go pts curMap val
        | S.size val == _NUMPTS = Nothing
        | null pts = Just curMap
        | otherwise = go (tail pts) (M.insert (head pts) val curMap) val