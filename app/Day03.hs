import qualified Data.Map as Map

main :: IO ()
main = do
  -- part 1
  let idx = input - 1 -- squares start counting at 1
   in print . distance $ grid !! idx
  -- part 2
  let weights = Map.singleton (0, 0) 1 -- first square gets weight 1
   in print . findWeight (tail grid) weights $ input
  where
    input = 277678
    grid = concatMap getCoords [0 ..]

type Point = (Int, Int)

type Weights = Map.Map Point Int

getCoords :: Int -> [Point]
getCoords 0 = [(0, 0)]
getCoords r =
  [(r, - r + 1 + dy) | dy <- [0 .. 2 * r -1]] -- bottom right +1 on y to top right
    ++ [(r - dx, r) | dx <- diffs] -- top right to top left
    ++ [(- r, r - dy) | dy <- diffs] -- top left to bottom left
    ++ [(- r + dx, - r) | dx <- diffs] -- bottom left to bottom right
  where
    diffs = [1 .. 2 * r]

distance :: Point -> Int
distance (x, y) = abs x + abs y

addWeight :: Weights -> Point -> (Int, Weights)
addWeight weights point = (weight, Map.insert point weight weights)
  where
    neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], not (dx == 0 && dy == 0)]
    weight = sum . map (flip (Map.findWithDefault 0) weights) . neighbours $ point

findWeight :: [Point] -> Weights -> Int -> Int
findWeight [] _ _ = error "the grid should be infinite"
findWeight (point : points) weights target =
  let (weight, weights') = addWeight weights point
   in if weight > target then weight else findWeight points weights' target
