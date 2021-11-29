import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

main :: IO ()
main = do
  jumps <- getContents
  print . run modifyJump $ jumps
  print . run modifyJump' $ jumps
  where
    run nextJump = simulate 0 0 nextJump . Map.fromList . zip [0 ..] . map read . lines

simulate :: Int -> Int -> (Int -> Int) -> IntMap Int -> Int
simulate pc steps nextJump prog = case Map.lookup pc prog of
  Just jump ->
    let pc' = pc + jump
        steps' = steps + 1
        jump' = nextJump jump
        prog' = Map.insert pc jump' prog
     in simulate pc' steps' nextJump prog'
  Nothing -> steps

modifyJump :: Int -> Int
modifyJump = (+ 1)

modifyJump' :: Int -> Int
modifyJump' jump
  | jump >= 3 = jump - 1
  | otherwise = jump + 1
