import Data.Char (digitToInt)

main :: IO ()
main = do
  nums <- getContents
  solve sumRepetitions nums
  solve sumRepetitions' nums
  where
    solve f = print . f . map digitToInt . init

sumRepetitions :: (Eq a, Num a) => [a] -> a
sumRepetitions nums = fst . foldl match (0, last nums) $ nums
  where
    match (acc, prev) n = (if prev == n then acc + n else acc, n)

sumRepetitions' :: (Eq a, Num a) => [a] -> a
sumRepetitions' nums = foldl match 0 . zip [0 ..] $ nums
  where
    len = length nums
    atOffset idx = nums !! ((idx + len `div` 2) `mod` len)
    match acc (idx, n) = if atOffset idx == n then acc + n else acc
