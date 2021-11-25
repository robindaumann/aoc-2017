import Split (splitOn)

main :: IO ()
main = do
  nums <- getContents
  solve subMinMax nums
  solve divMod0 nums
  where
    solve f = print . sum . map (f . map read . splitOn '\t') . lines

subMinMax :: [Int] -> Int
subMinMax nums = maximum nums - minimum nums

divMod0 :: [Int] -> Int
divMod0 nums = head [x `div` y | x <- nums, y <- nums, x `mod` y == 0, x /= y]
