module Split(splitOn) where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s = case dropWhile (== c) s of
  [] -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'