import Data.List(nub, sort)

main :: IO()
main = do
    pws <- getContents
    print . countValid (not . hasDuplicates) $ pws
    print . countValid (not . hasAnagrams) $ pws
    where countValid f pws = length . filter f . map words . lines $ pws

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates l = nub l /= l

hasAnagrams :: Ord a => [[a]] -> Bool
hasAnagrams l = let filtered = nub $ map sort l in length filtered /= length l
