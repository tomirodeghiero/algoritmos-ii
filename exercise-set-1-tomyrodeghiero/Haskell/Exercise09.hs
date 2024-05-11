removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

main :: IO ()
main = print $ removeDuplicates [1, 2, 5, 2, 3, 3, 4, 5, 6, 1, 7]
