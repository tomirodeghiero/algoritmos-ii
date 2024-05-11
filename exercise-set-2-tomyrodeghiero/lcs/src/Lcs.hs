--Get subsequences
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

-- Helper function to get the longest list
longest :: [a] -> [a] -> [a]
longest xs ys = if length xs > length ys then xs else ys

--Longest common subsequence using brute force
lcsBf :: Eq a => [a] -> [a] -> [a]
lcsBf xs ys = foldr longest [] [x | x <- subsequences xs, x `elem` subsequences ys]

-- Longest common subsequence using divide and Conquer
lcsDc :: Eq a => [a] -> [a] -> [a]
lcsDc [] _ = []
lcsDc _ [] = []
lcsDc (x:xs) (y:ys)
  | x == y = x : lcsDc xs ys
  | otherwise = longest (lcsDc (x:xs) ys) (lcsDc xs (y:ys))