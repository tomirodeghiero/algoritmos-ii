-- Verify if the first list is a contiguous subsequence of the second list
isContiguousSubsequence :: Eq a => [a] -> [a] -> Bool
isContiguousSubsequence [] _ = True
isContiguousSubsequence _ [] = False
isContiguousSubsequence (x:xs) (y:ys)
    | x == y = isPrefix xs ys
    | otherwise = isContiguousSubsequence (x:xs) ys

-- Verify if the first list is a prefix of the second list
isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys