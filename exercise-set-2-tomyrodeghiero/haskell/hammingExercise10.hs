hammingDistance :: String -> String -> Int
hammingDistance [] [] = 0
hammingDistance xs [] = length xs
hammingDistance [] ys = length ys
hammingDistance (x:xs) (y:ys)
    | x /= y = 1 + hammingDistance xs ys
    | otherwise = hammingDistance xs ys