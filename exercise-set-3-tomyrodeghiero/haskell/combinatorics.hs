comb :: Int -> Int -> Int
comb n m
    | n < 0 || m < 0 = error "error"
    | m > n = 0
    | m == 0 || n == m = 1
    | otherwise = (comb (n-1) (m-1)) + (comb (n-1) m)