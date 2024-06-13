buscarResultado :: (Int -> Int -> Int) -> [Int] -> Int -> Bool
buscarResultado _ [] _ = False
buscarResultado _ [x] valor = x == valor
buscarResultado f (x:xs) valor = or [f x (foldl1 f (take i xs)) == valor || buscarResultado f (drop i xs) valor | i <- [1..length xs]]