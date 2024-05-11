-- Solucion fuerza bruta
maxindexFB :: [Int] -> Int
maxindexFB [] = error "The list cannot be empty"
maxindexFB (x:xs) = maxIndexHelper xs x 1 0

maxIndexHelper :: [Int] -> Int -> Int -> Int -> Int
maxIndexHelper [] _ _ maxIndex = maxIndex
maxIndexHelper (x:xs) maxVal index maxIndex
    | x > maxVal = maxIndexHelper xs x (index + 1) index
    | otherwise = maxIndexHelper xs maxVal (index + 1) maxIndex

-- maxindex retorna la posicion del maximo elemento de la lista
-- Solucion Decrease & Conquer
maxIndexDc :: [Int] -> Int
maxIndexDc [] = error "The list cannot be empty"
maxIndexDc xs = maxIndexDcHelper xs 0 (head xs) 0

maxIndexDcHelper :: [Int] -> Int -> Int -> Int -> Int
maxIndexDcHelper [] maxIndex _ _ = maxIndex
maxIndexDcHelper (x:xs) maxIndex maxVal currentIndex
    | x > maxVal = maxIndexDcHelper xs currentIndex x (currentIndex + 1)
    | otherwise = maxIndexDcHelper xs maxIndex maxVal (currentIndex + 1)