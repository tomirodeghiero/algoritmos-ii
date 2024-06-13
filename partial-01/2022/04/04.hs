quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
  where
    left = [a | a <- xs, a <= x]
    right = [b | b <- xs, b > x]

tieneParConSumaX :: (Num a, Ord a) => [a] -> a -> Bool
tieneParConSumaX s1 x = tieneParConSumaX' 0 (length sortedList - 1) sortedList
  where
    sortedList = quickSort s1
    tieneParConSumaX' i j lista
        | i >= j = False
        | currentSum == x = True
        | currentSum < x = tieneParConSumaX' (i + 1) j lista
        | currentSum > x = tieneParConSumaX' i (j - 1) lista
        where
            currentSum = lista !! i + lista !! j