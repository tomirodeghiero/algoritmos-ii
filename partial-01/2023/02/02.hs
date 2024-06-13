quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
  where
    left = [a | a <- xs, a <= x]
    right = [b | b <- xs, b > x]

encontrarInterseccion :: (Ord a) => [a] -> [a] -> [a]
encontrarInterseccion a b = intersectarOrdenado (quickSort a) (quickSort b)
  where
    intersectarOrdenado [] _ = []
    intersectarOrdenado _ [] = []
    intersectarOrdenado (x:xs) (y:ys)
      | x == y = x : intersectarOrdenado xs ys
      | x < y = intersectarOrdenado xs (y:ys)
      | otherwise = intersectarOrdenado (x:xs) ys