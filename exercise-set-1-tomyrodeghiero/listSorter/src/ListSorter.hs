module ListSorter where

-- Test ordered list
isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:(y :ys)) = if x<=y then isOrdered(y:ys)  else False

-- Remove list members that satisfy the predicate
remove :: (a -> Bool) -> [a] -> [a]
remove p [] = []
remove p (x:xs) = if p x then xs else x : remove p xs


-- Sort a list by selectionSort algorithm
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = selectionSort (remove (== x) xs) ++ [x]
  where x = maximum xs


-- Función que implementa SlowSort
slowSort :: (Ord a) => [a] -> [a]
slowSort xs = head (filter isSorted (permutations xs))

-- Función que verifica si una lista de enteros está ordenada
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Permutaciones
permutations :: [a] -> [[a]]
permutations [] = [[]]  -- Caso base añadido para listas vacías.
permutations [x] = [[x]]  -- Una lista con un elemento tiene una permutación: ella misma.
permutations (x:xs) = concatMap (interleave x) ps
  where ps = permutations xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)