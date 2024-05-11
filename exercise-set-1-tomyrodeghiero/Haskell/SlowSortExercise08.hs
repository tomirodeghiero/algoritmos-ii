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

main :: IO ()
main = do
    let listaDesordenada = [3, 1, 4, 1, 5, 9, 2, 6, 5]
    print (slowSort listaDesordenada)