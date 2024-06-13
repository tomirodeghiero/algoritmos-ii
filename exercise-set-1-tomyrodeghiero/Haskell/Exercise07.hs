import Data.List (sort, isPrefixOf, tails)
import Control.Monad (filterM)

-- Permutaciones
permutations :: [a] -> [[a]]
permutations [] = [[]]  -- Caso base añadido para listas vacías.
permutations [x] = [[x]]  -- Una lista con un elemento tiene una permutación: ella misma.
permutations (x:xs) = concatMap (interleave x) ps
  where ps = permutations xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- Subconjuntos
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets [x] = [[x], []]
subsets (x:xs) = ps ++ [(x:r) | r <- ps]
  where ps = subsets x

subsets' :: [a] -> [[a]]
subsets' [] = [[]]
subsets' (x:xs) = subsets' xs ++ map (x:) (subsets' xs)

-- Sublistas (secuencias contiguas de una lista)
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = [take n (x:xs) | n <- [1..length (x:xs)]] ++ sublists xs

-- Función para verificar si dos cadenas son anagramas
areAnagrams :: Ord a => [a] -> [a] -> Bool
areAnagrams s1 s2 = sort s1 == sort s2

-- Función para verificar si existe un subconjunto con suma específica
hasSubsetWithSumN :: (Num a, Eq a) => [a] -> a -> Bool
hasSubsetWithSumN s n = foundSumEqualN (map sum (subsets s)) n
  where
    foundSumEqualN :: (Eq a, Num a) => [a] -> a -> Bool
    foundSumEqualN [] _ = False  -- No hay más subconjuntos; no se encontró.
    foundSumEqualN (x:xs) n
      | x == n = True   -- Se encontró un subconjunto cuya suma es igual a n.
      | otherwise = foundSumEqualN xs n  -- Sigue buscando en el resto.

-- Función para verificar si una cadena es subcadena de otra
isSubstring :: Eq a => [a] -> [a] -> Bool
isSubstring [] _ = True  -- Una lista vacía es subcadena de cualquier lista.
isSubstring _ [] = False -- Una lista no vacía no puede ser subcadena de una lista vacía.
isSubstring sub (x:xs)
    | startWidth sub (x:xs) = True
    | otherwise = isSubstring sub xs

-- Auxiliar para verificar si la primera lista es prefijo de la segunda.
startWidth :: Eq a => [a] -> [a] -> Bool
startWidth [] _ = True  -- Una lista vacía es prefijo de cualquier lista.
startWidth _ [] = False -- Una lista no vacía no puede ser prefijo de una lista vacía.
startWidth (x:xs) (y:ys) = x == y && startWidth xs ys

-- Función para verificar si una lista es subsecuencia de otra
isSubsequence :: (Eq a) => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys)
    | x == y    = isSubsequence xs ys
    | otherwise = isSubsequence (x:xs) ys

-- Función principal para probar las funciones anteriores
main :: IO ()
main = do
    print $ areAnagrams "listen" "silent" -- Debería retornar True
    print $ hasSubsetWithSumN [1,2,3,4] 6 -- Debería retornar True, ya que 2+4=6
    print $ isSubstring "world" "helloworld" -- Debería retornar True
    print $ isSubsequence "hlo" "helloworld" -- Debería retornar True