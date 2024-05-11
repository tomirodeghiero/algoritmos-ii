-- Exercise 06
import Data.List (intersect)

--  . Decidir si un conjunto de enteros se puede particionar en dos conjuntos con la misma suma.
subconjuntos :: [a] -> [[a]]
subconjuntos [] = [[]]
subconjuntos (x:xs) = [x:sub | sub <- subconjuntos xs] ++ subconjuntos xs

encontrarParticionDeIgualSuma :: [Int] -> Bool
encontrarParticionDeIgualSuma xs = any esParticionValida [(ys, zs) | ys <- subs, zs <- subs, ys /= zs, nonOverlap ys zs]
    where subs = subconjuntos xs
          esParticionValida (ys, zs) = sum ys == sum zs && (nonOverlap ys zs) && (length ys + length zs == length xs)
          nonOverlap ys zs = null (intersect ys zs)

-- . Dadas dos cadenas, decidir si las mismas son anagramas.
sonAnagramas :: String -> String -> Bool
sonAnagramas s1 s2 = sort s1 == sort s2

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:y:xs)
    | x <= y = x : sort (y:xs)
    | otherwise = y : sort (x:xs)

-- . Dado un número natural n, descomponerlo en sus factores primos.
descomponerEnFactoresPrimos :: Int -> [Int]
descomponerEnFactoresPrimos n = factoresPrimosAux n 2
    where factoresPrimosAux n i
            | n == 1 = []
            | n `mod` i == 0 = i : factoresPrimosAux (n `div` i) i
            | otherwise = factoresPrimosAux n (i + 1)

-- . Dadas dos cadenas p y s, decida si p es subcadena de s.
esSubcadena :: String -> String -> Bool
esSubcadena p s = any (esPrefijo p) (sufijos s)
    where esPrefijo p s = p == take (length p) s
          sufijos s = [drop i s | i <- [0..length s]]

-- . Dadas dos cadenas p y s, decida si p es subsecuencia de s (los elementos no necesariamente tienen que aparecer contiguos en s).
esSubsecuencia :: String -> String -> Bool
esSubsecuencia [] _ = True
esSubsecuencia _ [] = False
esSubsecuencia (x:xs) (y:ys)
    | x == y    = esSubsecuencia xs ys
    | otherwise = esSubsecuencia (x:xs) ys