data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show)

listaEnArbol :: (Ord a) => [a] -> Tree a -> Bool
listaEnArbol [] Nil = True
listaEnArbol [] _ = False
listaEnArbol _ Nil = False
listaEnArbol (x:xs) (Node hi r hd)
    | x == r = listaEnArbol xs (Node hi r hd)
    | x < r = listaEnArbol (x:xs) hi
    | x > r = listaEnArbol (x:xs) hd