data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show)

encontrarRamaMayorMenor :: Tree a -> (Int, Int)
encontrarRamaMayorMenor Nil = (0, 0)
encontrarRamaMayorMenor (Node hi _ hd) = (1 + max largoIzq largoDer, 1 + min cortoIzq cortoDerecha)
  where
    (largoIzq, cortoIzq) = encontrarRamaMayorMenor hi
    (largoDer, cortoDerecha) = encontrarRamaMayorMenor hd