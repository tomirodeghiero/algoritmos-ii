-- 01. Find the element equal to index in a sorted array with Divide & Conquer
indexEqualToElement :: [Int] -> Maybe Int
indexEqualToElement xs =  indexEqualToElement' xs 0 (length xs - 1)

indexEqualToElement' :: [Int] -> Int -> Int -> Maybe Int
indexEqualToElement' xs left right
  | left > right = Nothing
  | middle == xs !! middle = Just middle
  | middle < xs !! middle = indexEqualToElement' xs left (middle - 1)
  | otherwise = indexEqualToElement' xs (middle + 1) right
  where middle = (left + right) `div` 2