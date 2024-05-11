-- Find the minimum and largest number in a list of integers with Divide & Conquer
findMinimumAndLargest :: [Int] -> (Int, Int)
findMinimumAndLargest [] = error "The list cannot be empty"
findMinimumAndLargest [x] = (x, x)
findMinimumAndLargest xs = (min leftMin rightMin, max leftMax rightMax)
    where
        half = length xs `div` 2
        (leftMin, leftMax) = findMinimumAndLargest (take half xs)
        (rightMin, rightMax) = findMinimumAndLargest (drop half xs)

-- Find the minimum and largest number in a list of integers with Decrease & Conquer
findMinimumAndLargest' :: [Int] -> (Int, Int)
findMinimumAndLargest' [] = error "The list cannot be empty"
findMinimumAndLargest' [x] = (x, x)
findMinimumAndLargest' (x:xs) = (min x minTail, max x maxTail)
    where
        (minTail, maxTail) = findMinimumAndLargest' xs

-- Teachers Resolution
minmax :: [Int] -> (Int, Int)
minmax [] = error "The list cannot be empty"
minmax [x] = (x, x)
minmax (x:(y:[])) = (min x y, max x y)
minmax (x:xs) = (min x a, max x b)
    where
        (a, b) = minmax xs