import Data.List (sortBy)
import Data.Ord (comparing)

type Point = (Double, Double)

-- Calculate the distance between two points
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- Finds the closest pair of points in a list of points
closestPair :: [Point] -> ((Point, Point), Double)
closestPair points
    | length points <= 3 = bruteForceClosest points
    | otherwise = 
        let sortedX = sortBy (comparing fst) points
            mid = length points `div` 2
            (left, right) = splitAt mid sortedX
            (pairL, distL) = closestPair left
            (pairR, distR) = closestPair right
            delta = min distL distR
            strip = stripClosest sortedX (fst (left !! mid)) delta
            (pairS, distS) = closestStrip strip delta
        in minimumBy (comparing snd) [(pairL, distL), (pairR, distR), (pairS, distS)]

-- Brute force approach to find the closest pair in a small list of points in the plane
bruteForceClosest :: [Point] -> ((Point, Point), Double)
bruteForceClosest points =
    minimumBy (comparing snd) [((p1, p2), distance p1 p2) | p1 <- points, p2 <- points, p1 /= p2]

-- Find the closest pair of points in the strip
closestStrip :: [Point] -> Double -> ((Point, Point), Double)
closestStrip strip delta = 
    let sortedY = sortBy (comparing snd) strip
        pairs = [((p1, p2), distance p1 p2) | p1 <- sortedY, p2 <- sortedY, p1 /= p2, snd p2 - snd p1 < delta]
    in if null pairs then error "No closest pair in the strip" else minimumBy (comparing snd) pairs

-- Generates a list of points in the strip that are within delta of the dividing line
stripClosest :: [Point] -> Double -> Double -> [Point]
stripClosest points mid delta = [p | p <- points, abs (fst p - mid) < delta]

-- Utility function to find minimum by a given comparator
minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ [x] = x
minimumBy comp (x:xs) = 
    let y = minimumBy comp xs
    in case comp x y of
        LT -> x
        _ -> y