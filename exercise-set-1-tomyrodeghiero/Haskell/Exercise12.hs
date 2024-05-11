type Point = (Double, Double)

crossProduct :: Point -> Point -> Point -> Double
crossProduct (x1, y1) (x2, y2) (x3, y3) = 
    (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)

isLeftTurn :: Point -> Point -> Point -> Bool
isLeftTurn p1 p2 p3 = crossProduct p1 p2 p3 > 0

isRightTurn :: Point -> Point -> Point -> Bool
isRightTurn p1 p2 p3 = crossProduct p1 p2 p3 < 0

isCollinear :: Point -> Point -> Point -> Bool
isCollinear p1 p2 p3 = crossProduct p1 p2 p3 == 0

isOnHull :: Point -> Point -> [Point] -> Bool
isOnHull p1 p2 points = all (\p -> not (isLeftTurn p1 p2 p) || isCollinear p1 p2 p) points

bruteForceConvexHull :: [Point] -> [Point]
bruteForceConvexHull points = 
    [(x, y) | (x, y) <- points, z <- points, (x, y) /= z, isOnHull (x, y) z (filter (/= (x, y)) points)]

main :: IO ()
main = do
    let points = [(0.0, 0.0), (1.0, 1.0), (2.0, 2.0), (0.0, 3.0),
                  (3.0, 0.0), (1.5, 1.5), (2.0, 1.0), (1.0, 2.0), 
                  (3.0, 3.0)]
    print $ bruteForceConvexHull points