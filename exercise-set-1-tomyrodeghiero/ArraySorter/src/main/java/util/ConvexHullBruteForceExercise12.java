package util;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

public class ConvexHullBruteForceExercise12 {

    public static List<Point2D> bruteForceConvexHull(Point2D[] points) {
        List<Point2D> convexHull = new ArrayList<>();

        for (int i = 0; i < points.length; i++) {
            for (int j = i + 1; j < points.length; j++) {
                int ijSide = 0;
                boolean isEdge = true;

                for (int k = 0; k < points.length; k++) {
                    if (k == i || k == j)
                        continue;
                    int crossProduct = crossProductSign(points[i], points[j], points[k]);
                    if (ijSide == 0) {
                        ijSide = crossProduct;
                    } else if (crossProduct != 0 && crossProduct != ijSide) {
                        isEdge = false;
                        break;
                    }
                }

                if (isEdge) {
                    if (!convexHull.contains(points[i]))
                        convexHull.add(points[i]);
                    if (!convexHull.contains(points[j]))
                        convexHull.add(points[j]);
                }
            }
        }

        return convexHull;
    }

    private static int crossProductSign(Point2D a, Point2D b, Point2D c) {
        double crossProduct = (b.getX() - a.getX()) * (c.getY() - a.getY())
                - (b.getY() - a.getY()) * (c.getX() - a.getX());
        if (crossProduct > 0) {
            return 1;
        } else if (crossProduct < 0) {
            return -1;
        } else {
            return 0;
        }
    }

    public static void main(String[] args) {
        Point2D[] points = {
                new Point2D.Double(0.0, 0.0),
                new Point2D.Double(1.0, 1.0),
                new Point2D.Double(2.0, 2.0),
                new Point2D.Double(0.0, 3.0),
                new Point2D.Double(3.0, 0.0),
                new Point2D.Double(1.5, 1.5),
                new Point2D.Double(2.0, 1.0),
                new Point2D.Double(1.0, 2.0),
                new Point2D.Double(3.0, 3.0)
        };

        List<Point2D> convexHull = bruteForceConvexHull(points);
        for (Point2D p : convexHull) {
            System.out.println(p);
        }
    }

}
