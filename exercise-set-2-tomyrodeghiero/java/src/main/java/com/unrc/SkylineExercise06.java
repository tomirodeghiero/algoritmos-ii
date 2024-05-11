package com.unrc;

import java.util.ArrayList;
import java.util.List;

public class SkylineExercise06 {
    static class Building {
        int xStart, xEnd, height;

        public Building(int left, int height, int right) {
            this.xStart = left;
            this.height = height;
            this.xEnd = right;
        }
    }

    static class Point {
        int x, height;

        public Point(int x, int height) {
            this.x = x;
            this.height = height;
        }
    }

    public List<Point> getSkyline(Building[] buildings) {
        return getSkyline(buildings, 0, buildings.length - 1);
    }

    private List<Point> getSkyline(Building[] buildings, int start, int end) {
        List<Point> result = new ArrayList<>();

        if (start > end) {
            return result;
        } else if (start == end) {
            result.add(new Point(buildings[start].xStart, buildings[start].height));
            result.add(new Point(buildings[start].xEnd, 0));
            return result;
        }

        int mid = start + (end - start) / 2;
        List<Point> leftSkyline = getSkyline(buildings, start, mid);
        List<Point> rightSkyline = getSkyline(buildings, mid + 1, end);
        return mergeSkylines(leftSkyline, rightSkyline);
    }

    private List<Point> mergeSkylines(List<Point> left, List<Point> right) {
        int leftHeight = 0, rightHeight = 0;
        int currentHeight = 0;
        int leftIndex = 0, rightIndex = 0;
        List<Point> result = new ArrayList<>();

        while (leftIndex < left.size() && rightIndex < right.size()) {
            Point leftPoint = left.get(leftIndex);
            Point rightPoint = right.get(rightIndex);

            // Determine which point to process next
            if (leftPoint.x < rightPoint.x) {
                leftHeight = leftPoint.height;
                currentHeight = Math.max(leftHeight, rightHeight);
                result.add(new Point(leftPoint.x, currentHeight));
                leftIndex++;
            } else if (leftPoint.x > rightPoint.x) {
                rightHeight = rightPoint.height;
                currentHeight = Math.max(leftHeight, rightHeight);
                result.add(new Point(rightPoint.x, currentHeight));
                rightIndex++;
            } else {
                leftHeight = leftPoint.height;
                rightHeight = rightPoint.height;
                currentHeight = Math.max(leftHeight, rightHeight);
                result.add(new Point(leftPoint.x, currentHeight));
                leftIndex++;
                rightIndex++;
            }
        }

        // Add remaining points from the left skyline
        while (leftIndex < left.size()) {
            result.add(left.get(leftIndex++));
        }

        // Add remaining points from the right skyline
        while (rightIndex < right.size()) {
            result.add(right.get(rightIndex++));
        }

        return result;
    }

    public static void main(String[] args) {
        Building[] buildings = {
                new Building(2, 9, 10),
                new Building(3, 7, 15),
                new Building(5, 12, 12),
                new Building(15, 20, 10),
                new Building(19, 24, 8)
        };

        SkylineExercise06 solution = new SkylineExercise06();
        List<Point> skyline = solution.getSkyline(buildings);
        for (Point point : skyline) {
            System.out.println("(" + point.x + ", " + point.height + ")");
        }
    }
}
