package com.unrc;

public class FindIndexGreatestElementExercise09 {
    // Algorithm to find the index of the greatest element - Brute Force
    public static int findMaxIndexBruteForce(int[] array) {
        if (array == null || array.length == 0) {
            throw new IllegalArgumentException("The array cannot be null or empty.");
        }

        int maxIndex = 0;
        for (int i = 1; i < array.length; i++) {
            if (array[i] > array[maxIndex]) {
                maxIndex = i;
            }
        }

        return maxIndex;
    }

    // Algorithm to find the index of the greatest element - Decrease & Conquer
    public static int findMaxIndexDecreaseConquer(int[] array) {
        if (array == null || array.length == 0) {
            throw new IllegalArgumentException("The array cannot be null or empty.");
        }

        return findMaxIndexDecreaseConquer(array, 0, array[0], 0);
    }

    private static int findMaxIndexDecreaseConquer(int[] array, int currentIndex, int maxValue, int maxIndex) {
        // Base case: if the current index is equal to the length of the array
        if (currentIndex == array.length) {
            return maxIndex;
        }

        // If the current element is greater than the maximum value found so far, update
        // the maximum value and index
        if (array[currentIndex] > maxValue) {
            maxValue = array[currentIndex];
            maxIndex = currentIndex;
        }

        return findMaxIndexDecreaseConquer(array, currentIndex + 1, maxValue, maxIndex);
    }

    public static void main(String[] args) {
        int[] array = { 1000, 3, 5, 7, 6, 4, 21, 100 };
        System.out.println("Max index (Brute Force): " + findMaxIndexBruteForce(array));
        System.out.println("Max index (Decrease & Conquer): " + findMaxIndexDecreaseConquer(array));
    }
}
