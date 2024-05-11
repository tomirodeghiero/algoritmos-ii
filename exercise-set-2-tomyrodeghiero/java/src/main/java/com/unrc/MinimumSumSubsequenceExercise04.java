package com.unrc;

public class MinimumSumSubsequenceExercise04 {
    /**
     * Method that calculates the minimum sum of a subsequence of an array.
     * 
     * @param array The array.
     * @return The minimum sum of a subsequence of the array.
     * @throws IllegalArgumentException If the array is null or empty.
     */
    public static int minimumSumSubsequence(int[] array) {
        if (array == null || array.length == 0) {
            throw new IllegalArgumentException("The array cannot be null or empty.");
        }

        // Check if all values are positive
        for (int value : array) {
            if (value < 0) {
                // If any value is negative, use Divide and Conquer
                return foundMinimumSumSubsequence(array, 0, array.length - 1);
            }
        }

        // If all values are positive, the minimum sum is 0 (the empty subsequence)
        return 0;
    }

    public static int foundMinimumSumSubsequence(int[] array, int left, int right) {
        // Base case: if the array has 1 element, the minimum sum is the element itself
        if (left == right) {
            return array[left];
        }

        // Divide: found the middle of the array
        int middle = (left + right) / 2;

        // Conquer: found the minimum sum of the left and right subarrays
        int leftSum = foundMinimumSumSubsequence(array, left, middle);
        int rightSum = foundMinimumSumSubsequence(array, middle + 1, right);
        int crossingSum = foundCrossingSum(array, left, middle, right);

        return Math.min(crossingSum, Math.min(leftSum, rightSum));
    }

    public static int foundCrossingSum(int[] array, int left, int middle, int right) {
        int leftMinSum = Integer.MAX_VALUE;
        int sum = 0;
        for (int i = middle; i >= left; i--) {
            sum += array[i];
            if (sum < leftMinSum) {
                leftMinSum = sum;
            }
        }

        int rightMinSum = Integer.MAX_VALUE;
        sum = 0;
        for (int i = middle + 1; i <= right; i++) {
            sum += array[i];
            if (sum < rightMinSum) {
                rightMinSum = sum;
            }
        }

        // Return the sum of the left and right subarrays
        return leftMinSum + rightMinSum;
    }

    public static void main(String[] args) {
        int[] array = { -4, -70, -13, 1, 7, 5 };
        int minSum = foundMinimumSumSubsequence(array, 0, array.length - 1);
        System.out.println("Minimum sum of a subsequence: " + minSum);
    }
}
