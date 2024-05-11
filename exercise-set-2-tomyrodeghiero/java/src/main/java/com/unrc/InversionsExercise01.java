package com.unrc;

public class InversionsExercise01 {
    private static int mergeSort(int[] array, int left, int right) {
        int amountOfInversions = 0;
        if (left < right) {
            int middle = (left + right) / 2;
            amountOfInversions += mergeSort(array, left, middle);
            amountOfInversions += mergeSort(array, middle + 1, right);
            amountOfInversions += merge(array, left, middle, right);
        }
        return amountOfInversions;
    }

    public static int merge(int[] array, int left, int middle, int right) {
        int amountOfInversions = 0;
        int n1 = middle - left + 1;
        int n2 = right - middle;
        int[] leftArray = new int[n1];
        int[] rightArray = new int[n2];
        for (int i = 0; i < n1; i++) {
            leftArray[i] = array[left + i];
        }
        for (int j = 0; j < n2; j++) {
            rightArray[j] = array[middle + 1 + j];
        }

        int i = 0, j = 0, k = left;
        while (i < n1 && j < n2) {
            if (leftArray[i] <= rightArray[j]) {
                array[k] = leftArray[i];
                i++;
            } else {
                array[k] = rightArray[j];
                j++;
                amountOfInversions += (middle + 1) - (left + i);
            }
            k++;
        }

        while (i < n1) {
            array[k] = leftArray[i];
            i++;
            k++;
        }

        while (j < n2) {
            array[k] = rightArray[j];
            j++;
            k++;
        }
        return amountOfInversions;
    }

    public static void main(String[] args) {
        int[] array = { 0, 1, 4, 3, 2 };
        int amountOfInversions = mergeSort(array, 0, array.length - 1);
        System.out.println("Amount of inversions: " + amountOfInversions);
    }
}