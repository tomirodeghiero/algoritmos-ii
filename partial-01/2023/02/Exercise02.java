import java.util.List;

public class Exercise02 {

    public static void quickSort(int[] array, int left, int right) {
        if (left < right) {
            int pivot = partition(array, left, right);
            quickSort(array, left, pivot - 1);
            quickSort(array, pivot + 1, right);
        }
    }

    private static int partition(int[] array, int left, int right) {
        int pivot = array[right];
        int i = left - 1;

        for (int j = left; j < right; j++) {
            if (array[j] < pivot) {
                i++;
                swap(array, i, j);
            }
        }

        swap(array, i + 1, right);
        return i + 1;
    }

    private static void swap(int[] array, int i, int j) {
        int temp = array[j];
        array[j] = array[i];
        array[i] = temp;
    }

    public static List<Integer> encontrarInterseccion(int[] A, int[] B) {
        quickSort(A, 0, A.length - 1);
        quickSort(B, 0, B.length - 1);

        int i = 0, j = 0;
        List<Integer> intersection = new java.util.ArrayList<>();

        while (i < A.length && j < B.length) {
            if (A[i] == B[j]) {
                intersection.add(A[i]);
                i++;
                j++;
            } else if (A[i] < B[j]) {
                i++;
            } else {
                j++;
            }
        }

        return intersection;
    }

    public static void main(String[] args) {
        int[] A = { 1, 3, 4, 5, 7 };
        int[] B = { 2, 3, 5, 6 };

        List<Integer> intersection = encontrarInterseccion(A, B);
        System.out.println("La intersección de A y B es: " + intersection);
    }
}