public class Exercise03 {
    public static boolean findElementInMatrix(int[][] matrix, int element) {
        if (matrix == null || matrix.length == 0 || matrix[0].length == 0) {
            return false;
        }

        int n = matrix.length;
        int row = 0;
        int col = n - 1;

        while (row < n && col >= 0) {
            if (matrix[row][col] == element) {
                return true;
            } else if (matrix[row][col] > element) {
                col--;
            } else {
                row++;
            }
        }

        return false;
    }

    public static void main(String[] args) {
        int[][] matrix = {
                { 1, 2, 3 },
                { 6, 7, 8, },
                { 12, 13, 14 }
        };

        int element = 29; // Elemento a buscar
        System.out.println("El elemento " + element + " está en la matriz: " + findElementInMatrix(matrix, element));
    }

}