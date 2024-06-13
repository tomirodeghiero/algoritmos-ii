public class Exercise01 {
    public static int indiceIgualAElemento(int[] array, int left, int right) {
        if (left > right) {
            return -1;
        }

        int middle = (left + right) / 2;

        if (array[middle] == middle) {
            return middle;
        } else if (array[middle] < middle) {
            return indiceIgualAElemento(array, middle + 1, right);
        } else {
            return indiceIgualAElemento(array, left, middle - 1);
        }
    }

    public static void main(String[] args) {
        int[] A = { -10, 1, 5, 8, 9 };
        int fixedPoint = indiceIgualAElemento(A, 0, A.length - 1);
        System.out.println(
                "El elemento con el mismo valor al índice es: " + (fixedPoint >= 0 ? fixedPoint : "No existe"));
    }
}