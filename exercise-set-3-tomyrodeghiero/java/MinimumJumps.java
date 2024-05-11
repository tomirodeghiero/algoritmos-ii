import java.util.HashMap;
import java.util.Map;

public class MinimumJumps {

    private static Map<Integer, Integer> memo = new HashMap<>();

    public static int minJumps(int[] arr, int start, int end) {
        // Si ya estamos en el final, no necesitamos más saltos.
        if (start == end) {
            return 0;
        }

        // Si no es posible avanzar más desde esta posición.
        if (arr[start] == 0) {
            return Integer.MAX_VALUE;
        }

        // Si ya hemos calculado este subproblema, lo retornamos de la memoria.
        if (memo.containsKey(start)) {
            return memo.get(start);
        }

        int minJumps = Integer.MAX_VALUE;
        // Probar cada posible movimiento desde el inicio hasta la menor distancia entre
        // el final y el salto máximo.
        for (int i = start + 1; i <= end && i <= start + arr[start]; i++) {
            int jumps = minJumps(arr, i, end);
            if (jumps != Integer.MAX_VALUE && jumps + 1 < minJumps) {
                minJumps = jumps + 1;
            }
        }

        // Almacenamos el resultado calculado en la memoria.
        memo.put(start, minJumps);

        return minJumps;
    }

    public static void main(String[] args) {
        int[] arr = { 2, 3, 1, 1, 2, 4, 2, 0, 1, 1 }; // Ejemplo de arreglo de entrada.
        int result = minJumps(arr, 0, arr.length - 1);
        System.out.println("Mínimo número de saltos requerido es: " + result);
    }
}
