import java.util.HashMap;
import java.util.Map;

public class MinimumJumps {

    private static Map<Integer, Integer> memo = new HashMap<>();

    public static int minJumps(int[] arr, int l, int h) {
        // Si ya estamos en el final, no necesitamos más saltos.
        if (l == h)
            memo.put(l, 0);

        // Si no es posible avanzar más desde esta posición.
        if (arr[l] == 0)
            memo.put(l, Integer.MAX_VALUE);

        // Si ya hemos calculado este subproblema, lo retornamos de la memoria.
        if (memo.containsKey(l)) {
            return memo.get(l);
        }

        int minJumps = Integer.MAX_VALUE;
        // Probar cada posible movimiento desde el inicio hasta la menor distancia entre
        // el final y el salto máximo.
        for (int i = l + 1; i <= h && i <= l + arr[l]; i++) {
            int jumps = minJumps(arr, i, h);
            if (jumps != Integer.MAX_VALUE && jumps + 1 < minJumps) {
                minJumps = jumps + 1;
            }
        }

        // Almacenamos el resultado calculado en la memoria.
        memo.put(l, minJumps);

        return minJumps;
    }

    public static void main(String[] args) {
        int[] arr = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }; // Ejemplo de arreglo de entrada.
        int result = minJumps(arr, 0, arr.length - 1);
        System.out.println("Mínimo número de saltos requerido es: " + result);
    }
}