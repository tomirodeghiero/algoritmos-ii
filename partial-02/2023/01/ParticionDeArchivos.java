import java.util.Arrays;

public class ParticionDeArchivos {
    static int maximizarArchivos(int[] tamanhos, int capacidad) {
        // Ordenar los archivos por tamaño en orden ascendente
        Arrays.sort(tamanhos);

        int numArchivos = 0;
        int espacioUsado = 0;

        // Añadir archivos mientras la capacidad lo permita
        for (int tam : tamanhos) {
            if (espacioUsado + tam <= capacidad) {
                espacioUsado += tam;
                numArchivos++;
            } else {
                break;
            }
        }

        return numArchivos;
    }

    public static void main(String[] args) {
        int[] tamanhos = {4, 8, 1, 2, 10, 3};
        int capacidad = 10;

        int maxArchivos = maximizarArchivos(tamanhos, capacidad);
        System.out.println("Máximo número de archivos que se pueden cargar: " + maxArchivos);
    }
}
