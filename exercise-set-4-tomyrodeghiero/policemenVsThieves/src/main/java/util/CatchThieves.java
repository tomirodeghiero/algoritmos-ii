package util;

/**
 * Greedy Policemen catch thieves.
 */
public final class CatchThieves {
  /**
   * Returns the maximum number of thieves that can be caught.
   * Each policeman can catch only one thief which is at most k units away from
   * him.
   *
   * @param seqTP     is the sequence of thieves and policemen, 't' for thieves
   *                  and 'p' for policemen.
   * @param distanceK represents the maximum units away a policeman can catch a
   *                  thief.
   * @return the maximum number of thieves that can be caught.
   */
  public final int maxCatch(final char[] seqTP, final int distanceK) {
    if (seqTP == null) {
      throw new IllegalArgumentException("La secuencia de Ladrones y Policías no puede ser null."); // Validación de entrada
    }
    if (distanceK < 0) {
      throw new IllegalArgumentException("La distancia K no puede ser negativa."); // Validación de entrada
    }

    int thiefCount = 0; // Contador para el número de ladrones atrapados
    boolean[] caught = new boolean[seqTP.length]; // Array para rastrear a los ladrones atrapados

    // Iterar sobre la secuencia para encontrar policías
    for (int i = 0; i < seqTP.length; i++) {
      if (seqTP[i] == 'P') { // Si se encuentra un policía
        // Comprobar si hay ladrones dentro del alcance K antes y después del policía
        for (int j = Math.max(0, i - distanceK); j <= Math.min(seqTP.length - 1, i + distanceK); j++) {
          if (seqTP[j] == 'T' && !caught[j]) { // Si se encuentra un ladrón no atrapado
            caught[j] = true; // Marcar al ladrón como atrapado
            thiefCount++; // Incrementar el contador de ladrones atrapados
            break; // Pasar al siguiente policía una vez que se atrape a un ladrón
          }
        }
      }
    }

    return thiefCount; // Devolver el número máximo de ladrones atrapados
  }

  public static void main(String[] args) {
    CatchThieves ct = new CatchThieves();
    char[] seqTP = { 'P', 'T', 'T', 'P', 'T' }; // Secuencia de policías y ladrones
    int distanceK = 1; // Distancia máxima a la que un policía puede atrapar a un ladrón
    System.out.println("Número máximo de ladrones atrapados: " + ct.maxCatch(seqTP, distanceK)); // Imprimir resultado
  }
}
