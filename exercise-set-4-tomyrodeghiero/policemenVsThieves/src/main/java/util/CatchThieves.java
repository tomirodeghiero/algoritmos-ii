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
      throw new IllegalArgumentException("Thieves and Police sequence cannot be null.");
    }
    if (distanceK < 0) {
      throw new IllegalArgumentException("Distance cannot be negative.");
    }

    int thiefCount = 0;
    boolean[] caught = new boolean[seqTP.length]; // Para rastrear a los ladrones atrapados

    for (int i = 0; i < seqTP.length; i++) {
      if (seqTP[i] == 'P') {
        // Compruebar si hay ladrones dentro del alcance K antes y después del policía
        for (int j = Math.max(0, i - distanceK); j <= Math.min(seqTP.length - 1, i + distanceK); j++) {
          if (seqTP[j] == 'T' && !caught[j]) {
            caught[j] = true;
            thiefCount++;
            break; // Pasar al siguiente policía una vez que atrapen a un ladrón
          }
        }
      }
    }

    return thiefCount;
  }

  public static void main(String[] args) {
    CatchThieves ct = new CatchThieves();
    char[] seqTP = { 'P', 'T', 'T', 'P', 'T' };
    int distanceK = 1;
    System.out.println("Número máximo de ladrones atrapados: " + ct.maxCatch(seqTP, distanceK));
  }
}
