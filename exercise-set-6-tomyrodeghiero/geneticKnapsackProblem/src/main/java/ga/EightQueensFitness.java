package ga;

import org.jgap.FitnessFunction;
import org.jgap.IChromosome;

/**
 * Esta clase se usa para calcular el valor de fitness para el problema de las 8 reinas.
 */
public class EightQueensFitness extends FitnessFunction {
    /**
     * Implementa la función de fitness, que evalúa qué tan bueno es el cromosoma.
     *
     * @param chromosome Cromosoma a evaluar.
     * @return Valor de fitness.
     */
    @Override
    protected double evaluate(IChromosome chromosome) {
        int clashes = 0;

        for (int i = 0; i < chromosome.size(); i++) {
            int queenPosition = (Integer) chromosome.getGene(i).getAllele();

            // Verificar si hay otra reina en la misma columna
            for (int j = i + 1; j < chromosome.size(); j++) {
                int otherQueenPosition = (Integer) chromosome.getGene(j).getAllele();

                if (queenPosition == otherQueenPosition) {
                    clashes++;
                }

                // Verificar si hay otra reina en la misma diagonal
                if (Math.abs(queenPosition - otherQueenPosition) == Math.abs(i - j)) {
                    clashes++;
                }
            }
        }

        // El fitness es la cantidad de reinas que no se atacan, se maximiza este valor
        return 28 - clashes; // 28 es el máximo número de pares de reinas que no se atacan
    }
}
