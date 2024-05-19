package ga;

import org.jgap.Chromosome;
import org.jgap.Configuration;
import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.Genotype;
import org.jgap.IChromosome;
import org.jgap.InvalidConfigurationException;
import org.jgap.impl.DefaultConfiguration;
import org.jgap.impl.IntegerGene;

/**
 * Clase principal para resolver el problema de las 8 reinas usando algoritmos genéticos y el framework JGAP.
 */
public class EightQueensGA {

    /**
     * Método principal.
     * Crear una configuración y genotipo inicial,
     * con el tamaño de la población de configuración para la biblioteca JGAP.
     * Evolucionar el problema.
     *
     * @param args Argumentos para la función principal.
     */
    public static void main(String[] args) throws InvalidConfigurationException {
        // Configuración del algoritmo genético
        Configuration conf = new DefaultConfiguration();
        FitnessFunction myFunc = new EightQueensFitness();
        conf.setFitnessFunction(myFunc);

        Gene[] sampleGenes = new Gene[8];
        for (int i = 0; i < 8; i++) {
            sampleGenes[i] = new IntegerGene(conf, 0, 7); // Los genes son valores de 0 a 7 (posiciones de las reinas en las filas)
        }
        IChromosome sampleChromosome = new Chromosome(conf, sampleGenes);
        conf.setSampleChromosome(sampleChromosome);
        conf.setPopulationSize(100);

        // Inicializar la población
        Genotype population = Genotype.randomInitialGenotype(conf);

        // Evolución del algoritmo genético
        for (int i = 0; i < 50; i++) {
            population.evolve();
        }

        // Obtener la mejor solución encontrada
        IChromosome bestSolutionSoFar = population.getFittestChromosome();
        System.out.println("La mejor solución tiene un valor de fitness de " +
            bestSolutionSoFar.getFitnessValue());

        // Imprimir la mejor solución encontrada
        System.out.println("Posiciones de las reinas (columna para cada fila):");
        int[] positions = new int[8];
        for (int i = 0; i < bestSolutionSoFar.size(); i++) {
            positions[i] = (Integer) bestSolutionSoFar.getGene(i).getAllele();
            System.out.println("Fila " + i + ": Columna " + positions[i]);
        }

        // Imprimir el tablero de ajedrez
        imprimirTablero(positions);

        // Imprimir el número de reinas que se atacan entre sí
        int clashes = 28 - (int) bestSolutionSoFar.getFitnessValue();
        System.out.println("Número de reinas que se atacan entre sí: " + clashes);
    }

    /**
     * Imprime el tablero de ajedrez con las posiciones de las reinas.
     *
     * @param positions Array de enteros que indica la posición de las reinas en cada fila.
     */
    public static void imprimirTablero(int[] positions) {
        for (int i = 0; i < 8; i++) {
            for (int j = 0; j < 8; j++) {
                if (positions[i] == j) {
                    System.out.print("Q ");
                } else {
                    System.out.print(". ");
                }
            }
            System.out.println();
        }
    }
}
