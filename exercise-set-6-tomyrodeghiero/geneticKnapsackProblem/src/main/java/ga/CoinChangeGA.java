package ga;

import org.jgap.Chromosome;
import org.jgap.Configuration;
import org.jgap.FitnessFunction;
import org.jgap.Gene;
import org.jgap.Genotype;
import org.jgap.IChromosome;
import org.jgap.impl.DefaultConfiguration;
import org.jgap.impl.IntegerGene;

/**
 * Clase principal para resolver el problema de dar cambio usando algoritmos genéticos y el framework JGAP.
 */
public class CoinChangeGA {

    /**
     * Función de fitness específica para el problema de dar cambio.
     * Evalúa la calidad de un cromosoma basado en qué tan cerca está de la cantidad deseada C
     * y la cantidad de monedas usadas.
     */
    public static class CoinFitnessFunction extends FitnessFunction {
        private final int C; // Cantidad de centavos a cambiar
        private final int[] d; // Denominaciones de las monedas

        /**
         * Constructor para la función de fitness.
         *
         * @param C Cantidad de centavos a cambiar
         * @param d Denominaciones de las monedas
         */
        public CoinFitnessFunction(int C, int[] d) {
            this.C = C;
            this.d = d;
        }

        /**
         * Método para evaluar la aptitud (fitness) de un cromosoma.
         *
         * @param chromosome El cromosoma a evaluar
         * @return El valor de fitness del cromosoma
         */
        @Override
        protected double evaluate(IChromosome chromosome) {
            int sum = 0;
            int numCoins = 0;

            // Calcular la suma total y la cantidad de monedas usadas
            for (int i = 0; i < chromosome.size(); i++) {
                int num = (Integer) chromosome.getGene(i).getAllele();
                sum += num * d[i];
                numCoins += num;
            }

            // Diferencia entre la cantidad deseada y la suma obtenida
            int diff = Math.abs(C - sum);
            // El fitness es la diferencia más la cantidad de monedas usadas
            return diff + numCoins;
        }
    }

    /**
     * Método principal para ejecutar el algoritmo genético.
     *
     * @param args Argumentos de línea de comandos
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        // Definir el número de centavos a cambiar
        int C = 78;
        // Definir las denominaciones de las monedas y sus cantidades máximas
        int[] d = {25, 10, 5, 1};
        int[] M = {3, 5, 10, 20};

        // Configuración del algoritmo genético
        Configuration conf = new DefaultConfiguration();
        FitnessFunction myFunc = new CoinFitnessFunction(C, d);
        conf.setFitnessFunction(myFunc);

        // Definir la estructura de los cromosomas (genes)
        Gene[] sampleGenes = new Gene[d.length];
        for (int i = 0; i < d.length; i++) {
            sampleGenes[i] = new IntegerGene(conf, 0, M[i]);
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
        for (int i = 0; i < bestSolutionSoFar.size(); i++) {
            System.out.println("Monedas de " + d[i] + " centavos: " +
                bestSolutionSoFar.getGene(i).getAllele());
        }
    }
}
