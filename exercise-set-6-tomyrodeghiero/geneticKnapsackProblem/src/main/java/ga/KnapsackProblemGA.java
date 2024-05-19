package ga;

import org.jgap.Chromosome;
import org.jgap.Gene;
import org.jgap.Genotype;
import org.jgap.IChromosome;
import org.jgap.InvalidConfigurationException;
import org.jgap.Configuration;
import org.jgap.impl.DefaultConfiguration;
import org.jgap.impl.IntegerGene;

/**
 * Main class to call to GA Library.
 */
public class KnapsackProblemGA {

    /**
	 * Main method.
	 * Create a Configuration and Initial Genotype,
   * with the population size of configuration for JGAP Library.
	 * Evolve the problem.
	 * @param args Arguments for the main function.
     */
    public static void main(String[] args) throws InvalidConfigurationException {
        // Definir los parámetros del problema
        int maxCapacity = 15; // Capacidad máxima de la mochila
        int[] weights = {2, 5, 10, 5}; // Pesos de los ítems
        int[] values = {20, 30, 50, 10}; // Valores de los ítems

        KnapsackProblem myKnapsack = new KnapsackProblem(maxCapacity, weights, values);

        // Configuración del algoritmo genético
        Configuration conf = new DefaultConfiguration();
        KnapsackFitness myFunc = new KnapsackFitness(myKnapsack);
        conf.setFitnessFunction(myFunc);

        Gene[] sampleGenes = new Gene[weights.length];
        for (int i = 0; i < weights.length; i++) {
            sampleGenes[i] = new IntegerGene(conf, 0, 1); // Los genes son 0 o 1 (incluir o no el ítem)
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
            System.out.println("Ítem " + i + ": " +
                bestSolutionSoFar.getGene(i).getAllele());
        }
    }
}
