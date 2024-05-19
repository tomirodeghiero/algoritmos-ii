package ga;

import org.jgap.FitnessFunction;
import org.jgap.IChromosome;

/**
 * This class is used for calculate the fitness value.
 *
 */
public class KnapsackFitness extends FitnessFunction {

    /**
   * reference to knapsack instance.
     */
    private KnapsackProblem myKnapsack;

    /**
   * KnapsackFitness constructor.
   * @param knapsack instance.
     */
    public KnapsackFitness(KnapsackProblem knapsack) {
        myKnapsack = knapsack;
    }

    /**
   * Implements the fitness function, how good is the chromosome I am evaluating.
   * @param chromosome Chromosome to evaluate
   * @return fitness value
     */
    @Override
    protected double evaluate(IChromosome chromosome) {
        int totalWeight = 0;
        int totalValue = 0;

        for (int i = 0; i < chromosome.size(); i++) {
            int geneValue = (Integer) chromosome.getGene(i).getAllele();
            totalWeight += geneValue * myKnapsack.getWeights()[i];
            totalValue += geneValue * myKnapsack.getValues()[i];
        }

        if (totalWeight > myKnapsack.getMaxCapacity()) {
            return 0; // Penalizar soluciones que exceden la capacidad
        }

        return totalValue;
    }
}
