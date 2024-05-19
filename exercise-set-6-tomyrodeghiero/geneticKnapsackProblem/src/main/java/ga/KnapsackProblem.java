package ga;


/**
* Knapsack Problem characterization.
 */
public class KnapsackProblem {
    private int maxCapacity;
    private int[] weights;
    private int[] values;

    /**
   * KnapsackProblem constructor.
   * @param maxCapacity maximum capacity allowed.
     */
    public KnapsackProblem(int maxCapacity, int[] weights, int[] values) {
        this.maxCapacity = maxCapacity;
        this.weights = weights;
        this.values = values;
    }

    public int getMaxCapacity() {
        return maxCapacity;
    }

    public int[] getWeights() {
        return weights;
    }

    public int[] getValues() {
        return values;
    }
}
