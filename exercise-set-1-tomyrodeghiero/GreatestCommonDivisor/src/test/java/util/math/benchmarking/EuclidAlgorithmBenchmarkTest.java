package util.math.benchmarking;

import org.junit.Test;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.TimeValue;
import util.math.GreatestCommonDivisor;

import java.util.Random;
import java.util.concurrent.TimeUnit;

/**
 * Runs benchmark on euclid algorithm.
 * This class uses JMH benchmarking framework.
 * @author scilingo
 *
 */
public class EuclidAlgorithmBenchmarkTest {


	/**
	 * Runner for JMH, implemented as a jUnit test.
	 * @throws Exception when parameter options or run fails
	 */
	@Test 
	public void launchBenchmark() throws Exception {

		Options opt = new OptionsBuilder()
				.include(this.getClass().getName() + ".*")
				.mode (Mode.AverageTime)
				.timeUnit(TimeUnit.MICROSECONDS)
				.warmupTime(TimeValue.seconds(1))
				.warmupIterations(2)
				.measurementTime(TimeValue.seconds(1))
				.measurementIterations(2)
				.threads(2)
				.forks(1)
				.shouldFailOnError(true)
				.shouldDoGC(true)
				.build();

		new Runner(opt).run();
	}

	/**
	 * State to be used for benchmarking. 
	 * This is a form of parameterized benchmarking.
	 * Benchmark will be run on all BenchmarkState produced.
	 * 
	 * @author scilingo
	 *
	 */
	@State(Scope.Benchmark)
	public static class BenchmarkState {

		/**
		 * Order of magnitude of the numbers to be used for benchmarking.
		 * Change/extend the param for other order sizes.
		 */
		@Param({"100", "1000", "10000", "1000000"})
		public int order;

		/**
		 * The numbers to be used.
		 */

		public int m;
		public int n;

		/**
		 * Sets test input values with a random values upto given order.
		 */
		@Setup(Level.Trial)
		public void setUp() {

			Random random = new Random();
			m = Math.abs(random.nextInt(order));
			n = Math.abs(random.nextInt(order));
			System.out.println("Random Numbers "+m+" and "+n);

		}
	}


	/**
	 * Benchmark method. Runs euclid algorithm and takes measurements described in 
	 * the tags.
	 * Runs multiple times for each order, taking into account warmup runs and iterations.
	 * @param state is the (parameterized) state on which the benchmark method runs.
	 */
	@Fork(value = 1, warmups = 1)
	@Warmup(iterations = 1)
	@Benchmark
	@BenchmarkMode(Mode.AverageTime)
	public void benchmarkEuclidAlgorithm(BenchmarkState state) {		

		int gcd = GreatestCommonDivisor.euclidAlgorithm(state.m,state.n);

	}

	/**
	 * Benchmark method. Runs definition based algorithm and takes measurements described in 
	 * the tags.
	 * Runs multiple times for each order, taking into account warmup runs and iterations.
	 * @param state is the (parameterized) state on which the benchmark method runs.
	 */
	@Fork(value = 1, warmups = 1)
	@Warmup(iterations = 1)
	@Benchmark
	@BenchmarkMode(Mode.AverageTime)
	public void benchmarkDefinitionBased(BenchmarkState state) {		

		int gcd = GreatestCommonDivisor.definitionBasedAlgorithm(state.m,state.n);

	}


}