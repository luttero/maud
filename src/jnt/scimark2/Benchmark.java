package jnt.scimark2;

public class Benchmark {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Benchmark benchmark = new Benchmark();
		double score = benchmark.run();
		System.out.println("Score: " + score);
	}
	
	public double run() {
		
//		 default to the (small) cache-contained version

		double min_time = Constants.RESOLUTION_DEFAULT;

		int FFT_size = Constants.FFT_SIZE;
		int SOR_size =  Constants.SOR_SIZE;
		int Sparse_size_M = Constants.SPARSE_SIZE_M;
		int Sparse_size_nz = Constants.SPARSE_SIZE_nz;
		int LU_size = Constants.LU_SIZE;

		// run the benchmark

		double res[] = new double[6];
		Random R = new Random(Constants.RANDOM_SEED);

		res[1] = kernel.measureFFT( FFT_size, min_time, R);
		res[2] = kernel.measureSOR( SOR_size, min_time, R);
		res[3] = kernel.measureMonteCarlo(min_time, R);
		res[4] = kernel.measureSparseMatmult( Sparse_size_M, Sparse_size_nz, min_time, R);
		res[5] = kernel.measureLU( LU_size, min_time, R);

		res[0] = (res[1] + res[2] + res[3] + res[4] + res[5]) / 5;

	  return res[0];
		
	}
}
