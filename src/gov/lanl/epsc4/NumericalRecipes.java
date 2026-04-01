package gov.lanl.epsc4;

/**
 * Contains translations of general numerical algorithms (e.g., Jacobi, Eigsrt).
 * All methods assume 1-based indexing for arrays.
 */
public class NumericalRecipes {

//    private static final int NMAX = 500;

    /**
     * Computes all eigenvalues and eigenvectors of a real symmetric matrix.
     */
    public static void jacobi(double[][] a, int n, int np, double[] d,
                              double[][] v, IntHolder nrot, IntHolder ier) {
        // ... (Full implementation from jacobi) ...
    }

    /**
     * Sorts eigenvalues (d) from largest to smallest.
     */
    public static void eigsrt(double[] d, double[][] v, int n, int np) {
        // ... (Full implementation from eigsrt) ...
    }
}
