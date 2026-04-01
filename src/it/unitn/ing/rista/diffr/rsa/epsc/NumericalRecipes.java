package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Contains translations of general numerical algorithms, such as those
 * from "Numerical Recipes" (e.g., Jacobi, Eigsrt).
 *
 * All methods assume 1-based indexing for arrays.
 */
public class NumericalRecipes {

    // Max array size for internal work arrays in jacobi
    private static final int NMAX = 500;

    /**
     * Computes all eigenvalues and eigenvectors of a real symmetric matrix a[1..n][1..n].
     * Translation of Fortran SUBROUTINE jacobi.
     *
     * @param a    (In/Out) The 1-based [np+1][np+1] input matrix. Overwritten on output.
     * @param n    The logical size of the matrix (e.g., 3 for a 3x3).
     * @param np   The physical size of the array (e.g., 3 for a [4][4]).
     * @param d    (Out) The 1-based [np+1] array to store eigenvalues.
     * @param v    (Out) The 1-based [np+1][np+1] matrix to store eigenvectors.
     * @param nrot (Out) Wrapper to store the number of Jacobi rotations.
     * @param ier  (Out) Wrapper to store the error flag (0=OK, 1=Fail).
     */
    public static void jacobi(double[][] a, int n, int np, double[] d,
                              double[][] v, IntHolder nrot, IntHolder ier) {
        
        // Internal 1-based work arrays
        double[] b = new double[NMAX + 1];
        double[] z = new double[NMAX + 1];

        for (int ip = 1; ip <= n; ip++) {
            for (int iq = 1; iq <= n; iq++) {
                v[ip][iq] = 0.0;
            }
            v[ip][ip] = 1.0;
        }
        for (int ip = 1; ip <= n; ip++) {
            b[ip] = a[ip][ip];
            d[ip] = b[ip];
            z[ip] = 0.0;
        }
        
        nrot.value = 0;
        for (int i = 1; i <= 50; i++) {
            double sm = 0.0;
            for (int ip = 1; ip <= n - 1; ip++) {
                for (int iq = ip + 1; iq <= n; iq++) {
                    sm += Math.abs(a[ip][iq]);
                }
            }

            if (sm == 0.0) {
                ier.value = 0;
                return;
            }

            double tresh = (i < 4) ? (0.2 * sm / (n * n)) : 0.0;
            
            for (int ip = 1; ip <= n - 1; ip++) {
                for (int iq = ip + 1; iq <= n; iq++) {
                    double g = 100.0 * Math.abs(a[ip][iq]);
                    
                    if ((i > 4) && (Math.abs(d[ip]) + g == Math.abs(d[ip]))
                               && (Math.abs(d[iq]) + g == Math.abs(d[iq]))) {
                        a[ip][iq] = 0.0;
                    } else if (Math.abs(a[ip][iq]) > tresh) {
                        double h = d[iq] - d[ip];
                        double t;
                        if (Math.abs(h) + g == Math.abs(h)) {
                            t = a[ip][iq] / h;
                        } else {
                            double theta = 0.5 * h / a[ip][iq];
                            t = 1.0 / (Math.abs(theta) + Math.sqrt(1.0 + theta * theta));
                            if (theta < 0.0) t = -t;
                        }
                        
                        double c = 1.0 / Math.sqrt(1.0 + t * t);
                        double s = t * c;
                        double tau = s / (1.0 + c);
                        h = t * a[ip][iq];
                        z[ip] -= h;
                        z[iq] += h;
                        d[ip] -= h;
                        d[iq] += h;
                        a[ip][iq] = 0.0;

                        for (int j = 1; j <= ip - 1; j++) {
                            g = a[j][ip]; h = a[j][iq];
                            a[j][ip] = g - s * (h + g * tau);
                            a[j][iq] = h + s * (g - h * tau);
                        }
                        for (int j = ip + 1; j <= iq - 1; j++) {
                            g = a[ip][j]; h = a[j][iq];
                            a[ip][j] = g - s * (h + g * tau);
                            a[j][iq] = h + s * (g - h * tau);
                        }
                        for (int j = iq + 1; j <= n; j++) {
                            g = a[ip][j]; h = a[iq][j];
                            a[ip][j] = g - s * (h + g * tau);
                            a[iq][j] = h + s * (g - h * tau);
                        }
                        for (int j = 1; j <= n; j++) {
                            g = v[j][ip]; h = v[j][iq];
                            v[j][ip] = g - s * (h + g * tau);
                            v[j][iq] = h + s * (g - h * tau);
                        }
                        nrot.value++;
                    }
                } // iq
            } // ip
            
            for (int ip = 1; ip <= n; ip++) {
                b[ip] += z[ip];
                d[ip] = b[ip];
                z[ip] = 0.0;
            }
        } // i (main iteration loop)

        // Failed to converge after 50 iterations
        ier.value = 1;
    }

    /**
     * Sorts eigenvalues (d) from largest to smallest, and re-arranges
     * the corresponding eigenvectors (v) accordingly.
     * Translation of Fortran SUBROUTINE eigsrt.
     *
     * @param d (In/Out) The 1-based [np+1] array of eigenvalues.
     * @param v (In/Out) The 1-based [np+1][np+1] matrix of eigenvectors.
     * @param n The logical size of the arrays.
     * @param np The physical size of the arrays.
     */
    public static void eigsrt(double[] d, double[][] v, int n, int np) {
        
        for (int i = 1; i <= n - 1; i++) {
            int k = i;
            double p = d[i];
            for (int j = i + 1; j <= n; j++) {
                if (d[j] >= p) {
                    k = j;
                    p = d[j];
                }
            }
            if (k != i) {
                d[k] = d[i];
                d[i] = p;
                for (int j = 1; j <= n; j++) {
                    p = v[j][i];
                    v[j][i] = v[j][k];
                    v[j][k] = p;
                }
            }
        }
    }
}