package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Contains helper methods for numerical integration and matrix operations
 * required by the Eshelby subroutines.
 *
 * <p>All methods assume 1-based indexing for arrays to match
 * the Fortran porting logic.
 */
public class IntegrationUtils {

/**
     * Given lower and upper limits of integration (x1, x2), returns arrays
     * x[n] and w[n] containing abscissas and weights of the
     * n-point Gauss-Legendre quadrature formula.
     *
     * Translation of Fortran SUBROUTINE GAULEG.
     *
     * @param x1 The lower bound of the integration interval.
     * @param x2 The upper bound of the integration interval.
     * @param x  (Out) Array to be filled with quadrature points (1-based, size [n+1]).
     * @param w  (Out) Array to be filled with quadrature weights (1-based, size [n+1]).
     * @param n  The number of points (quadrature order).
     */
    public static void gauleg(double x1, double x2, double[] x, double[] w, int n) {

        final double eps = 3.0e-14;
        int m = (n + 1) / 2;
        double xm = 0.5 * (x1 + x2);
        double xl = 0.5 * (x2 - x1);

        for (int i = 1; i <= m; i++) {
            // Initial guess for the root
            double z = Math.cos(CommonConstants.PI * (i - 0.25) / (n + 0.5));
            double z1;
            double pp = 0;
            double p1 = 0, p2 = 0, p3 = 0;

            // Loop using Newton's method to find the root
            do {
                p1 = 1.0;
                p2 = 0.0;
                // Recurrence relation for Legendre polynomials
                for (int j = 1; j <= n; j++) {
                    p3 = p2;
                    p2 = p1;
                    p1 = ((2.0 * j - 1.0) * z * p2 - (j - 1.0) * p3) / j;
                }
                
                // pp is the derivative of p1 (the n-th Legendre polynomial)
                pp = n * (z * p1 - p2) / (z * z - 1.0);
                z1 = z;
                z = z1 - p1 / pp; // Newton's step
                
            } while (Math.abs(z - z1) > eps);

            // Scale root to the interval [x1, x2] and store
            x[i] = xm - xl * z;
            x[n + 1 - i] = xm + xl * z;

            // Calculate and store the weight
            w[i] = 2.0 * xl / ((1.0 - z * z) * pp * pp);
            w[n + 1 - i] = w[i];
        }
    }
    /**
     * Inverts a 3x3 symmetric matrix 'A' using explicit 6-element Voigt notation.
     * Translation of Fortran SUBROUTINE ESH_INV3_VOIGT.
     *
     * @param A    Input 6-element Voigt array (1-based, size 7 or more).
     * (11, 22, 33, 23, 13, 12)
     * @param AINV (Out) 6-element Voigt array for the inverse (1-based).
     */
    public static void esh_inv3_voigt(double[] A, double[] AINV) {
        
        // DET=A(1)*A(2)*A(3)+2*A(4)*A(5)*A(6)-A(1)*A(4)*A(4)-A(2)*A(5)*A(5)-A(3)*A(6)*A(6)
        double det = A[1] * A[2] * A[3]
                   + 2.0 * A[4] * A[5] * A[6]
                   - A[1] * A[4] * A[4]
                   - A[2] * A[5] * A[5]
                   - A[3] * A[6] * A[6];

        // AINV(1) = ( A(2)*A(3) - A(4)*A(4))/DET
        AINV[1] = (A[2] * A[3] - A[4] * A[4]) / det;
        // AINV(2) = ( A(1)*A(3) - A(5)*A(5))/DET
        AINV[2] = (A[1] * A[3] - A[5] * A[5]) / det;
        // AINV(3) = ( A(1)*A(2) - A(6)*A(6))/DET
        AINV[3] = (A[1] * A[2] - A[6] * A[6]) / det;
        // AINV(4) = (-A(1)*A(4) + A(5)*A(6))/DET
        AINV[4] = (-A[1] * A[4] + A[5] * A[6]) / det;
        // AINV(5) = ( A(4)*A(6) - A(2)*A(5))/DET
        AINV[5] = (A[4] * A[6] - A[2] * A[5]) / det;
        // AINV(6) = (-A(3)*A(6) + A(4)*A(5))/DET
        AINV[6] = (-A[3] * A[6] + A[4] * A[5]) / det;
    }

    /**
     * Inverts a 4x4 symmetric matrix 'a' using explicit 10-element Voigt notation.
     * Translation of Fortran SUBROUTINE esh_inv4_voigt.
     *
     * @param a    Input 10-element Voigt array (1-based, size 11).
     * @param ainv (Out) 10-element Voigt array for the inverse (1-based).
     */
    public static void esh_inv4_voigt(double[] a, double[] ainv) {

        // Calculate cofactors
        ainv[1] = a[2]*a[3]*a[10] + 2*a[4]*a[8]*a[9] - a[2]*a[9]*a[9] - a[3]*a[8]*a[8] - a[4]*a[4]*a[10];
        ainv[2] = a[1]*a[3]*a[10] + 2*a[5]*a[7]*a[9] - a[1]*a[9]*a[9] - a[3]*a[7]*a[7] - a[5]*a[5]*a[10];
        ainv[3] = a[1]*a[2]*a[10] + 2*a[6]*a[7]*a[8] - a[1]*a[8]*a[8] - a[2]*a[7]*a[7] - a[6]*a[6]*a[10];
        
        ainv[4] = a[1]*a[4]*a[10] + a[5]*a[7]*a[8] + a[6]*a[7]*a[9] - a[1]*a[8]*a[9] - a[4]*a[7]*a[7] - a[5]*a[6]*a[10];
        ainv[4] = -ainv[4];

        ainv[5] = a[4]*a[6]*a[10] + a[2]*a[7]*a[9] + a[5]*a[8]*a[8] - a[4]*a[7]*a[8] - a[6]*a[8]*a[9] - a[2]*a[5]*a[10];

        ainv[6] = a[3]*a[6]*a[10] + a[5]*a[8]*a[9] + a[4]*a[7]*a[9] - a[3]*a[7]*a[8] - a[6]*a[9]*a[9] - a[4]*a[5]*a[10];
        ainv[6] = -ainv[6];

        ainv[7] = a[4]*a[6]*a[9] + a[4]*a[5]*a[8] + a[2]*a[3]*a[7] - a[4]*a[4]*a[7] - a[2]*a[5]*a[9] - a[3]*a[6]*a[8];
        ainv[7] = -ainv[7];

        ainv[8] = a[1]*a[4]*a[9] + a[5]*a[5]*a[8] + a[3]*a[6]*a[7] - a[4]*a[5]*a[7] - a[5]*a[6]*a[9] - a[1]*a[3]*a[8];

        ainv[9] = a[1]*a[2]*a[9] + a[5]*a[6]*a[8] + a[4]*a[6]*a[7] - a[2]*a[5]*a[7] - a[6]*a[6]*a[9] - a[1]*a[4]*a[8];
        ainv[9] = -ainv[9];

        ainv[10] = a[1]*a[2]*a[3] + 2*a[4]*a[5]*a[6] - a[1]*a[4]*a[4] - a[2]*a[5]*a[5] - a[3]*a[6]*a[6];

        // Calculate determinant
        double det = a[1]*ainv[1] + a[2]*ainv[2] + a[3]*ainv[3]
                   + 2.0 * a[4]*ainv[4] + 2.0 * a[5]*ainv[5] + 2.0 * a[6]*ainv[6]
                   + 2.0 * a[7]*ainv[7] + 2.0 * a[8]*ainv[8] + 2.0 * a[9]*ainv[9]
                   + a[10]*ainv[10];
        
        det = det / 4.0;

        // Divide cofactors by determinant
        for (int i = 1; i <= 10; i++) {
            ainv[i] = ainv[i] / det;
        }
    }

    /**
     * Performs the multiplication: A(i,k) = B(i,j,k,l) * C(j,l) using Voigt's notation.
     * Translation of Fortran SUBROUTINE esh_mult_voigt.
     *
     * @param B Input 6x6 symmetric matrix (1-based, [7][7]).
     * @param C Input 3x3 symmetric tensor in 6-element Voigt notation (1-based, [7]).
     * @param A (Out) 3x3 symmetric tensor result, in 6-element Voigt notation (1-based, [7]).
     */
    public static void esh_mult_voigt(double[][] B, double[] C, double[] A) {
        
        // A(1) = 11 component
        A[1] = B[1][1]*C[1] + B[6][6]*C[2] + B[5][5]*C[3]
             + 2.0 * (B[5][6]*C[4] + B[1][5]*C[5] + B[1][6]*C[6]);

        // A(2) = 22 component
        A[2] = B[6][6]*C[1] + B[2][2]*C[2] + B[4][4]*C[3]
             + 2.0 * (B[2][4]*C[4] + B[4][6]*C[5] + B[2][6]*C[6]);

        // A(3) = 33 component
        A[3] = B[5][5]*C[1] + B[4][4]*C[2] + B[3][3]*C[3]
             + 2.0 * (B[3][4]*C[4] + B[3][5]*C[5] + B[4][5]*C[6]);

        // A(4) = 23 component
        A[4] = B[5][6]*C[1] + B[2][4]*C[2] + B[3][4]*C[3]
             + (B[2][3] + B[4][4])*C[4] + (B[3][6] + B[4][5])*C[5] + (B[4][6] + B[2][5])*C[6];

        // A(5) = 13 component
        A[5] = B[1][5]*C[1] + B[4][6]*C[2] + B[3][5]*C[3]
             + (B[3][6] + B[4][5])*C[4] + (B[1][3] + B[5][5])*C[5] + (B[1][4] + B[5][6])*C[6];

        // A(6) = 12 component
        A[6] = B[1][6]*C[1] + B[2][6]*C[2] + B[4][5]*C[3]
             + (B[4][6] + B[2][5])*C[4] + (B[1][4] + B[5][6])*C[5] + (B[1][2] + B[6][6])*C[6];
    }
}

