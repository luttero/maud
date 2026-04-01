package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Utility class for tensor and matrix operations.
 *
 * Contains the translated Fortran routines for 6x6 matrix inversion
 * using LU Decomposition (from Numerical Recipes).
 *
 * <p><b>Indexing Note:</b>
 * All methods assume 1-based indexing. Arrays passed to these
 * methods (e.g., double[7][7]) should use indices [1..6].
 */
public class TensorUtils {

    // Parameters from LUDCMPC
    private static final int NMAX = 120;
    private static final double TINY = 1.0e-20;

    /**
     * Inverts a 6x6 matrix 'A' and stores the result in 'AI'.
     * Translation of Fortran SUBROUTINE INVTEN.
     *
     * <p>This method uses LU decomposition to solve A*X = I,
     * where I is the identity matrix and X is the inverse.
     *
     * @param A  The 7x7 input matrix (using indices 1..6).
     * @param AI The 7x7 output matrix (using indices 1..6) to store the inverse.
     */
    public static void invten(double[][] A, double[][] AI) {
        
        // Local arrays, 1-based [7][7] or [7]
        double[][] ax = new double[7][7];
        double[][] d = new double[7][7]; // Will hold inv(AX)
        int[] indx = new int[7];
        double[] col = new double[7]; // Temp vector for a column

        // ANORM=TNORM(A,6,6)
        double anorm = tnorm(A, 6, 6);

        // Normalize A into AX and initialize D as the identity matrix
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                ax[i][j] = A[i][j] / anorm;
                d[i][j] = (i == j) ? 1.0 : 0.0;
            }
        }

        // CALL LUDCMPC(AX,6,6,INDX,DET)
        // Decompose ax in-place. 'det' holds the determinant sign (1.0 or -1.0).
        double det = ludcmpc(ax, 6, 6, indx);

        // Solve A*x = b for each column of the identity matrix.
        // The Fortran version passes D(1,I) (the I-th column)
        // to LUBKSBC, which modifies it in-place.
        // We simulate this by copying columns into 'col',
        // solving, and copying the result back into 'D'.
        
        for (int i = 1; i <= 6; i++) { // 'i' is the column index
            // 1. Copy i-th column of identity (from d) into 'col'
            for (int j = 1; j <= 6; j++) {
                col[j] = d[j][i];
            }

            // 2. Solve A*x = col. 'col' is overwritten with the solution x.
            // CALL LUBKSBC(AX,6,6,INDX,D(1,I))
            lubksbc(ax, 6, 6, indx, col);

            // 3. Store the solution 'col' into the i-th column of D
            for (int j = 1; j <= 6; j++) {
                d[j][i] = col[j];
            }
        }

        // At this point, D = inv(AX).
        // Renormalize to get the final inverse, AI.
        // AI(I,J)=D(I,J)*INVFAC(I,J)/ANORM
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                AI[i][j] = d[i][j] * CommonGlobals.invfac[i][j] / anorm;
            }
        }
    }

    /**
     * Performs LU decomposition.
     * Translation of Fortran SUBROUTINE LUDCMPC.
     *
     * @param A    The matrix (NxN) to be decomposed. Overwritten with LU decomposition.
     * @param N    The logical size of the matrix.
     * @param NP   The physical size (for Fortran compatibility, N=NP here).
     * @param INDX Output array for pivot indices.
     * @return The determinant sign (1.0 for even permutations, -1.0 for odd).
     */
    public static double ludcmpc(double[][] A, int N, int NP, int[] INDX) {
        double[] vv = new double[NMAX + 1]; // 1-based indexing
        double d = 1.0;

        for (int i = 1; i <= N; i++) {
            double aamax = 0.0;
            for (int j = 1; j <= N; j++) {
                if (Math.abs(A[i][j]) > aamax) {
                    aamax = Math.abs(A[i][j]);
                }
            }
            if (aamax == 0.0) {
                // Fortran STOP
                throw new RuntimeException("PROGRAM STOP --> ZERO MATRIX IN SUBROUT LUDCMPC");
            }
            vv[i] = 1.0 / aamax;
        }

        for (int j = 1; j <= N; j++) {
            for (int i = 1; i < j; i++) {
                double sum = A[i][j];
                for (int k = 1; k < i; k++) {
                    sum -= A[i][k] * A[k][j];
                }
                A[i][j] = sum;
            }
            
            double aamax = 0.0;
            int imax = 0;
            for (int i = j; i <= N; i++) {
                double sum = A[i][j];
                for (int k = 1; k < j; k++) {
                    sum -= A[i][k] * A[k][j];
                }
                A[i][j] = sum;
                double dum = vv[i] * Math.abs(sum);
                if (dum >= aamax) {
                    aamax = dum;
                    imax = i;
                }
            }

            if (j != imax) {
                for (int k = 1; k <= N; k++) {
                    double dum = A[imax][k];
                    A[imax][k] = A[j][k];
                    A[j][k] = dum;
                }
                d = -d;
                vv[imax] = vv[j];
            }
            
            INDX[j] = imax;
            if (A[j][j] == 0.0) {
                A[j][j] = TINY;
            }
            
            if (j != N) {
                double dum = 1.0 / (A[j][j]);
                for (int i = j + 1; i <= N; i++) {
                    A[i][j] *= dum;
                }
            }
        }
        return d;
    }

    /**
     * Performs forward and backward substitution.
     * Translation of Fortran SUBROUTINE LUBKSBC.
     *
     * @param A    The LU-decomposed matrix from ludcmpc.
     * @param N    The logical size of the matrix.
     * @param NP   The physical size.
     * @param INDX The pivot index array from ludcmpc.
     * @param D    The right-hand-side vector (b). Overwritten with the solution (x).
     */
    public static void lubksbc(double[][] A, int N, int NP, int[] INDX, double[] D) {
        int ii = 0;
        
        // Forward substitution
        for (int i = 1; i <= N; i++) {
            int ll = INDX[i];
            double sum = D[ll];
            D[ll] = D[i];
            if (ii != 0) {
                for (int j = ii; j <= i - 1; j++) {
                    sum -= A[i][j] * D[j];
                }
            } else if (sum != 0.0) {
                ii = i;
            }
            D[i] = sum;
        }

        // Backward substitution
        for (int i = N; i >= 1; i--) {
            double sum = D[i];
            if (i < N) {
                for (int j = i + 1; j <= N; j++) {
                    sum -= A[i][j] * D[j];
                }
            }
            D[i] = sum / A[i][i];
        }
    }

/**
     * Calculates the Frobenius (L2) norm of a matrix.
     * Translation of Fortran FUNCTION tnorm.
     *
     * <p>This function iterates through all elements of the matrix,
     * sums their squares, and returns the square root.
     *
     * @param A     Input matrix (e.g., [7][7], using indices 1..6).
     * @param nrows The number of rows (e.g., 6).
     * @param ncols The number of columns (e.g., 6).
     * @return The Frobenius norm of the matrix.
     */
public static double tnorm(double[][] A, int nrows, int ncols) {
        double tnorm = 0.0;

        // The Fortran code passes the 2D array A(6,6) as a 1D vector v(36)
        // and sums the squares of all 36 elements.
        // The Java equivalent is to iterate over all elements of the 2D array.
        // We use column-major iteration to match the Fortran memory layout,
        // although for a simple sum, row-major would also work.

        // j = column index
        for (int j = 1; j <= ncols; j++) {
            // i = row index
            for (int i = 1; i <= nrows; i++) {
                if (A != null && A[i] != null && A[i].length > j) {
                    tnorm += A[i][j] * A[i][j];
                }
            }
        }

        return Math.sqrt(tnorm);
    }

/**
     * Transforms between Voigt notation and full tensor notation.
     * Translation of Fortran SUBROUTINE voigt.
     *
     * <p>Assumes 1-based indexing for all arrays.
     *
     * @param T1   (In/Out) 6x1 Voigt vector (uses indices 1-6)
     * @param T2   (In/Out) 3x3 Full tensor (uses indices [1-3][1-3])
     * @param C2   (In/Out) 6x6 Voigt matrix (uses indices [1-6][1-6])
     * @param C4   (In/Out) 3x3x3x3 Full tensor (uses indices [1-3][1-3][1-3][1-3])
     * @param IOPT Option flag:
     * <ul>
     * <li>1: T1 (6x1) -> T2 (3x3)
     * <li>2: T2 (3x3) -> T1 (6x1)
     * <li>3: C2 (6x6) -> C4 (3x3x3x3)
     * <li>4: C4 (3x3x3x3) -> C2 (6x6)
     * </ul>
     */
    public static void voigt(double[] T1, double[][] T2, double[][] C2, double[][][][] C4, int IOPT) {
        
        // IJV(6,2) is the mapping key from common_globals
        // Fortran: 1=11, 2=22, 3=33, 4=23, 5=13, 6=12
        // Assumes CommonGlobals.ijv is initialized (e.g., in a static block
        // or initialization routine) and is 1-based [7][3].

        if (IOPT == 1) {
            // 6x1 Voigt vector (T1) TO 3x3 symmetric tensor (T2)
            for (int i = 1; i <= 6; i++) {
                int i1 = CommonGlobals.ijv[i][1];
                int i2 = CommonGlobals.ijv[i][2];
                T2[i1][i2] = T1[i];
                T2[i2][i1] = T1[i];
            }
        } else if (IOPT == 2) {
            // 3x3 tensor (T2) TO 6x1 Voigt vector (T1)
            for (int i = 1; i <= 6; i++) {
                int i1 = CommonGlobals.ijv[i][1];
                int i2 = CommonGlobals.ijv[i][2];
                T1[i] = T2[i1][i2];
            }
        } else if (IOPT == 3) {
            // 6x6 Voigt matrix (C2) TO 3x3x3x3 tensor (C4)
            for (int i = 1; i <= 6; i++) {
                int i1 = CommonGlobals.ijv[i][1];
                int i2 = CommonGlobals.ijv[i][2];
                for (int j = 1; j <= 6; j++) {
                    int j1 = CommonGlobals.ijv[j][1];
                    int j2 = CommonGlobals.ijv[j][2];
                    C4[i1][i2][j1][j2] = C2[i][j];
                    C4[i2][i1][j1][j2] = C2[i][j];
                    C4[i1][i2][j2][j1] = C2[i][j];
                    C4[i2][i1][j2][j1] = C2[i][j];
                }
            }
        } else if (IOPT == 4) {
            // 3x3x3x3 tensor (C4) TO 6x6 Voigt matrix (C2)
            for (int i = 1; i <= 6; i++) {
                int i1 = CommonGlobals.ijv[i][1];
                int i2 = CommonGlobals.ijv[i][2];
                for (int j = 1; j <= 6; j++) {
                    int j1 = CommonGlobals.ijv[j][1];
                    int j2 = CommonGlobals.ijv[j][2];
                    C2[i][j] = C4[i1][i2][j1][j2];
                }
            }
        }
    }
    
/**
     * A simple class to hold the three Euler angles.
     * Used as a return type for the matrix-to-angles conversion.
     */
    public static class EulerAngles {
        public double phi;   // Corresponds to ph
        public double theta; // Corresponds to th
        public double omega; // Corresponds to tm

        public EulerAngles(double phi, double theta, double omega) {
            this.phi = phi;
            this.theta = theta;
            this.omega = omega;
        }
    }

    /**
     * Calculates the Euler angles (phi, theta, omega) from a 3x3 rotation matrix.
     * This is the Java translation of 'euler' (iopt=1).
     *
     * @param a The 3x3 rotation matrix (1-based, e.g., [4][4]).
     * @return An `EulerAngles` object containing {phi, theta, omega} in DEGREES.
     */
    public static EulerAngles matrixToEuler(double[][] a) {
        double ph, th, tm;
        
        // th=dacos(a(3,3))
        th = Math.acos(a[3][3]);
        
        // if(dabs(a(3,3)).ge.0.9999999d0) then
        if (Math.abs(a[3][3]) >= 0.9999999) {
            // Gimbal lock case
            tm = 0.0;
            // ph=datan2(a(1,2),a(1,1))
            ph = Math.atan2(a[1][2], a[1][1]);
        } else {
            // sth=dsin(th)
            double sth = Math.sin(th);
            // tm=datan2(a(1,3)/sth,a(2,3)/sth)
            tm = Math.atan2(a[1][3] / sth, a[2][3] / sth);
            // ph=datan2(a(3,1)/sth,-a(3,2)/sth)
            ph = Math.atan2(a[3][1] / sth, -a[3][2] / sth);
        }

        // Convert from radians to degrees
        th = th * CommonConstants.RAD_TO_DEG;
        ph = ph * CommonConstants.RAD_TO_DEG;
        tm = tm * CommonConstants.RAD_TO_DEG;
        
        return new EulerAngles(ph, th, tm);
    }

    /**
     * Calculates a 3x3 rotation matrix 'a' from Euler angles (phi, theta, omega).
     * This is the Java translation of 'euler' (iopt=2).
     *
     * @param ph The first Euler angle (in DEGREES).
     * @param th The second Euler angle (in DEGREES).
     * @param tm The third Euler angle ('omega' or 'tm', in DEGREES).
     * @param a  The 3x3 output matrix (1-based, e.g., [4][4]) to be populated.
     */
    public static void eulerFromAngles(double ph, double th, double tm, double[][] a) {
        // sph=dsin(ph*DEG_TO_RAD)
        double sph = Math.sin(ph * CommonConstants.DEG_TO_RAD);
        double cph = Math.cos(ph * CommonConstants.DEG_TO_RAD);
        double sth = Math.sin(th * CommonConstants.DEG_TO_RAD);
        double cth = Math.cos(th * CommonConstants.DEG_TO_RAD);
        double stm = Math.sin(tm * CommonConstants.DEG_TO_RAD);
        double ctm = Math.cos(tm * CommonConstants.DEG_TO_RAD);

        // a(1,1)=ctm*cph-sph*stm*cth
        a[1][1] = ctm * cph - sph * stm * cth;
        // a(2,1)=-stm*cph-sph*ctm*cth
        a[2][1] = -stm * cph - sph * ctm * cth;
        // a(3,1)=sph*sth
        a[3][1] = sph * sth;
        
        // a(1,2)=ctm*sph+cph*stm*cth
        a[1][2] = ctm * sph + cph * stm * cth;
        // a(2,2)=-sph*stm+cph*ctm*cth
        a[2][2] = -sph * stm + cph * ctm * cth;
        // a(3,2)=-sth*cph
        a[3][2] = -sth * cph;
        
        // a(1,3)=sth*stm
        a[1][3] = sth * stm;
        // a(2,3)=ctm*sth
        a[2][3] = ctm * sth;
        // a(3,3)=cth
        a[3][3] = cth;
    } 
       
       // A single Random instance for the 'ran2' stub.
       public static java.util.Random randomGenerator = new java.util.Random();

    /**
     * <b>*** STUB IMPLEMENTATION ***</b>
     * <p>
     * This is a stub for the Fortran 'ran2' function. It mimics the behavior
     * of `ran2(jran)*nact(ng)+1` which returns a random integer between 1 and nact(ng).
     * <p>
     * The Fortran 'jran' seed is ignored, and Java's built-in RNG is used.
     * You may need to replace this with a more faithful port of 'ran2' if
     * specific seed behavior is required.
     *
     * @param jran Ignored seed.
     * @return A pseudo-random double between 0.0 (inclusive) and 1.0 (exclusive).
     */
    public static double ran2(int jran) {
        // System.err.println("Warning: Using stub implementation for ran2(jran).");
        return randomGenerator.nextDouble();
    }
    

    /**
     * A 1-based 3x3 identity matrix, used by Rodrigues.
     */
    public static final double[][] XID33 = {
        {0, 0, 0, 0},
        {0, 1, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1}
    };

    /**
     * Calculates the determinant of a 3x3 matrix.
     * Translation of Fortran FUNCTION det.
     *
     * @param a The 1-based [4][4] matrix.
     * @return The determinant.
     */
    public static double det(double[][] a) {
        return a[1][1] * a[2][2] * a[3][3]
             + a[1][2] * a[2][3] * a[3][1]
             + a[1][3] * a[2][1] * a[3][2]
             - a[1][3] * a[2][2] * a[3][1]
             - a[2][3] * a[3][2] * a[1][1]
             - a[1][2] * a[2][1] * a[3][3];
    }

  /**
   * Rotates a 4th-order stiffness tensor into the ellipsoid coordinate system,
   * calls Eshelby, and rotates the resulting Eshelby tensors back.
   * Full translation of Fortran SUBROUTINE stiffness_rotation.
   *
   * @param stiffness (In) The 4th-order stiffness tensor (sample frame).
   * @param rotb      (In) The 3x3 rotation matrix (ellipsoid -> sample).
   * @param elipaxis  (In) The 3 ellipsoid axis lengths.
   * @param es4       (Out) The symmetric Eshelby tensor (sample frame).
   * @param eas4      (Out) The anti-symmetric Eshelby tensor (sample frame).
   */
  public static void stiffness_rotation(double[][][][] stiffness, double[][] rotb, double[] elipaxis,
                                        double[][][][] es4, double[][][][] eas4) {

    // --- 1. Rotate STIFFNESS from Sample to Ellipsoid axes ---
    // ASS4GA = R_transpose * R_transpose * STIFFNESS * R * R
    for (int i = 1; i <= 3; i++) {
      for (int j = 1; j <= 3; j++) {
        for (int m = 1; m <= 3; m++) {
          for (int n = 1; n <= 3; n++) {
            double DUMMY = 0.0;
            for (int i1 = 1; i1 <= 3; i1++) {
              for (int j1 = 1; j1 <= 3; j1++) {
                for (int m1 = 1; m1 <= 3; m1++) {
                  for (int n1 = 1; n1 <= 3; n1++) {
                    // ROTB(I1,I) is R_i1,i (transpose)
                    DUMMY += rotb[i1][i] * rotb[j1][j] * rotb[m1][m] * rotb[n1][n]
                        * stiffness[i1][j1][m1][n1];
                  }
                }
              }
            }
            ModStiffnessRotation.ASS4GA[i][j][m][n] = DUMMY;
          }
        }
      }
    }

    // --- 2. Calculate Eshelby tensors in Ellipsoid axes ---
    CalculationRoutines.eshelbyb(elipaxis, ModStiffnessRotation.ASS4GA, 0.0,
        ModStiffnessRotation.E4GA, ModStiffnessRotation.AUX3333, 1);

    // --- 3. Rotate Eshelby tensors (E4GA, AUX3333) from Ellipsoid to Sample axes ---
    // ES4 = R * R * E4GA * R_transpose * R_transpose
    for (int i = 1; i <= 3; i++) {
      for (int j = 1; j <= 3; j++) {
        for (int m = 1; m <= 3; m++) {
          for (int n = 1; n <= 3; n++) {
            double DUMMYE_S = 0.0;
            double DUMMYE_A = 0.0;
            for (int i1 = 1; i1 <= 3; i1++) {
              for (int j1 = 1; j1 <= 3; j1++) {
                for (int m1 = 1; m1 <= 3; m1++) {
                  for (int n1 = 1; n1 <= 3; n1++) {
                    double rotFactor = rotb[i][i1] * rotb[j][j1] * rotb[m][m1] * rotb[n][n1];
                    DUMMYE_S += rotFactor * ModStiffnessRotation.E4GA[i1][j1][m1][n1];
                    DUMMYE_A += rotFactor * ModStiffnessRotation.AUX3333[i1][j1][m1][n1];
                  }
                }
              }
            }
            es4[i][j][m][n] = DUMMYE_S;
            eas4[i][j][m][n] = DUMMYE_A;
          }
        }
      }
    }
  }
}
