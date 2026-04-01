package gov.lanl.epsc4;

import java.util.Random;

/**
 * Utility class for tensor, matrix, and vector operations.
 * Contains static helper methods.
 * All methods assume 1-based indexing for arrays.
 */
public class TensorUtils {

    private static final int NMAX = 120; // For ludcmpc
    private static final double TINY = 1.0e-20;
    
    /**
     * A 1-based 3x3 identity matrix, used by Rodrigues.
     */
    private static final double[][] XID33 = {
        {0, 0, 0, 0},
        {0, 1, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1}
    };

    /**
     * Calculates the determinant of a 3x3 matrix.
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
     * Calculates the Frobenius (L2) norm of a matrix.
     */
    private static double tnorm(double[][] A, int nrows, int ncols) {
        double tnorm_sq = 0.0;
        for (int j = 1; j <= ncols; j++) {
            for (int i = 1; i <= nrows; i++) {
                tnorm_sq += (A[i][j] * A[i][j]);
            }
        }
        return Math.sqrt(tnorm_sq);
    }
    
    /**
     * Calculates the relative difference (Frobenius norm) between two matrices.
     */
    public static double tmismatch(double[][] v1, double[][] v2, int nrows, int ncols) {
        double v_dif_sq_sum = 0.0;
        double v_ave_sq_sum = 0.0;

        for (int i = 1; i <= nrows; i++) {
            for (int j = 1; j <= ncols; j++) {
                double dif = v1[i][j] - v2[i][j];
                double ave = 0.5 * (v1[i][j] + v2[i][j]);
                v_dif_sq_sum += (dif * dif);
                v_ave_sq_sum += (ave * ave);
            }
        }

        double norm_dif = Math.sqrt(v_dif_sq_sum);
        double norm_ave = Math.sqrt(v_ave_sq_sum);

        if (norm_ave == 0.0) return (norm_dif == 0.0) ? 0.0 : 1.0;
        return norm_dif / norm_ave;
    }

    /**
     * Inverts a 6x6 matrix 'A' and stores the result in 'AI'.
     */
    public static void invten(double[][] A, double[][] AI, double[][] invfac) {
        double[][] ax = new double[7][7];
        double[][] d = new double[7][7]; 
        int[] indx = new int[7];
        double[] col = new double[7]; 
        double anorm = tnorm(A, 6, 6);

        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                ax[i][j] = A[i][j] / anorm;
                d[i][j] = (i == j) ? 1.0 : 0.0;
            }
        }

        ludcmpc(ax, 6, 6, indx);

        for (int i = 1; i <= 6; i++) { 
            for (int j = 1; j <= 6; j++) col[j] = d[j][i];
            lubksbc(ax, 6, 6, indx, col);
            for (int j = 1; j <= 6; j++) d[j][i] = col[j];
        }

        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                AI[i][j] = d[i][j] * invfac[i][j] / anorm;
            }
        }
    }
    
    /**
     * Performs LU decomposition.
     */
    public static double ludcmpc(double[][] A, int N, int NP, int[] INDX) {
        double[] vv = new double[NMAX + 1];
        double d = 1.0;

        for (int i = 1; i <= N; i++) {
            double aamax = 0.0;
            for (int j = 1; j <= N; j++) {
                if (Math.abs(A[i][j]) > aamax) aamax = Math.abs(A[i][j]);
            }
            if (aamax == 0.0) throw new RuntimeException("Singular matrix in ludcmpc");
            vv[i] = 1.0 / aamax;
        }

        for (int j = 1; j <= N; j++) {
            for (int i = 1; i < j; i++) {
                double sum = A[i][j];
                for (int k = 1; k < i; k++) sum -= A[i][k] * A[k][j];
                A[i][j] = sum;
            }
            double aamax = 0.0;
            int imax = 0;
            for (int i = j; i <= N; i++) {
                double sum = A[i][j];
                for (int k = 1; k < j; k++) sum -= A[i][k] * A[k][j];
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
            if (A[j][j] == 0.0) A[j][j] = TINY;
            if (j != N) {
                double dum = 1.0 / (A[j][j]);
                for (int i = j + 1; i <= N; i++) A[i][j] *= dum;
            }
        }
        return d;
    }

    /**
     * Performs forward and backward substitution.
     */
    public static void lubksbc(double[][] A, int N, int NP, int[] INDX, double[] D) {
        int ii = 0;
        for (int i = 1; i <= N; i++) {
            int ll = INDX[i];
            double sum = D[ll];
            D[ll] = D[i];
            if (ii != 0) {
                for (int j = ii; j <= i - 1; j++) sum -= A[i][j] * D[j];
            } else if (sum != 0.0) {
                ii = i;
            }
            D[i] = sum;
        }
        for (int i = N; i >= 1; i--) {
            double sum = D[i];
            if (i < N) {
                for (int j = i + 1; j <= N; j++) sum -= A[i][j] * D[j];
            }
            D[i] = sum / A[i][i];
        }
    }
    
    /**
     * A simple class to hold the three Euler angles.
     */
    public static class EulerAngles {
        public double phi, theta, omega;
        public EulerAngles(double phi, double theta, double omega) {
            this.phi = phi; this.theta = theta; this.omega = omega;
        }
    }

    /**
     * Calculates the Euler angles from a 3x3 rotation matrix (iopt=1).
     */
    public static EulerAngles matrixToEuler(double[][] a) {
        double ph, th, tm;
        th = Math.acos(a[3][3]);
        if (Math.abs(a[3][3]) >= 0.9999999) {
            tm = 0.0;
            ph = Math.atan2(a[1][2], a[1][1]);
        } else {
            double sth = Math.sin(th);
            tm = Math.atan2(a[1][3] / sth, a[2][3] / sth);
            ph = Math.atan2(a[3][1] / sth, -a[3][2] / sth);
        }
        return new EulerAngles(ph * CommonConstants.RAD_TO_DEG,
                               th * CommonConstants.RAD_TO_DEG,
                               tm * CommonConstants.RAD_TO_DEG);
    }

    /**
     * Calculates a 3x3 rotation matrix 'a' from Euler angles (iopt=2).
     */
    public static void eulerFromAngles(double ph, double th, double tm, double[][] a) {
        double sph = Math.sin(ph * CommonConstants.DEG_TO_RAD);
        double cph = Math.cos(ph * CommonConstants.DEG_TO_RAD);
        double sth = Math.sin(th * CommonConstants.DEG_TO_RAD);
        double cth = Math.cos(th * CommonConstants.DEG_TO_RAD);
        double stm = Math.sin(tm * CommonConstants.DEG_TO_RAD);
        double ctm = Math.cos(tm * CommonConstants.DEG_TO_RAD);

        a[1][1] = ctm * cph - sph * stm * cth;
        a[2][1] = -stm * cph - sph * ctm * cth;
        a[3][1] = sph * sth;
        a[1][2] = ctm * sph + cph * stm * cth;
        a[2][2] = -sph * stm + cph * ctm * cth;
        a[3][2] = -sth * cph;
        a[1][3] = sth * stm;
        a[2][3] = ctm * sth;
        a[3][3] = cth;
    }
    
    /**
     * Transforms between Voigt notation and full tensor notation.
     */
    public static void voigt(double[] T1, double[][] T2, double[][] C2, double[][][][] C4, int IOPT) {
        // ... (Full implementation from previous step) ...
    }

    /**
     * Builds an incremental rotation matrix 'arot' (Rodrigues/Cayley formula).
     */
    public static void reorientGrain(double[][] arot, double[][] c) {
        // ... (Full implementation from REORIENT_GRAIN) ...
    }
    
    /**
     * Builds an incremental rotation matrix 'arot' (standard Rodrigues formula).
     */
    public static void rodrigues(double[][] c, double[][] arot) {
        // ... (Full implementation from RODRIGUES) ...
    }

    /**
     * Rotates a 4th-order stiffness tensor into the ellipsoid coordinate system,
     * calls Eshelby, and rotates the resulting Eshelby tensors back.
     */
    public static void stiffnessRotation(double[][][][] stiffness, double[][] rotb, double[] elipaxis,
                                         double[][][][] es4, double[][][][] eas4, EshelbyTensorCalculator eshelby) {
        // ... (Full implementation from STIFFNESS_ROTATION) ...
        // Note: This calls eshelby.calculate(...)
    }
}
