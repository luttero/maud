package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_polefig module.
 *
 * This class holds static "work" variables used by the PoleFig subroutine.
 * It duplicates some variables from other modules (like crystal_symmetry)
 * because the PoleFig routine re-calculates them for its own use.
 *
 * All arrays are 1-based to match Fortran indexing.
 */
public class ModPolefig {

    // --- Work Vectors & Matrices (1-based) ---
    public static double[][] pc = new double[4][25];
    public static double[] ps = new double[4];
    public static double[] etelcsx = new double[7];
    public static double[] aux = new double[4];
    public static double[] vs = new double[4];
    public static double[][] ipol = new double[7][4];
    public static double[] sb = new double[4];
    public static double[] sn = new double[4];
    public static double[][] sneq = new double[4][25];
    public static double[] cdim = new double[4];
    public static double[] cang = new double[4];
    public static double[][] cvec = new double[4][4];

    // --- Symmetry Operators (1-based) ---
    public static int[][][] hh = new int[4][4][25];
    public static double[][][] hx = new double[4][4][7];
    public static int[] itag = new int[25];

    /** Pole Figure rotation matrices (0-based 3rd index) */
    public static double[][][] temprot = new double[4][4][4];

    // --- Work Scalars ---
    public static double ang, chi, eps, eta, prodesc, snnor, snpro, spread2;
    public static double sumstr, sumwgt, toler2;
    public static int npol, nPoles, nn, ndiff2, nind, ipl, ipolefig;
    public static int nsymop, nr, nrot, ns, mn, k, m;
    public static int[] ipole = new int[5];
    public static int[] isn = new int[5];
    public static int[] isb = new int[5];

    // Static initializer for temprot
    static {
        // temprot(..., 0) - Regular (3 at center)
        temprot[1][1][0] = 1.0; temprot[1][2][0] = 0.0; temprot[1][3][0] = 0.0;
        temprot[2][1][0] = 0.0; temprot[2][2][0] = 1.0; temprot[2][3][0] = 0.0;
        temprot[3][1][0] = 0.0; temprot[3][2][0] = 0.0; temprot[3][3][0] = 1.0;

        // temprot(..., 1) - 90 deg rot about S1 (2 at center)
        temprot[1][1][1] = 1.0; temprot[1][2][1] = 0.0; temprot[1][3][1] = 0.0;
        temprot[2][1][1] = 0.0; temprot[2][2][1] = 0.0; temprot[2][3][1] = 1.0;
        temprot[3][1][1] = 0.0; temprot[3][2][1] = -1.0; temprot[3][3][1] = 0.0;

        // temprot(..., 2) - 90 deg rot about S2 (1 at center)
        temprot[1][1][2] = 0.0; temprot[1][2][2] = 0.0; temprot[1][3][2] = -1.0;
        temprot[2][1][2] = 0.0; temprot[2][2][2] = 1.0; temprot[2][3][2] = 0.0;
        temprot[3][1][2] = 1.0; temprot[3][2][2] = 0.0; temprot[3][3][2] = 0.0;

        // temprot(..., 3) - 90 deg rot about S3
        temprot[1][1][3] = 0.0; temprot[1][2][3] = 1.0; temprot[1][3][3] = 0.0;
        temprot[2][1][3] = -1.0; temprot[2][2][3] = 0.0; temprot[2][3][3] = 0.0;
        temprot[3][1][3] = 0.0; temprot[3][2][3] = 0.0; temprot[3][3][3] = 1.0;
    }
}