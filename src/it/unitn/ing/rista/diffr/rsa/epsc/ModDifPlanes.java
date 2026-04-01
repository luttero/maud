package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_dif_planes module.
 *
 * Holds static variables for diffraction calculations.
 * Arrays are 1-based to match Fortran indexing.
 */
public class ModDifPlanes {

    // --- Fixed-size "work" arrays ---
    public static double[][] pc = new double[4][25]; // 3x24 -> [4][25]
    public static double[] ps = new double[4];
    public static double[] etelcsx = new double[7];
    public static double[] aux = new double[4];
    public static double[][] rg = new double[4][4];
    public static double[] sn = new double[4];
    public static double[][] sneq = new double[4][25];
    public static double[] sb = new double[4];
    public static int[] isn = new int[5]; // 4 -> [5]
    public static int[] isb = new int[5];
    public static int[][] ipol = new int[7][4]; // 6x3 -> [7][4]
    
    // --- Allocatable arrays (initialized in difPlanes) ---
    public static int[] nfamily;
    public static double[][] vs;
    public static double[] para_w;
    public static double[][][] vc;
    public static double[] para_w_dev;
    public static double[] para_sq_dev;
    public static double[] para_sq;

    // --- Merkel 05/2010 variables ---
    public static double[] pressure = new double[7]; // 6 -> [7]
    public static double p, etelhycsx[] = new double[7], etelcsy[] = new double[7];
    public static double[] p_incr = new double[7]; // 6 -> [7]
    public static double pref, sref, tmp, tmpepsdev1, tmpepsdev2, toler;
    public static double angdetector, chi, eta, eps, eps_dev, eref, prodesc;

    // --- Scalar integer variables ---
    public static int idum, i, igset, ij, ip1, isw, j, n, n1, nd, nfamilyx, ng;
    public static int nind, ipl;

    /** Controls header printing in output files (static to persist) */
    public static boolean heading = true;
}