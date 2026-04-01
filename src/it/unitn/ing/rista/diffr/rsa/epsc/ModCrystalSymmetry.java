package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_crystal_symmetry module.
 *
 * Contains static fields for symmetry operations, unit cell vectors, etc.
 * All arrays are 1-based to match Fortran indexing.
 */
public class ModCrystalSymmetry {

    public static int nsymop, n, mn, h, isign, k, m, nr, nrot;
    
    /** Flag for redundant symmetry operations (0=unique, 1=redundant) */
    public static int[] itag = new int[25]; // 24 ops + 1 for 1-based index

    /** 3x3 Symmetry operation matrices [i][j][op_number] */
    public static double[][][] hh = new double[4][4][25];

    /** 3x3 Temporary/generator matrices for operations */
    public static double[][][] hx = new double[4][4][7]; // 6 generators + 1

    /** Unit cell dimensions (a, b, c) */
    public static double[] cdim = new double[4];

    /** Unit cell angles (alpha, beta, gamma) in radians */
    public static double[] cang = new double[4];

    /** Unit cell basis vectors [i][j] = component i of vector j */
    public static double[][] cvec = new double[4][4];

    // Scalar work variables
    public static double ang, sbnor, sndif, snnor;
}
