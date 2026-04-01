package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_data_crystal module.
 *
 * This class holds static "work" variables used by the data_crystal subroutine
 * during the parsing of the crystal file.
 *
 * All arrays are 1-based to match Fortran indexing.
 */
public class ModDataCrystal {

    // --- Vectors/Matrices for crystal_symmetry calls ---
    public static double[] sn = new double[4];
    public static double[][] sneq = new double[4][25];
    public static double[] sb = new double[4];
    
    // --- Scalar work variables ---
    public static double da, db, dc, prod, stwx;

    // --- Miller-Bravais indices ---
    public static int[] isn = new int[5]; // 4-element + 1
    public static int[] isb = new int[5];
    public static int[] ipole = new int[5];

    // --- Matrix work variables ---
    public static int[][] fijx = new int[4][4];
    public static int[][] fnew = new int[4][4];

    /** Allocatable array for modes to be used */
    public static int[] mode;

    // --- Dummy/Work arrays for tensor/vector ops ---
    public static double[] aux6 = new double[7];
    public static double[][] aux33 = new double[4][4];
    public static double[][] aaa = new double[4][4];
    public static double[][][][] ccc4 = new double[4][4][4][4];

    // --- Local scalar variables ---
    public static int unit, im, iloop, i, j, idum, iopsysx, isys, itwx, k, m, modex;
    public static int nind, nmodesx, nrsx, npoles, nsmx, nst, l;
    
    // AllocateStatus is handled by Java exceptions
}
