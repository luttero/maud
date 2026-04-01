package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_sc_new module.
 *
 * This class holds static "work" arrays used by the sc_new self-consistent
 * solver. All arrays are 1-based to match Fortran indexing.
 */
public class ModScNew {

    // --- 6x6 Matrices ---
    public static double[][] e2i = new double[7][7];
    public static double[][] anew = new double[7][7];
    public static double[][] aux21 = new double[7][7];
    public static double[][] aux22 = new double[7][7];
    public static double[][] aux23 = new double[7][7];
    public static double[][] aux24 = new double[7][7];
    public static double[][] aux25 = new double[7][7];
    public static double[][] aux26 = new double[7][7];
    public static double[][] aux27 = new double[7][7];
    public static double[][] aux28 = new double[7][7];
    public static double[][] aux29 = new double[7][7];
    public static double[][] escr2 = new double[7][7];
    public static double[][] e2inv = new double[7][7];
    public static double[][] E2INVGRTEMP = new double[7][7];
    public static double[][] E2GRTEMP = new double[7][7];
    public static double[][] E2IGRTEMP = new double[7][7];

    // --- 4th Order Tensors (3x3x3x3) ---
    public static double[][][][] ass4 = new double[4][4][4][4];
    public static double[][][][] esim4 = new double[4][4][4][4];
    public static double[][][][] ESIM4TEMP = new double[4][4][4][4];
    public static double[][][][] ESCR4TEMP = new double[4][4][4][4];
    public static double[][][][] EINVSATEMP = new double[4][4][4][4];

    // --- Vectors ---
    public static double[] aux11 = new double[7];
    public static double[] aux12 = new double[7];
    public static double[] aux6 = new double[7];
    public static double[] aux34 = new double[7];
    public static double[] aux31 = new double[7];
    public static double[] aux32 = new double[7];
    public static double[] axb = new double[4];

    // --- 3x3 Matrices ---
    public static double[][] aux33 = new double[4][4];
    public static double[][] EIGB = new double[4][4];

    // --- Scalars ---
    public static double error, tmis, xmmin;

}
