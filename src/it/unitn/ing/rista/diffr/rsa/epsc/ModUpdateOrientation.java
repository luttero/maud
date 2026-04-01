package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_update_orientation module.
 *
 * This class holds static "work" arrays used by the updateOrientation
 * subroutine for matrix and tensor calculations.
 *
 * All arrays are 1-based to match Fortran indexing.
 */
public class ModUpdateOrientation {

    // --- 3x3 Matrices ---
    public static double[][] rotslip = new double[4][4];
    public static double[][] AROTG = new double[4][4];
    public static double[][] ROTLOC = new double[4][4];
    public static double[][] rg = new double[4][4];
    public static double[][] rot = new double[4][4];
    public static double[][] DEV33 = new double[4][4];
    public static double[][] LIJGR0 = new double[4][4];

    // --- 6-element Vectors ---
    public static double[] DEV = new double[7];

    // --- 6x6 Matrices ---
    public static double[][] C2 = new double[7][7];

    // --- 4th Order Tensors ---
    public static double[][][][] C4 = new double[4][4][4][4];
}

