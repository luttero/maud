package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_cr_to_sa module.
 *
 * This class holds static "work" arrays used during tensor rotations.
 * They are populated by the 'voigt' function and then used in the
 * 'crToSa' rotation calculations.
 *
 * All arrays use 1-based indexing for easier porting.
 */
public class ModCrToSa {

    // 3x3x3x3 stiffness tensor (from ccc2)
    public static double[][][][] ccc4 = new double[4][4][4][4];

    // 3x3x3x3 compliance tensor (from scc2)
    public static double[][][][] scc4 = new double[4][4][4][4];

    // 3x3 thermal expansion tensor (from alfacc)
    public static double[][] alfacc2 = new double[4][4];

    // Dummy arrays for voigt calls (to match Fortran signature)
    public static double[] T1 = new double[7];
    public static double[][] T2 = new double[4][4];
    public static double[][] C2 = new double[7][7];
    public static double[][][][] C4 = new double[4][4][4][4];
    
}
