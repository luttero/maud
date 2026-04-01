package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_corotation module.
 *
 * This class holds static "work" arrays used by the corotation subroutine
 * for matrix/tensor conversions.
 *
 * All arrays are 1-based to match Fortran indexing.
 */
public class ModCorotation {

    public static double[] stcs6 = new double[7];
    public static double[][] stcs33 = new double[4][4];
    public static double[][] tmp33 = new double[4][4];
    public static double[] tmp6 = new double[7];

    public static double[] stss6 = new double[7];
    public static double[][] stss33 = new double[4][4];
    public static double[][] tmps33 = new double[4][4];
    public static double[] tmps6 = new double[7];

    public static double[][] aux66 = new double[7][7];
    public static double[][][][] aux3333 = new double[4][4][4][4];

}

