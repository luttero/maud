package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_g_state module.
 *
 * Contains 1-based static "work" arrays used by the g_state subroutine
 * for matrix calculations.
 */
public class ModGState {

    public static double[] aux11 = new double[7];
    public static double[][] aux21 = new double[7][7];
    public static double[][] aux22 = new double[7][7];
    public static double[] aux6 = new double[7];

}
