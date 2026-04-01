package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_back_stress module.
 * Contains static fields for back-stress calculations.
 */
public class ModBackStress {
    // Allocatable arrays (initially null, must be allocated)
    public static double[] gambs;
    public static double[] gmod;
    public static int[] iprsys;

    // Scalar doubles
    public static double percent_gam;
    public static double percent_tau;
    public static double gambsmax;
    public static double gamdbs;
    public static double gamdmax;
    public static double rss;
    public static double rssd;
}