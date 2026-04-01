package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_pileup module.
 *
 * Holds static variables for dislocation pileup calculations.
 * Arrays are 1-based to match Fortran indexing.
 */
public class ModPileup {

    // --- Allocatable arrays ---
    public static double[] gamrec;
    public static double[] gamrev;
    public static double[] rsspr;
    public static int[] iprsys;
    public static int[] irec;

    // --- Scalar variables ---
    public static int ipoly;
    public static double dgamrecx, drssprx, frac_bs, gamdmax, grecmax, rss, tau_bs;

}

