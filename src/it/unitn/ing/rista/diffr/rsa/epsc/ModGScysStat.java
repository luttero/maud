package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_g_scys_stat module.
 *
 * Holds static variables for tracking statistics about
 * Single Crystal Yield Surface (SCYS) updates.
 * Arrays are 1-based to match Fortran indexing.
 */
public class ModGScysStat {

    public static int ng_up_tot;
    public static double tau_up_max;
    public static double wgt_up_tot;

    /** Weight of updated grains in integer bins (0 to 100) */
    public static double[] wgt_up_int; // allocatable
    
    /** Maximum tau_update value for each grain */
    public static double[] tau_update_max; // allocatable
}
