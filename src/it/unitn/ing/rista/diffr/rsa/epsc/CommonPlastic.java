package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the common_plastic module.
 *
 * <p>All allocatable arrays are translated as public static fields,
 * initialized to null. They must be allocated before use.
 */
public class CommonPlastic {

    // Allocatable double arrays
    // (Initialized to null, must be allocated before use)
    public static double[] shear_mod_acum_ch;
    public static double[] shear_mod_acum_pa;
    public static double[] shear_mod_ch;
    public static double[] shear_dif_acum_ch;
    public static double[] shear_mod_pa;
    public static double[] shear_dif_acum_pa;
    public static double[] shear_mod_acum;
    public static double[] shear_mod;
    public static double[] shear_dif_acum;
    public static double[] WP;
    public static double[] WC;
    public static double[] aux;
    public static double[] aux_pa;
    public static double[] aux_ch;
}
