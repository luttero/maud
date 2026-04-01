package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the (inferred) mod_crss_disloc_dens module.
 *
 * This class holds the persistent arrays that are allocated in one
 * ioption (0) and used in others (2, 3).
 */
public class ModCrssDislocDens {

    /**
     * d(rho_forest) / d(gamma)
     * Allocated in crssDislocDens(ioption=0)
     */
    public static double[] Drho_for_Dgamma;

    /**
     * d(rho_debris) / d(gamma)
     * Allocated in crssDislocDens(ioption=0)
     */
    public static double[] Drho_deb_Dgamma;

}