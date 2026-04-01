package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_crss_voce module.
 *
 * Holds static variables for the Voce hardening model.
 * Arrays are 1-based to match Fortran indexing.
 */
public class ModCrssVoce {

    // Allocatable arrays
    public static double[] hselfx;
    public static double[][] hlatex;

    // Scalar parameters
    public static double tau0x, tau1x, thet0x, thet1x, twvol, gamdthres, fact, voce;

}

