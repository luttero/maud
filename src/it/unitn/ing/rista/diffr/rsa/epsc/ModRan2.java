package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_ran2 module.
 *
 * Holds the static state variables for the ran2 random number generator.
 */
public class ModRan2 {

    public static final int NTAB = 32;

    public static int idum2 = 123456789;
    public static int iy = 0;
    public static int[] iv = new int[NTAB + 1]; // 1-based indexing

}

