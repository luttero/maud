package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_stiffness_rotation module.
 *
 * This class holds static "work" arrays for 4th-order tensor rotations.
 *
 * All arrays are 1-based to match Fortran indexing.
 */
public class ModStiffnessRotation {

    /** Stiffness tensor in grain/ellipsoid axes */
    public static double[][][][] ASS4GA = new double[4][4][4][4];
    
    /** Eshelby tensor in grain/ellipsoid axes */
    public static double[][][][] E4GA = new double[4][4][4][4];
    
    /** Anti-symmetric Eshelby tensor in grain/ellipsoid axes */
    public static double[][][][] AUX3333 = new double[4][4][4][4];

}

