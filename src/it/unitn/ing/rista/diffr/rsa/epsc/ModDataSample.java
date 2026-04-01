package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_data_sample module.
 *
 * This class holds static "work" arrays used by the data_sample subroutine.
 * All arrays are 1-based to match Fortran indexing.
 */
public class ModDataSample {

    /** 3x3 work matrix for Euler rotations */
    public static double[][] aux33 = new double[4][4];
    
    /** 3x3 work matrix */
    public static double[][] aux = new double[4][4];
    
    /** 3-element work vector */
    public static double[] bur = new double[4];
    
    /** Sum of all grain weights */
    public static double totwgt;
}
