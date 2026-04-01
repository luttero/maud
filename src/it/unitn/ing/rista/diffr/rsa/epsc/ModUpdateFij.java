package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the mod_update_fij module.
 *
 * This class holds static "work" arrays used by the updateFij subroutine
 * for matrix calculations.
 *
 * All arrays are 1-based to match Fortran indexing.
 */
public class ModUpdateFij {

    /** 3x3 work matrix for new deformation gradient */
    public static double[][] FNEW = new double[4][4];
    
    /** 3x3 macroscopic strain rate tensor */
    public static double[][] etrss_33 = new double[4][4];
    
    /** 6-element grain strain rate vector */
    public static double[] etrcs6 = new double[7];
    
    /** 3x3 grain strain rate tensor */
    public static double[][] etrcs_33 = new double[4][4];

    /** 6x6 dummy matrix for voigt call */
    public static double[][] C2 = new double[7][7];
    
    /** 3x3x3x3 dummy tensor for voigt call */
    public static double[][][][] C4 = new double[4][4][4][4];
    
    /** 3x3 identity matrix */
    public static int[][] XID3 = new int[4][4];
}

