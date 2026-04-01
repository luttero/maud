package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the common_eshelby module.
 *
 * <p><b>Array Indexing Note:</b>
 * Arrays are sized [N+1] or as specified to allow for
 * 1-based or 0-based Fortran indexing.
 */
public class CommonEshelby {

    // Parameters
    public static final int ngaumx = 16;
    public static final int ngaumx2 = 256;

    // (N) -> [N+1] for 1-based indexing
    // (0:N) -> [N+1] for 0-based indexing
    
    public static int[] ngaussph = new int[13]; // (12) -> [13]
    public static int[] ngaussth = new int[13];  // (12) -> [13]

    public static double[][][] alpha = new double[13][4][ngaumx2 + 1]; // (12,3,ngaumx2)
    public static double[][][] aa1 = new double[13][7][ngaumx2 + 1];   // (12,6,ngaumx2)
    public static double[][][] aww = new double[13][4][ngaumx2 + 1];   // (12,3,ngaumx2)
    public static double[][][] aaww1 = new double[13][7][ngaumx2 + 1]; // (12,6,ngaumx2)
    public static double[][] ww = new double[13][ngaumx2 + 1];         // (12,ngaumx2)

    public static double[][] punti = new double[11][12]; // (10,11) -> [11][12]
    public static double[][] pesi = new double[11][12];  // (10,11) -> [11][12]
    public static double[] dte = new double[11];         // (0:10) -> [11]
    
    public static double[] puntigl = new double[17]; // (16) -> [17]
    public static double[] pesigl = new double[17];  // (16) -> [17]

    public static double[][] p = new double[4][4];     // (3,3) -> [4][4]
    public static double[][] pesh = new double[4][4];  // (3,3) -> [4][4]
    public static double[][] desh = new double[4][4];  // (3,3) -> [4][4]
    public static double[][] c2 = new double[7][7];    // (6,6) -> [7][7]
    public static double[][] gamma2 = new double[7][7]; // (6,6) -> [7][7]
    public static double[][][][] gamma4 = new double[4][4][4][4]; // (3,3,3,3) -> [4][4][4][4]

    public static double[] x1 = new double[11];    // (10) -> [11]
    public static double[] a1 = new double[11];    // (10) -> [11]
    public static double[] a1inv = new double[11]; // (10) -> [11]

    public static double[] aa1x = new double[7];     // (6) -> [7]
    public static double[][] aa2x = new double[4][4];  // (3,3) -> [4][4]
    public static double[] aaww1x = new double[7];   // (6) -> [7]
    public static double[][] aaww2x = new double[4][4]; // (3,3) -> [4][4]

    public static double[] xph = new double[ngaumx + 1]; // (ngaumx) -> [ngaumx+1]
    public static double[] xth = new double[ngaumx + 1]; // (ngaumx) -> [ngaumx+1]
    public static double[] wph = new double[ngaumx + 1]; // (ngaumx) -> [ngaumx+1]
    public static double[] wth = new double[ngaumx + 1]; // (ngaumx) -> [ngaumx+1]

    public static double abc, abcoro3, costh, dumscr, dumsim, pdil, ratio1, ratio2, ro3, simbtet, sinth;

    public static int IGAUSSLEG, npoints;
}