package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * Translation of the common_globals module.
 * All module variables are translated as public static fields.
 *
 * <p><b>Array Indexing Note:</b>
 * <br>
 * 1. <b>Allocatable Arrays</b> (e.g., {@code nsm, stw}):
 * These are declared as Java arrays (e.g., {@code int[], double[]}) and
 * initialized to {@code null}. They must be allocated (e.g., using
 * {@code Resize.resize_int1(...)}) before use.
 *
 * <br>
 * 2. <b>Fixed-Size Arrays</b> (e.g., {@code ccc2p0(6,6)}):
 * Fortran arrays are 1-indexed. To ease porting, these Java arrays are
 * declared with size {@code [N+1]} (e.g., {@code new double[7][7]}).
 * Index 0 is unused, and code can continue to access elements
 * using indices {@code [1]} through {@code [N]}.
 *
 * <br>
 * 3. <b>Mixed-Index Arrays</b> (e.g. {@code axisph(0:3,3)}):
 * This has indices (0,1,2,3) and (1,2,3).
 * It is translated as {@code new double[4][4]}, where
 * {@code [0..3]} is used for the first dimension and
 * {@code [1..3]} (ignoring {@code [i][0]}) is used for the second.
 */
public class CommonGlobals {

    // --- Integer Parameters (from comments) ---
    // These were commented out, but would be:
    public static int NGR = 10000;
    public static int NMOD = 10;
  public static int NSLS = 50;
  public static int NDIFFX = 10;
  public static int NPROCX = 5;
  public static int NPT = 30;

    // --- Allocatable Integer Arrays ---
    // (Initialized to null, must be allocated before use)
    public static int[] nsm, iTwinLevel, iTwinSys;
    public static int[][] iactSlip;
    public static int[] nact;
    public static int[][] iact;
    public static int[] ng_update, nactSlip, itw;
    public static int[] iParentGrain, iParentSystem, iParentMode;
    public static int[][] iChildGrain;
    public static int[] ngrset;
    public static int[][] igrset;
    public static int[][] mode_slip;
    public static int[] iSysMode;

    // --- Scalar Integers ---
    public static int nmodes, nsys, nslmod, nslsys, ntwmod, ntwsys, ipileup, ndiff;
    public static int nsteps, itmax_mod, itmax_grain, itexskip, ngrain;
    public static int iMaxTwinLevel, MaxTwins, ngParent, IncludeTS, iTotStep = 0;
    public static int ishape, irot, kCL;
    public static int itwinning, idetail, iwrite9, icvx, i_control_var;
    public static IntHolder jran = new IntHolder(-1);
    
    // --- Fixed-Size Integer Arrays (Note: [N+1] size for 1-based indexing) ---
    public static int[] istbc = new int[7];  // (6) -> [7]
    public static int[] ietbc = new int[7];  // (6) -> [7]
    public static int[][] ijv = new int[7][3]; // (6,2) -> [7][3]
    public static int[][] ifulletbc = new int[4][4]; // (3,3) -> [4][4]
    public static int[] i6 = new int[7];     // (6) -> [7]


    // --- Allocatable Double Arrays ---
    // (Initialized to null, must be allocated before use)
    public static double[] stw, tau0, tau1, thet0;
    public static double[][] ncc;
    public static double[] thet1;
    public static double[][][] qc2, ccs2;
    public static double[][][] mc2;
    public static double[][][] scs2;
    public static double[][][] mcs;
    public static double[][] alfacs;
    public static double[][][] qcs;
    public static double[] wgtset, wgtsetini;
    public static double[][] wgtgrset;
    public static double[] RAND_WGT;
    public static double[][][] ncs;
    public static double[][] gamd;
    public static double[][] stcs, strcs, stcsref, etcs, etrcs;
    public static double[][] etelcs, etelhycs;
    public static double[] gamtot;
    public static double[][][] acs2;
    public static double[][][] f;
    public static double[][] taud, tau, tau_update;
    public static double[] phi, the, ome, wgt;
    public static double[][][] r;
    public static double[][] h;
    public static double[][] ktwtag;
    public static double[] wgtx, link;
    public static double[][] etthcs;
    public static double[] vfrac_mod_acum, vfrac_mod;
    public static double[][] bcc;
    public static double[][][] bcs;
    public static double[] wgtd, TwinFrac, TwinCRSS, tau0_mode_c;
    public static double[] chiPoleFig, etaPoleFig;
    public static double[][][] fijgr;
    public static double[] meffc;
    public static double[][][] omegag;
    public static double[][][][][] EINVSAGR, ESCR4GR;
    public static double[][][] aefgr, aloca;
    public static double[][][][][] as;
    public static double[][][] axisgr; // Fortran was (0:3,3,NGR)
    public static double[] BURG, ACTENER, aK1, DRAG, rho_ini_for;
    public static double[] rho_ini_deb, tau0_mode_a, tau0_mode_b;
    public static double[] a_deb_a, a_deb_b, a_deb_c;
    public static double[][] TLATENT;
    public static double[] HPK0, HPK1, HPK2, edot_zero, tau_prop_a;
    public static double[] tau_crit_a, tau_crit_b, tau_crit_c;
    public static double[] tau_prop_b, tau_prop_c, shearmod;
    public static double[] burg_tw;
    public static double[][] hd;
    public static double[][] rho_for;
    public static double[] rho_deb, tau0_mode, a_deb;
    public static double[][] dnsa, dbsa;

    // --- Fixed-Size Double Arrays (Note: [N+1] size for 1-based indexing) ---
    public static double[][] ccc2p0 = new double[7][7]; // (6,6) -> [7][7]
    public static double[][] ccc2dp = new double[7][7]; // (6,6) -> [7][7]
    public static double[][] ccc2 = new double[7][7];   // (6,6) -> [7][7]
    public static double[][] scc2 = new double[7][7];   // (6,6) -> [7][7]
    public static double[] alfacc = new double[7];    // (6) -> [7]
    public static double[] stPGav = new double[7];    // (6) -> [7]
    public static double[] etPGav = new double[7];    // (6) -> [7]
    public static double[] stCHav = new double[7];    // (6) -> [7]
    public static double[] etCHav = new double[7];    // (6) -> [7]
    public static double[] stav = new double[7];      // (6) -> [7]
    public static double[] etav = new double[7];      // (6) -> [7]
    public static double[] etrav = new double[7];     // (6) -> [7]
    public static double[][][][] ESCR4 = new double[4][4][4][4]; // (3,3,3,3) -> [4][4][4][4]
    public static double[] eulerph = new double[4];   // (3) -> [4]
    public static double[][] css2 = new double[7][7];   // (6,6) -> [7][7]
    public static double[][] sss2 = new double[7][7];   // (6,6) -> [7][7]
    public static double[][] ass2 = new double[7][7];   // (6,6) -> [7][7]
    public static double[][] aef = new double[7][7];    // (6,6) -> [7][7]
    public static double[] stss = new double[7];      // (6) -> [7]
    public static double[] strss = new double[7];     // (6) -> [7]
    public static double[] etss = new double[7];      // (6) -> [7]
    public static double[] etrss = new double[7];     // (6) -> [7]
    public static double[] etelss = new double[7];    // (6) -> [7]
    public static double[] etssref = new double[7];   // (6) -> [7]
    public static double[] alfass = new double[7];    // (6) -> [7]
    public static double[] auxsample = new double[7]; // (6) -> [7]
    public static double[] stbc = new double[7];      // (6) -> [7]
    public static double[] strbc = new double[7];     // (6) -> [7]
    public static double[] etbc = new double[7];      // (6) -> [7]
    public static double[] etrbc = new double[7];     // (6) -> [7]
    public static double[][] fijph = new double[4][4];  // (3,3) -> [4][4]
    public static double[] profac = new double[7];    // (6) -> [7]
    public static double[][] invfac = new double[7][7]; // (6,6) -> [7][7]
    public static double[][] id2 = new double[7][7];    // (6,6) -> [7][7]
    public static double[][] etbc_sym = new double[4][4]; // (3,3) -> [4][4]
    public static double[][] omegabc = new double[4][4];  // (3,3) -> [4][4]
    public static double[][] omegabcr = new double[4][4]; // (3,3) -> [4][4]
    public static double[][] fulletbc = new double[4][4]; // (3,3) -> [4][4]
    public static double[] axis = new double[4];      // (3) -> [4]
    public static double[][][][] EINVSA = new double[4][4][4][4]; // (3,3,3,3) -> [4][4][4][4]
    public static double[] offset = new double[7];    // (6) -> [7]
    
    // Fortran (0:3, 3) -> Java [4][4] (use [0..3] and [1..3])
    public static double[][] axisph = new double[4][4];

    // --- Scalar Doubles ---
    public static double TVF = 0.0, CTVF = 0.0, kSM;
    public static double actav;
    public static double temp_s, temp_f, deltemp, error_mod;
    public static double chi_inter, q_rate, edot_macro, rho_avg_for, edot, rho_avg_deb;
}