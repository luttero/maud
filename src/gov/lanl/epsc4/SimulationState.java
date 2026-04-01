package gov.lanl.epsc4;

import java.util.Scanner;
import java.util.List;

/**
 * Holds all sample-level (global) state for the simulation.
 * This replaces CommonGlobals, ModScNew, ModCorotation, etc.
 * All calculation methods that operate on this state are here.
 */
public class SimulationState {

    // --- Simulation Parameters ---
    public int NGR, NMOD, NSLS, NDIFFX, NPROCX;
    public int nmodes, nsys, nslmod, nslsys, ntwmod, ntwsys, ipileup, ndiff;
    public int nsteps, itmax_mod, itmax_grain, itexskip;
    public int iMaxTwinLevel, MaxTwins, ngParent, IncludeTS;
    public IntHolder jran = new IntHolder(-1); // Mutable seed
    public int iTotStep = 0;
    public int ishape, irot, kCL;
    public int itwinning = 0, idetail, iwrite9 = 0, icvx, i_control_var;

    public int[] istbc = new int[7];
    public int[] ietbc = new int[7];
    public int[][] ijv = new int[7][3];
    public int[][] ifulletbc = new int[4][4];
    public int[] i6 = new int[7];
    
    // --- Sample-Level Tensors (1-based) ---
    public double[][] ccc2p0 = new double[7][7];
    public double[][] ccc2dp = new double[7][7];
    public double[][] ccc2 = new double[7][7];
    public double[][] scc2 = new double[7][7];
    public double[] alfacc = new double[7];
    
    public double[] stPGav = new double[7];
    public double[] EtPGav = new double[7];
    public double[] stCHav = new double[7];
    public double[] etCHav = new double[7];
    public double TVF = 0.0, CTVF = 0.0;
    public double kSM;
    
    public double[] stav = new double[7];
    public double[] etav = new double[7];
    public double[][] axisph = new double[4][4]; // 0:3, 1:3
    public double[] eulerph = new double[4];
    public double[][][][] ESCR4 = new double[4][4][4][4];
    public double[] ETRAV = new double[7];
    
    public double[][] css2 = new double[7][7];
    public double[][] sss2 = new double[7][7];
    public double[][] ass2 = new double[7][7];
    public double[][] aef = new double[7][7];
    public double[] stss = new double[7];
    public double[] strss = new double[7];
    public double[] etss = new double[7];
    public double[] etrss = new double[7];
    public double[] etelss = new double[7];
    public double[] etssref = new double[7];
    public double[] alfass = new double[7];
    public double[] auxsample = new double[7];
    public double actav;
    
    public double temp_s, temp_f, deltemp;
    public double[] stbc = new double[7];
    public double[] strbc = new double[7];
    public double[] etbc = new double[7];
    public double[] etrbc = new double[7];
    public double error_mod;
    
    public double[][] fijph = new double[4][4];
    public double[] profac = new double[7];
    public double[][] invfac = new double[7][7];
    public double[][] id2 = new double[7][7];
    
    public double[][] etbc_sym = new double[4][4];
    public double[][] omegabc = new double[4][4];
    public double[][] omegabcr = new double[4][4];
    public double[][] fulletbc = new double[4][4];
    public double[] axis = new double[4];
    public double[][][][] EINVSA = new double[4][4][4][4];
    public double[] offset = new double[7];
    
    public double chi_inter, Q_rate, edot_macro, rho_avg_for, edot, rho_avg_deb;
    
    // --- Mode-level Arrays (from common_globals) ---
    public double[] stw, tau0_mode_c, BURG, ACTENER, aK1, DRAG, rho_ini_for;
    public double[] rho_ini_deb, tau0_mode_a, tau0_mode_b;
    public double[] a_deb_a, a_deb_b, a_deb_c;
    public double[][] TLATENT;
    public double[] HPK0, HPK1, HPK2, edot_zero, tau_prop_a;
    public double[] tau_crit_a, tau_crit_b, tau_crit_c;
    public double[] tau_prop_b, tau_prop_c, shearmod;
    public double[] burg_tw, tau0_mode, a_deb;
    public int[] nsm, itw;
    public double[] vfrac_mod_acum, vfrac_mod, TwinFrac, TwinCRSS;
    
    // --- System-level Arrays (from common_globals) ---
    public int[] iTwinSys, iSysMode;
    public int[][] mode_slip;
    public double[] tau0, tau1, thet0, thet1;
    public double[][] ncc, mc2, h, bcc, hd, dnsa, dbsa;
    public double[][][] qc2;

    // --- Work variables from modules (OOP refactored) ---
    // From ModScNew
    private double[][] e2i = new double[7][7], anew = new double[7][7];
    private double[][] aux21 = new double[7][7], aux22 = new double[7][7];
    private double[][] aux23 = new double[7][7], aux24 = new double[7][7];
    private double[][] aux25 = new double[7][7], aux26 = new double[7][7];
    private double[][] aux27 = new double[7][7], aux28 = new double[7][7];
    private double[][] aux29 = new double[7][7];
    private double[] aux11 = new double[7], aux12 = new double[7];
    private double[][][][] ass4 = new double[4][4][4][4];
    private double[][][][] esim4 = new double[4][4][4][4];
    private double[] axb = new double[4];
    private double[][] EIGB = new double[4][4];

    // From ModUpdateFij
    private double[][] FNEW_upd = new double[4][4];
    private double[][] etrss_33_upd = new double[4][4];
    private double[] etrcs6_upd = new double[7];
    private double[][] etrcs_33_upd = new double[4][4];
    private int[][] XID3_upd = new int[4][4];

    // From ModCorotation
    private double[] stcs6_cor = new double[7], tmp6_cor = new double[7];
    private double[][] stcs33_cor = new double[4][4], tmp33_cor = new double[4][4];
    private double[] stss6_cor = new double[7], tmps6_cor = new double[7];
    private double[][] stss33_cor = new double[4][4], tmps33_cor = new double[4][4];
    private double[][] aux66_cor = new double[7][7];
    private double[][][][] aux3333_cor = new double[4][4][4][4];
    
    // --- Helper Methods ---

    /**
     * Initializes global constants from the main program's DATA block.
     * This *must* be called once before any other calculations.
     */
    public void initializeGlobals() {
        int[][] ijvx_data = {
            {0, 0}, {1, 1}, {2, 2}, {3, 3}, {2, 3}, {1, 3}, {1, 2}
        };

        for (int i = 1; i <= 6; i++) {
            i6[i] = 0;
            ijv[i][1] = ijvx_data[i][0];
            ijv[i][2] = ijvx_data[i][1];
            profac[i] = (i <= 3) ? 1.0 : 2.0;

            for (int j = 1; j <= 6; j++) {
                double diag_val = (i <= 3) ? 1.0 : 0.5;
                id2[i][j] = (i == j) ? diag_val : 0.0;
                
                double i_fact = (i <= 3) ? 1.0 : 2.0;
                double j_fact = (j <= 3) ? 1.0 : 2.0;
                invfac[i][j] = 1.0 / (i_fact * j_fact);
            }
        }
    }
    
    /**
     * Allocates all arrays that depend on NMOD.
     */
    public void allocateForNMOD() {
        nsm = new int[NMOD + 1];
        itw = new int[NMOD + 1];
        stw = new double[NMOD + 1];
        vfrac_mod_acum = new double[NMOD + 1];
        vfrac_mod = new double[NMOD + 1];
        TwinFrac = new double[NMOD + 1];
        TwinCRSS = new double[NMOD + 1];
        tau0_mode_c = new double[NMOD + 1];
        BURG = new double[NMOD + 1];
        ACTENER = new double[NMOD + 1];
        aK1 = new double[NMOD + 1];
        DRAG = new double[NMOD + 1];
        rho_ini_for = new double[NMOD + 1];
        rho_ini_deb = new double[NMOD + 1];
        tau0_mode_a = new double[NMOD + 1];
        tau0_mode_b = new double[NMOD + 1];
        a_deb_a = new double[NMOD + 1];
        a_deb_b = new double[NMOD + 1];
        a_deb_c = new double[NMOD + 1];
        TLATENT = new double[NMOD + 1][NMOD + 1];
        HPK0 = new double[NMOD + 1];
        HPK1 = new double[NMOD + 1];
        HPK2 = new double[NMOD + 1];
        edot_zero = new double[NMOD + 1];
        tau_prop_a = new double[NMOD + 1];
        tau_crit_a = new double[NMOD + 1];
        tau_crit_b = new double[NMOD + 1];
        tau_crit_c = new double[NMOD + 1];
        tau_prop_b = new double[NMOD + 1];
        tau_prop_c = new double[NMOD + 1];
        java.util.Arrays.fill(tau_prop_c, 1.0); // Special case
        shearmod = new double[NMOD + 1];
        burg_tw = new double[NMOD + 1];
        tau0_mode = new double[NMOD + 1];
        a_deb = new double[NMOD + 1];
    }
    
    /**
     * Allocates all arrays that depend on NSLS.
     */
    public void allocateForNSLS() {
        iTwinSys = new int[NSLS + 1];
        iSysMode = new int[NSLS + 1];
        tau0 = new double[NSLS + 1];
        tau1 = new double[NSLS + 1];
        thet0 = new double[NSLS + 1];
        ncc = new double[4][NSLS + 1]; // 3xNSLS
        thet1 = new double[NSLS + 1];
        qc2 = new double[4][4][NSLS + 1]; // 3x3xNSLS
        mc2 = new double[4][4][NSLS + 1]; // 3x3xNSLS
        h = new double[NSLS + 1][NSLS + 1];
        bcc = new double[4][NSLS + 1]; // 3xNSLS
        hd = new double[NSLS + 1][NSLS + 1];
        dnsa = new double[4][NSLS + 1]; // 3xNSLS
        dbsa = new double[4][NSLS + 1]; // 3xNSLS
        mode_slip = new int[NMOD + 1][NSLS + 1];
    }

    /**
     * Solves the self-consistent equation for the sample elasto-plastic 'ass2'
     * and thermal 'alfass' moduli.
     * Translation of Fortran SUBROUTINE sc_new.
     */
    public boolean scNew(int iopt, int liter, int interaction, int istep, List<Grain> grains, EshelbyTensorCalculator eshelby) {
        
        int iguess = 1;
        int niter = (iopt == 0) ? itmax_mod : 1;
        boolean converged = false;

        while (iguess <= niter) {
            
            // --- Calc. Eshelby tensor 'e2' ---
            TensorUtils.voigt(aux6, aux33, ass2, ass4, 3);
            updateShape(grains); 
            
            for (int j = 1; j <= 3; j++) {
                axb[j] = axisph[0][j];
                for (int i = 1; i <= 3; i++) EIGB[i][j] = axisph[i][j];
            }
            
            stiffnessRotation(ass4, EIGB, axb, esim4, ESCR4, eshelby);
            TensorUtils.voigt(aux6, aux33, e2, esim4, 4);
            TensorUtils.invten(e2, e2inv);
            TensorUtils.voigt(aux6, aux33, e2inv, EINVSA, 3);
            
            if (ishape >= 2) {
                // ... (Logic for per-grain shape) ...
                // This logic seems flawed in the original Fortran, as it only
                // calculates aefgr for the *last* grain. Replicating that.
                int igr = NGR; 
                Grain g = grains.get(igr - 1); // 0-based list
                for (int j = 1; j <= 3; j++) {
                    axb[j] = g.axisgr[0][j];
                    for (int i = 1; i <= 3; i++) EIGB[i][j] = g.axisgr[i][j];
                }
                
                double[][][][] esim4_temp = new double[4][4][4][4];
                double[][][][] escr4_temp = new double[4][4][4][4];
                stiffnessRotation(ass4, EIGB, axb, esim4_temp, escr4_temp, eshelby);
                
                double[][] e2gr_temp = new double[7][7];
                double[][] e2invgr_temp = new double[7][7];
                TensorUtils.voigt(aux6, aux33, e2gr_temp, esim4_temp, 4);
                TensorUtils.invten(e2gr_temp, e2invgr_temp);
                TensorUtils.voigt(aux6, aux33, e2invgr_temp, g.EINVSAGR, 3);

                for (int i=1; i<=3; i++) for (int j=1; j<=3; j++) for (int k=1; k<=3; k++) for (int l=1; l<=3; l++)
                    g.ESCR4GR[i][j][k][l] = escr4_temp[i][j][k][l];

                for (int i = 1; i <= 6; i++) {
                    for (int j = 1; j <= 6; j++) {
                        g.aefgr[i][j] = 0.0;
                        for (int k = 1; k <= 6; k++) {
                            g.aefgr[i][j] += ass2[i][k] * (e2invgr_temp[k][j] - id2[k][j]) * profac[k];
                        }
                    }
                }
            } 

            // --- Calc. effective stiffness: aef = ass2 * ( e2**(-1) - I ) ---
            TensorUtils.invten(e2, e2i);
            for (int i = 1; i <= 6; i++) {
                for (int j = 1; j <= 6; j++) {
                    aef[i][j] = 0.0;
                    for (int k = 1; k <= 6; k++) {
                        aef[i][j] += ass2[i][k] * (e2i[k][j] - id2[k][j]) * profac[k];
                    }
                }
            }
            
            // --- Defines an effective compliance for each grain 'meffc(ng)' ---
            double tmis = tmismatch(ass2, css2, 6, 6);
            double xmmin = 1.0; // HARD-WIRED SECANT
            
            mEffective(grains, xmmin);
            
            // --- Calculates localization and elasto-plastic stiffness tensors ---
            for (int i = 1; i <= 6; i++) {
                aux11[i] = 0.0;
                for (int j = 1; j <= 6; j++) {
                    aux22[i][j] = 0.0; // < acs2 * aloca >
                    aux26[i][j] = 0.0; // < aloca >
                    aux29[i][j] = 0.0; // < (ass2+aef) * (acs2+aef)^-1 >
                }
            }

            // --- STARTS LOOP OVER GRAINS ---
            for (Grain g : grains) {
                if (g.wgt > 0) {
                    for (int i = 1; i <= 6; i++) {
                        for (int j = 1; j <= 6; j++) {
                            if (ishape == 0 || ishape == 1) {
                                aux24[i][j] = g.acs2[i][j] + g.meffc * aef[i][j];
                                aux25[i][j] = ass2[i][j] + g.meffc * aef[i][j];
                            } else {
                                aux24[i][j] = g.acs2[i][j] + g.meffc * g.aefgr[i][j];
                                aux25[i][j] = ass2[i][j] + g.meffc * g.aefgr[i][j];
                            }
                        }
                    }

                    TensorUtils.invten(aux24, aux21); // aux21 = (acs2+aef)^-1
                    
                    // aux23 = aloca = (acs2+aef)^-1 * (ass2+aef)
                    for (int i = 1; i <= 6; i++) {
                        for (int j = 1; j <= 6; j++) {
                            aux23[i][j] = 0.0;
                            for (int k = 1; k <= 6; k++) {
                                aux23[i][j] += aux21[i][k] * aux25[k][j] * profac[k];
                            }
                        }
                    }

                    // Accumulate weighted averages
                    for (int i = 1; i <= 6; i++) {
                        for (int j = 1; j <= 6; j++) {
                            aux26[i][j] += aux23[i][j] * g.wgt; // <aloca>
                            for (int k = 1; k <= 6; k++) {
                                aux22[i][j] += g.acs2[i][k] * aux23[k][j] * profac[k] * g.wgt;
                            }
                        }
                    }

                    // --- Aux tensors for thermal tensor ---
                    if (iopt != 1) {
                        for (int i = 1; i <= 6; i++) {
                            for (int j = 1; j <= 6; j++) {
                                aux28[i][j] = 0.0;
                                for (int k = 1; k <= 6; k++) {
                                    aux28[i][j] += aux25[i][k] * aux21[k][j] * profac[k];
                                }
                            }
                        }
                        for (int i = 1; i <= 6; i++) {
                            aux12[i] = 0.0;
                            for (int j = 1; j <= 6; j++) {
                                for (int k = 1; k <= 6; k++) {
                                    aux12[i] += aux28[i][j] * g.acs2[j][k] * g.alfacs[k] * profac[j] * profac[k];
                                }
                            }
                        }
                        for (int i = 1; i <= 6; i++) {
                            aux11[i] += aux12[i] * g.wgt;
                            for (int j = 1; j <= 6; j++) {
                                aux29[i][j] += aux28[i][j] * g.wgt;
                            }
                        }
                    } 
                } 
            } // --- ENDS LOOP OVER GRAINS ---
            
            TensorUtils.invten(aux26, aux27); // aux27 = <aloca>^-1
            
            // anew = <acs2*aloca> * <aloca>^-1
            for (int i = 1; i <= 6; i++) {
                for (int j = 1; j <= 6; j++) {
                    anew[i][j] = 0.0;
                    for (int k = 1; k <= 6; k++) {
                        anew[i][j] += aux22[i][k] * aux27[k][j] * profac[k];
                    }
                }
            }

            // Enforce symmetry
            for (int i = 1; i <= 6; i++) {
                for (int j = i + 1; j <= 6; j++) {
                    anew[i][j] = 0.5 * (anew[i][j] + anew[j][i]);
                    anew[j][i] = anew[i][j];
                }
            }

            // --- Check convergence ---
            double error = tmismatch(ass2, anew, 6, 6);
            for (int i = 1; i <= 6; i++) {
                System.arraycopy(anew[i], 1, ass2[i], 1, 6);
            }

            // --- Print iteration status ---
            int iter_num = (iopt == 0) ? iguess : liter;
            if (IOUtils.writer11 != null) IOUtils.writer11.printf("For iteration = %3d the error is %12.4e%n", iter_num, error);
            System.out.printf("ITER: %3d   ERROR: %12.4e   NG: %5d%n", iter_num, error, NGR);
            
            // --- Control loop (iguess) ---
            converged = (error <= error_mod);
            if (iopt == 0) { // Main iteration loop
                if (converged) {
                    iguess = niter + 1; // Exit loop
                } else if (iguess == niter) {
                    throw new RuntimeException("CONVERGENCE IN SUBROUTINE SC NOT ACHIEVED AFTER " + niter + " ITERATIONS");
                }
                iguess++;
            } else { // Single iteration
                if (converged && liter > 1) {
                    iguess = niter + 1; // Exit
                } else {
                    iguess++;
                }
            }
        } // --- Closes DO WHILE (IGUESS.LE.NITER) ---

        // --- Evaluates the sc thermal expansion tensor ---
        if (iopt != 1) {
            TensorUtils.invten(aux29, aux24);
            TensorUtils.invten(anew, aux22); // anew is the converged ass2
            for (int i = 1; i <= 6; i++) {
                alfass[i] = 0.0;
                for (int j = 1; j <= 6; j++) {
                    for (int k = 1; k <= 6; k++) {
                        alfass[i] += aux22[i][j] * aux24[j][k] * aux11[k] * profac[j] * profac[k];
                    }
                }
            }
        }
        return converged;
    }
    
    // ... (Other methods: gAverage, sState, updateOrientation, corotation, etc.)
    // ... (These methods would be moved here from CalculationRoutines)
    // ... (They would be modified to take `List<Grain> grains` as a parameter)
}
