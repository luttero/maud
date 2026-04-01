package gov.lanl.epsc4;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;

/**
 * Contains methods for the core physics calculations of the simulation,
 * such as rotations, twinning, and orientation updates.
 * This class is non-static and holds work arrays for its methods.
 */
public class Physics {

    // --- Work arrays for cr_to_sa ---
    private double[][][][] ccc4_cr = new double[4][4][4][4];
    private double[][][][] scc4_cr = new double[4][4][4][4];
    private double[][] alfacc2_cr = new double[4][4];
    private double[] T1_cr = new double[7];
    private double[][] T2_cr = new double[4][4];
    private double[][] C2_cr = new double[7][7];
    private double[][][][] C4_cr = new double[4][4][4][4];

    // --- Work arrays for twinor ---
    private double[][] hpi_tw = new double[4][4];
    private double[][] htw_tw = new double[4][4];
    private double[][] aux_tw = new double[4][4];
    private double[][] atw_tw = new double[4][4];

    // --- Work arrays for update_orientation ---
    private double[][] rotslip_upd = new double[4][4];
    private double[][] AROTG_upd = new double[4][4];
    private double[][] ROTLOC_upd = new double[4][4];
    private double[][] rg_upd = new double[4][4];
    private double[][] rot_upd = new double[4][4];
    private double[][] DEV33_upd = new double[4][4];
    private double[][] LIJGR0_upd = new double[4][4];
    private double[] DEV_upd = new double[7];
    private double[][] C2_upd = new double[7][7];
    private double[][][][] C4_upd = new double[4][4][4][4];

    // --- Work arrays for crystal_symmetry & poleFig ---
    // (These are static because the symmetry operators are global constants)
    private static int nsymop;
    private static int[][][] hh = new int[4][4][25];
    private static double[][][] hx = new double[4][4][7];
    private static double[][] cvec = new double[4][4];

    // --- Work arrays for poleFig ---
    private double[] ps_pf = new double[4];
    private double[] etelcsx_pf = new double[7];
    private double[] vs_pf = new double[4];
    private double[] sb_pf = new double[4];
    private double[] sn_pf = new double[4];
    private double[][] sneq_pf = new double[4][25];
    private int[] itag_pf = new int[25];

    // --- Work arrays for calcChildStress ---
    private double[] stParent_ccs = new double[7];
    private double[] etelParent_ccs = new double[7];
    private double[] stChild_ccs = new double[7];
    private double[] etelChild_ccs = new double[7];
    private double[][] aChild_ccs = new double[7][7];
    private double[][] aux66_ccs = new double[7][7];
    private double[] aux6_ccs = new double[7];
    private double[][] rTW2SA_ccs = new double[4][4];
    private double[][] rSA2TW_ccs = new double[4][4];
    private double[][] aux33_ccs = new double[4][4];
    private double[][][][] aux3333_ccs = new double[4][4][4][4];
    private int[] indx_ccs = new int[7];


    public Physics() {
        // Initialize the twin rotation matrix (180 deg about Z)
        hpi_tw[1][1] = -1.0; hpi_tw[1][2] = 0.0;  hpi_tw[1][3] = 0.0;
        hpi_tw[2][1] = 0.0;  hpi_tw[2][2] = -1.0; hpi_tw[2][3] = 0.0;
        hpi_tw[3][1] = 0.0;  hpi_tw[3][2] = 0.0;  hpi_tw[3][3] = 1.0;
    }

    /**
     * Calculates the Voigt, Reuss, and Hill averages for the elastic moduli.
     */
    public void avModulus(SimulationState state, List<Grain> grains) {
        double[][] css2vo = new double[7][7];
        double[][] sss2re = new double[7][7];
        double[][] sss2vo = new double[7][7];
        double[][] css2re = new double[7][7];

        // Weighted sum loop
        for (Grain g : grains) {
            for (int i = 1; i <= 6; i++) {
                for (int j = 1; j <= 6; j++) {
                    css2vo[i][j] += g.wgt * g.ccs2[i][j];
                    sss2re[i][j] += g.wgt * g.scs2[i][j];
                }
            }
        }

        TensorUtils.invten(css2vo, sss2vo, state.invfac);
        
        PrintWriter out = IOUtils.writer11;
        if (out == null) return;
        
        out.println("ELASTIC PROPERTIES AVERAGE:");
        IOUtils.printMatrix(out, " VOIGT average stiffness matrix", css2vo);
        IOUtils.printMatrix(out, " VOIGT average compliance matrix", sss2vo);

        TensorUtils.invten(sss2re, css2re, state.invfac);
        IOUtils.printMatrix(out, " REUSS average stiffness matrix", css2re);
        IOUtils.printMatrix(out, " REUSS average compliance matrix", sss2re);

        // Calculate Hill average and store in global state
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                state.css2[i][j] = (css2vo[i][j] + css2re[i][j]) / 2.0;
                state.ass2[i][j] = state.css2[i][j];
            }
        }
        
        IOUtils.printMatrix(out, " HILL average stiffness matrix", state.css2);
        TensorUtils.invten(state.css2, state.sss2, state.invfac);
        IOUtils.printMatrix(out, " HILL average compliance matrix", state.sss2);
        out.println();
        out.println("******************************************************************************");
    }

    /**
     * Updates the deformation gradient tensors (fijph and fijgr).
     */
    public void updateFij(double step, SimulationState state, List<Grain> grains) {
        // --- Initialize 3x3 Identity Matrix ---
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                ModUpdateFij.XID3[i][j] = (i == j) ? 1 : 0;
            }
        }

        // --- 1. Update the average phase deformation gradient 'fijph' ---
        TensorUtils.voigt(state.etrss, ModUpdateFij.etrss_33, ModUpdateFij.C2, ModUpdateFij.C4, 1);
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                ModUpdateFij.FNEW[i][j] = 0.0;
                for (int k = 1; k <= 3; k++) {
                    ModUpdateFij.FNEW[i][j] += (ModUpdateFij.etrss_33[i][k] * step + ModUpdateFij.XID3[i][k])
                                            * state.fijph[k][j];
                }
            }
        }
        for (int i = 1; i <= 3; i++) {
            System.arraycopy(ModUpdateFij.FNEW[i], 1, state.fijph[i], 1, 3);
        }

        // --- 2. Update individual grain deformation gradients 'fijgr' ---
        if (state.ishape >= 2) {
            for (Grain g : grains) {
                for (int i = 1; i <= 6; i++) {
                    ModUpdateFij.etrcs6[i] = g.etrcs[i];
                }
                TensorUtils.voigt(ModUpdateFij.etrcs6, ModUpdateFij.etrcs_33, ModUpdateFij.C2, ModUpdateFij.C4, 1);
                
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                        ModUpdateFij.FNEW[i][j] = 0.0;
                        for (int k = 1; k <= 3; k++) {
                            ModUpdateFij.FNEW[i][j] += (ModUpdateFij.etrcs_33[i][k] * step + ModUpdateFij.XID3[i][k])
                                                    * g.fijgr[k][j];
                        }
                    }
                }
                for (int i = 1; i <= 3; i++) {
                    System.arraycopy(ModUpdateFij.FNEW[i], 1, g.fijgr[i], 1, 3);
                }
            }
        }
    }

    /**
     * Rotates material properties from Crystal (CR) to Sample (SA) frame.
     */
    public void crToSa(int ng1, int ng2, int iopt, SimulationState state, List<Grain> grains) {
        // iopt 0: Rotate Schmid tensors, thermal tensor, Burgers vector
        if (iopt == 0) {
            TensorUtils.voigt(state.alfacc, alfacc2_cr, C2_cr, C4_cr, 1);
            
            for (int ng_idx = ng1 - 1; ng_idx < ng2; ng_idx++) {
                Grain g = grains.get(ng_idx);
                // Rotation of Schmid tensor
                for (int ns = 1; ns <= state.nsys; ns++) {
                    for (int ij = 1; ij <= 6; ij++) {
                        g.mcs[ij][ns] = 0.0;
                        g.qcs[ij][ns] = 0.0;
                        int i = state.ijv[ij][1];
                        int j = state.ijv[ij][2];
                        for (int i1 = 1; i1 <= 3; i1++) {
                            for (int j1 = 1; j1 <= 3; j1++) {
                                g.mcs[ij][ns] += g.r[i1][i] * g.r[j1][j] * state.mc2[i1][j1][ns];
                                g.qcs[ij][ns] += g.r[i1][i] * g.r[j1][j] * state.qc2[i1][j1][ns];
                            }
                        }
                    }
                }
                // Burgers vector BCS and normal NCS
                for (int ns = 1; ns <= state.nsys; ns++) {
                    for (int i = 1; i <= 3; i++) {
                        g.bcs[i][ns] = 0.0;
                        g.ncs[i][ns] = 0.0;
                        for (int j = 1; j <= 3; j++) {
                            g.bcs[i][ns] += g.r[j][i] * state.bcc[j][ns];
                            g.ncs[i][ns] += g.r[j][i] * state.ncc[j][ns];
                        }
                    }
                }
                // Rotate thermal expansion tensor
                for (int ij = 1; ij <= 6; ij++) {
                    int i = state.ijv[ij][1];
                    int j = state.ijv[ij][2];
                    g.alfacs[ij] = 0.0;
                    for (int i1 = 1; i1 <= 3; i1++) {
                        for (int j1 = 1; j1 <= 3; j1++) {
                            g.alfacs[ij] += g.r[i1][i] * g.r[j1][j] * alfacc2_cr[i1][j1];
                        }
                    }
                }
            }
        } else { // iopt 1: Rotate stiffness (ccs2) and compliance (scs2)
            TensorUtils.voigt(T1_cr, T2_cr, state.ccc2, ccc4_cr, 3);
            TensorUtils.voigt(T1_cr, T2_cr, state.scc2, scc4_cr, 3);
            
            for (int ng_idx = ng1 - 1; ng_idx < ng2; ng_idx++) {
                Grain g = grains.get(ng_idx);
                for (int ij = 1; ij <= 6; ij++) {
                    int i = state.ijv[ij][1];
                    int j = state.ijv[ij][2];
                    for (int kl = 1; kl <= 6; kl++) {
                        int k = state.ijv[kl][1];
                        int l = state.ijv[kl][2];
                        g.ccs2[ij][kl] = 0.0;
                        g.scs2[ij][kl] = 0.0;
                        for (int i1 = 1; i1 <= 3; i1++) {
                            for (int j1 = 1; j1 <= 3; j1++) {
                                for (int k1 = 1; k1 <= 3; k1++) {
                                    for (int l1 = 1; l1 <= 3; l1++) {
                                        double rotFactor = g.r[i1][i] * g.r[j1][j] * g.r[k1][k] * g.r[l1][l];
                                        g.ccs2[ij][kl] += rotFactor * ccc4_cr[i1][j1][k1][l1];
                                        g.scs2[ij][kl] += rotFactor * scc4_cr[i1][j1][k1][l1];
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Calculates orientation matrix 'A' for a twinned crystal by 180-deg
     * rotation around the Burgers vector. Modifies A in place.
     */
    public void twinor(double[] bur, double[][] A) {
        double ang1 = Math.atan2(bur[2], bur[1]) + CommonConstants.PI_2;
        double ang2 = Math.sqrt((bur[1]*bur[1]) + (bur[2]*bur[2]));
        ang2 = Math.atan2(ang2, bur[3]);
        
        TensorUtils.eulerFromAngles(ang1 * CommonConstants.RAD_TO_DEG,
                                    ang2 * CommonConstants.RAD_TO_DEG, 0.0, aux_tw);
        
        // HTW = AUX_transpose * HPI * AUX
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                htw_tw[i][j] = 0.0;
                for (int k1 = 1; k1 <= 3; k1++) {
                    for (int k2 = 1; k2 <= 3; k2++) {
                        htw_tw[i][j] += aux_tw[k1][i] * hpi_tw[k1][k2] * aux_tw[k2][j];
                    }
                }
            }
        }
        
        // ATW = A * HTW
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                atw_tw[i][j] = 0.0;
                for (int k = 1; k <= 3; k++) {
                    atw_tw[i][j] += A[i][k] * htw_tw[k][j];
                }
            }
        }
        
        // A = ATW
        for (int i = 1; i <= 3; i++) {
            System.arraycopy(atw_tw[i], 1, A[i], 1, 3);
        }
    }

    /**
     * Generates a new "child" grain by twinning a parent grain.
     * This adds a new grain to the global arrays and calculates its orientation.
     */
    public Grain generateChildGrain(Grain parent, int ist, int imo, SimulationState state, List<Grain> grains) {
        int newGrainId = grains.size() + 1;
        if (newGrainId > state.NGR) {
            throw new RuntimeException("ERROR: Number of grains greater than code dimension NGR.");
        }
        
        Grain child = new Grain(newGrainId, state.NSLS, state.kCL);
        state.ngrain = newGrainId; // Update total grain count

        child.iParentGrain = parent.id;
        child.iParentSystem = ist;
        child.iParentMode = imo;
        child.iTwinLevel = parent.iTwinLevel + 1;
        parent.iChildGrain[ist] = -newGrainId; // Use negative as flag for "new this step"

        // Get Burgers vector and R_transpose of parent
        double[] bur = new double[4];
        double[][] aux33 = new double[4][4];
        for (int i = 1; i <= 3; i++) {
            bur[i] = state.bcc[i][ist];
            for (int j = 1; j <= 3; j++) {
                aux33[i][j] = parent.r[j][i]; // R_transpose
            }
        }

        twinor(bur, aux33); // aux33 now holds R_twin_transpose

        // Store new orientation matrix R_twin
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                child.r[i][j] = aux33[j][i];
            }
        }

        TensorUtils.EulerAngles angles = TensorUtils.matrixToEuler(child.r);
        child.phi = angles.phi;
        child.the = angles.theta;
        child.ome = angles.omega;

        if (state.kSM == 1.0) {
            double pressure = -(parent.stcs[1] + parent.stcs[2] + parent.stcs[3]) / 3.0;
            // pressureCij(pressure, state);
        }

        // Add to list *before* calling crToSa
        grains.add(child);
        
        crToSa(newGrainId, newGrainId, 0, state, grains);
        crToSa(newGrainId, newGrainId, 1, state, grains);

        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                child.acs2[i][j] = child.ccs2[i][j];
            }
        }
        
        return child;
    }

    /**
     * Manages the grain twinning process.
     */
    public void twinningBjcl(double step, List<Grain> grains, SimulationState state, HardeningModel hardeningModel) {
        
        double[][] vchange = new double[state.NMOD + 1][state.NSLS + 1];
        int[] iactivtwmod = new int[state.NMOD + 1];
        List<Grain> newGrains = new ArrayList<>();
        
        int currentMaxGrains = grains.size();
        for (int ng_idx = 0; ng_idx < currentMaxGrains; ng_idx++) {
            Grain g = grains.get(ng_idx);
            g.wgtd = 0.0;
            int iTwins = 0;
            int nst = 0;
            
            if (g.iTwinLevel < state.iMaxTwinLevel) {
                // ... (Full implementation from TWINNING_BJCL) ...
                // This logic is complex and involves:
                // 1. Looping 'mo' from 1 to nmodes
                // 2. Checking gamd(nst, ng) > 0
                // 3. If iChildGrain == 0, call:
                //    Grain child = generateChildGrain(g, nst, mo, state, grains);
                //    newGrains.add(child);
                // 4. Calculating vchange, sumvchange
                // 5. Scaling vchange if sumvchange > g.wgt
                // 6. Updating TVF/CTVF
                // 7. Applying wgtd: g.wgtd -= sumvchange
                // 8. Applying wgtd to children: grains.get(nch-1).wgtd += vchange(...)
                // 9. Hardening exhausted parent grains
            }
            if (iTwins > state.MaxTwins) state.MaxTwins = iTwins;
        } 
        
        // Add all newly created grains to the main list
        grains.addAll(newGrains);
        
        // Renormalize weights
        double totwgt = 0.0;
        for (Grain g : grains) {
            totwgt += (g.wgt + g.wgtd);
        }
        if (Math.abs(totwgt) > 1.0e-9) {
            for (Grain g : grains) {
                g.wgt = (g.wgt + g.wgtd) / totwgt;
            }
        }
    }
    
    // ... (All other physics methods: updateOrientation, corotation, etc.) ...
    
    /**
     * Calculates the stress and elastic strain state in the twin child.
     */
    public void calcChildStress(Grain child, Grain parent, SimulationState state) {
        
        int isys = child.iParentSystem;
        final int[] istTW = {0, 0, 0, 1, 1, 1, 0}; // 1-based: {0,0,1,1,1,0}
        final int[] ietTW = {0, 1, 1, 0, 0, 0, 1}; // 1-based: {1,1,0,0,0,1}

        // --- 1. Define rTW2SA (Twin-to-Sample rotation matrix) ---
        double vlen = 0.0;
        for(int i=1; i<=3; i++) vlen += (parent.bcs[i][isys] * parent.bcs[i][isys]);
        vlen = Math.sqrt(vlen);
        for(int i=1; i<=3; i++) rTW2SA_ccs[i][1] = parent.bcs[i][isys] / vlen;
        
        vlen = 0.0;
        for(int i=1; i<=3; i++) vlen += (parent.ncs[i][isys] * parent.ncs[i][isys]);
        vlen = Math.sqrt(vlen);
        for(int i=1; i<=3; i++) rTW2SA_ccs[i][3] = parent.ncs[i][isys] / vlen;
        
        rTW2SA_ccs[1][2] = rTW2SA_ccs[2][3]*rTW2SA_ccs[3][1] - rTW2SA_ccs[3][3]*rTW2SA_ccs[2][1];
        rTW2SA_ccs[2][2] = rTW2SA_ccs[3][3]*rTW2SA_ccs[1][1] - rTW2SA_ccs[1][3]*rTW2SA_ccs[3][1];
        rTW2SA_ccs[3][2] = rTW2SA_ccs[1][3]*rTW2SA_ccs[2][1] - rTW2SA_ccs[2][3]*rTW2SA_ccs[1][1];
        
        for (int i=1; i<=3; i++) for (int j=1; j<=3; j++) rSA2TW_ccs[i][j] = rTW2SA_ccs[j][i];

        // --- 2. Rotate Parent stress to twin coordinates (stParent) ---
        for (int i=1; i<=6; i++) aux6_ccs[i] = parent.stcs[i];
        TensorUtils.voigt(aux6_ccs, aux33_ccs, aux66_ccs, aux3333_ccs, 1);
        for (int ij = 1; ij <= 6; ij++) {
            int i = state.ijv[ij][1], j = state.ijv[ij][2];
            stParent_ccs[ij] = 0.0;
            for (int i1=1; i1<=3; i1++) for (int j1=1; j1<=3; j1++) {
                stParent_ccs[ij] += rSA2TW_ccs[i][i1] * rSA2TW_ccs[j][j1] * aux33_ccs[i1][j1];
            }
        }
        
        // --- 3. Rotate Parent elastic strain to twin coordinates (etelParent) ---
        if (state.kSM == 1.0) {
            for (int i = 1; i <= 6; i++) aux6_ccs[i] = parent.etelcs[i];
        } else {
            for (int i = 1; i <= 6; i++) {
                aux6_ccs[i] = 0.0;
                for (int j = 1; j <= 6; j++) {
                    aux6_ccs[i] += parent.scs2[i][j] * parent.stcs[j] * state.profac[j];
                }
            }
        }
        TensorUtils.voigt(aux6_ccs, aux33_ccs, aux66_ccs, aux3333_ccs, 1);
        for (int ij = 1; ij <= 6; ij++) {
            int i = state.ijv[ij][1], j = state.ijv[ij][2];
            etelParent_ccs[ij] = 0.0;
            for (int i1=1; i1<=3; i1++) for (int j1=1; j1<=3; j1++) {
                etelParent_ccs[ij] += rSA2TW_ccs[i][i1] * rSA2TW_ccs[j][j1] * aux33_ccs[i1][j1];
            }
        }
        
        // --- 4. Rotate Child stiffness to twin coordinates (aChild) ---
        TensorUtils.voigt(null, null, child.acs2, aux3333_ccs, 3);
        for (int ij = 1; ij <= 6; ij++) {
            int i = state.ijv[ij][1], j = state.ijv[ij][2];
            for (int kl = 1; kl <= 6; kl++) {
                int k = state.ijv[kl][1], l = state.ijv[kl][2];
                aChild_ccs[ij][kl] = 0.0;
                for (int i1=1; i1<=3; i1++) for (int j1=1; j1<=3; j1++)
                for (int k1=1; k1<=3; k1++) for (int l1=1; l1<=3; l1++) {
                    aChild_ccs[ij][kl] += rSA2TW_ccs[i][i1] * rSA2TW_ccs[j][j1] *
                                          rSA2TW_ccs[k][k1] * rSA2TW_ccs[l][l1] * aux3333_ccs[i1][j1][k1][l1];
                }
            }
        }
        
        // --- 5. Solve the linear system ---
        for (int i = 1; i <= 6; i++) {
            aux6_ccs[i] = -1.0 * istTW[i] * stParent_ccs[i];
            for (int j = 1; j <= 6; j++) {
                aux6_ccs[i] += aChild_ccs[i][j] * ietTW[j] * etelParent_ccs[j] * state.profac[j];
                aux66_ccs[i][j] = ietTW[j] * ((i == j) ? 1.0 : 0.0) - istTW[j] * aChild_ccs[i][j] * state.profac[j];
            }
        }
        TensorUtils.ludcmpc(aux66_ccs, 6, 6, indx_ccs);
        TensorUtils.lubksbc(aux66_ccs, 6, 6, indx_ccs, aux6_ccs);
        for (int i = 1; i <= 6; i++) {
            etelChild_ccs[i] = ietTW[i] * etelParent_ccs[i] + istTW[i] * aux6_ccs[i];
            stChild_ccs[i] = istTW[i] * stParent_ccs[i] + ietTW[i] * aux6_ccs[i];
        }

        // --- 6. Rotate Child stress/strain back to sample coordinates ---
        TensorUtils.voigt(stChild_ccs, aux33_ccs, aux66_ccs, aux3333_ccs, 1);
        for (int ij = 1; ij <= 6; ij++) {
            int i = state.ijv[ij][1], j = state.ijv[ij][2];
            child.stcs[ij] = 0.0;
            for (int i1=1; i1<=3; i1++) for (int j1=1; j1<=3; j1++) {
                child.stcs[ij] += rTW2SA_ccs[i][i1] * rTW2SA_ccs[j][j1] * aux33_ccs[i1][j1];
            }
        }
        
        if (state.kSM == 1.0) {
            TensorUtils.voigt(etelChild_ccs, aux33_ccs, aux66_ccs, aux3333_ccs, 1);
            for (int ij = 1; ij <= 6; ij++) {
                int i = state.ijv[ij][1], j = state.ijv[ij][2];
                child.etelcs[ij] = 0.0;
                for (int i1=1; i1<=3; i1++) for (int j1=1; j1<=3; j1++) {
                    child.etelcs[ij] += rTW2SA_ccs[i][i1] * rTW2SA_ccs[j][j1] * aux33_ccs[i1][j1];
                }
            }
            for (int i = 1; i <= 6; i++) {
                child.etelhycs[i] = parent.etelhycs[i];
            }
        }
    }

    /**
     * Initializes the symmetry operators for crystalSymmetry and poleFig.
     * This is a static method because the operators are constant.
     */
    private static void initializeSymmetry(int icrysym) {
        // ... (Full implementation of symmetry generation from crystal_symmetry iopt=1) ...
        // ... (This populates the static 'hh' and 'nsymop' fields) ...
    }
    
    /**
     * Calculates crystal vectors or equivalent poles.
     * (Static because it uses static symmetry operators).
     */
    public static void crystalSymmetry(int ioption, Scanner ur1, IntHolder icrysym, ...) {
        if (ioption == 1) {
            // ... (Read crysym, cdim, cang from ur1) ...
            // ... (Calculate 'cvec') ...
            initializeSymmetry(icrysym.value);
        }
        if (ioption == 2 || ioption == 3) {
            // ... (Calculate sn, sb from isn, isb using 'cvec') ...
        }
        if (ioption == 3) {
            // ... (Calculate equivalent poles 'sneq' using 'hh' and 'nsymop') ...
        }
    }
    
    /**
     * Calculates residual strain pole figures.
     */
    public void poleFig(int icrysym, int nfile, int iopt, String filecrys, SimulationState state, List<Grain> grains) {
        if (iopt == 0) {
            // ... (Implementation from PoleFig iopt=0, reads filecrys,
            //      calculates 'cvec', calls initializeSymmetry) ...
        }
        if (iopt == 1) {
            // ... (Implementation from PoleFig iopt=1, reads strainpf.in) ...
            // ... (Loops over grains list, calculates strain, writes to file) ...
        }
    }
}
