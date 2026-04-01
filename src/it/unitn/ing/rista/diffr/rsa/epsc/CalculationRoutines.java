package it.unitn.ing.rista.diffr.rsa.epsc;

import java.io.*;
import java.util.Locale;
import java.util.Scanner;

//import static it.unitn.ing.rista.diffr.rsa.epsc.ModCrystalSymmetry.*;
//import static it.unitn.ing.rista.diffr.rsa.epsc.ModPolefig.*;
// Import the new static module
//import static ModEffectiveMagnitudes.*;
//import static CommonPlastic.*;
//import static ModPolefig.*;
//import static ModUpdateOrientation.*;
//import static ModCorotation.*;

/**
 * Contains translated calculation subroutines like av_modulus and back_stress.
 */
public class CalculationRoutines {

/**
     * Initializes global constants from the main program's DATA block.
     * This includes the Voigt mapping, factors, and identity matrices.
     * This *must* be called once before any other calculations.
     */
    public static void initializeGlobals() {
        // DATA ((ijvx(i,j),j=1,2),i=1,6)/1,1,2,2,3,3,2,3,1,3,1,2/
        int[][] ijvx_data = {
            {0, 0}, // 0 (unused)
            {1, 1}, // 1
            {2, 2}, // 2
            {3, 3}, // 3
            {2, 3}, // 4
            {1, 3}, // 5
            {1, 2}  // 6
        };

        for (int i = 1; i <= 6; i++) {
            // Initialize i6 (vector of 0s)
            CommonGlobals.i6[i] = 0;
            
            // Set ijv (Voigt mapping)
            CommonGlobals.ijv[i][1] = ijvx_data[i][0];
            CommonGlobals.ijv[i][2] = ijvx_data[i][1];

            // profac(i)=1.d0+(i/4)
            // (Note: integer division (i/4) is 0 for i=1,2,3 and 1 for i=4,5,6)
            CommonGlobals.profac[i] = (i <= 3) ? 1.0 : 2.0;

            for (int j = 1; j <= 6; j++) {
                // id2(i,j)=(i/j)*(j/i)*(1.d0-0.5d0*(i/4))
                // (i/j)*(j/i) is 1 if i==j, 0 otherwise
                // (1.d0-0.5d0*(i/4)) is 1.0 for i=1,2,3 and 0.5 for i=4,5,6
                double diag_val = (i <= 3) ? 1.0 : 0.5;
                CommonGlobals.id2[i][j] = (i == j) ? diag_val : 0.0;
                
                // invfac(i,j)=1.d0/((1.d0+i/4)*(1.d0+j/4))
                double i_fact = (i <= 3) ? 1.0 : 2.0; // 1.0 + i/4
                double j_fact = (j <= 3) ? 1.0 : 2.0; // 1.0 + j/4
                CommonGlobals.invfac[i][j] = 1.0 / (i_fact * j_fact);
            }
        }
    }
    
    /**
     * Translation of SUBROUTINE av_modulus.
     * Calculates Voigt, Reuss, and Hill averages.
     * Assumes IOUtils.writer11 is initialized.
     *
     * <p><b>Note:</b> This method assumes all global arrays in CommonGlobals
     * (e.g., wgt, ccs2) were allocated with size [N+1] to allow
     * for 1-based indexing.
     */
    public static void avModulus() {
        // Local arrays (using 1-based indexing)
        double[][] css2vo = new double[7][7];
        double[][] css2re = new double[7][7];
        double[][] sss2vo = new double[7][7];
        double[][] sss2re = new double[7][7];

        // Note: gambsmax, gamdbs from Fortran are unused in this subr and omitted.

        // Initialize css2vo and sss2re (Java does this by default, but good form)
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                css2vo[i][j] = 0.0;
                sss2re[i][j] = 0.0;
            }
        }

        // Weighted sum loop
        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            for (int i = 1; i <= 6; i++) {
                for (int j = 1; j <= 6; j++) {
                    css2vo[i][j] += CommonGlobals.wgt[ng] * CommonGlobals.ccs2[i][j][ng];
                    sss2re[i][j] += CommonGlobals.wgt[ng] * CommonGlobals.scs2[i][j][ng];
                }
            }
        }

        // Call matrix inversion
        TensorUtils.invten(css2vo, sss2vo);

        // Get the writer for unit 11
        PrintWriter out = IOUtils.writer11;
        if (out == null) {
            System.err.println("Error: IOUtils.writer11 is not initialized!");
            return;
        }

        out.println("ELASTIC PROPERTIES AVERAGE:");
        IOUtils.printMatrix(out, " VOIGT average stiffness matrix", css2vo);
        IOUtils.printMatrix(out, " VOIGT average compliance matrix", sss2vo);

        // Call matrix inversion
        TensorUtils.invten(sss2re, css2re);

        IOUtils.printMatrix(out, " REUSS average stiffness matrix", css2re);
        IOUtils.printMatrix(out, " REUSS average compliance matrix", sss2re);

        // Calculate Hill average and store in CommonGlobals
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                CommonGlobals.css2[i][j] = (css2vo[i][j] + css2re[i][j]) / 2.0;
                CommonGlobals.ass2[i][j] = CommonGlobals.css2[i][j];
            }
        }

        out.println();
        out.println();
        IOUtils.printMatrix(out, " HILL average stiffness matrix", CommonGlobals.css2);

        // Call matrix inversion
        TensorUtils.invten(CommonGlobals.css2, CommonGlobals.sss2);

        IOUtils.printMatrix(out, " HILL average compliance matrix", CommonGlobals.sss2);

        out.println();
        out.println("******************************************************************************");
    }


    /**
     * Translation of SUBROUTINE BACK_STRESS.
     * @param ioption 0 for init, 1 for calculation.
     */
    public static void backStress(int ioption) {

        if (ioption == 0) {
            // Allocate arrays.
            // IMPORTANT: Allocate [NGR+1] to support 1-based indexing.
            ModBackStress.gambs = new double[CommonGlobals.NGR + 1]; // Java auto-inits to 0.0
            ModBackStress.gmod = new double[CommonGlobals.NGR + 1];  // Java auto-inits to 0.0
            ModBackStress.iprsys = new int[CommonGlobals.NGR + 1]; // Java auto-inits to 0

            // IOUtils.writer77 should be initialized externally
            if (IOUtils.writer77 == null) {
                System.err.println("Error: IOUtils.writer77 is not initialized. Call IOUtils.initializeWriters() first.");
 //               return;
            }

            //L !L write(*,*) 'enter value of %GAM in BACK_STRESS SUBR'
            //L System.out.println("enter value of %GAM in BACK_STRESS SUBR");
            //L System.out.println();
            //L Scanner consoleScanner = new Scanner(System.in);
            //L ModBackStress.percent_gam = consoleScanner.nextDouble();

            //L !L write(*,*) 'enter value of %TAU in BACK_STRESS SUBR'
            //L System.out.println("enter value of %TAU in BACK_STRESS SUBR");
            //L System.out.println();
            //L ModBackStress.percent_tau = consoleScanner.nextDouble();

        } else if (ioption == 1) {

            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {

                // *** if the grain starts going plastic then tags the primary system and
                // *** calculates associated shear modulus
                if (ModBackStress.iprsys[ng] == 0 && CommonGlobals.nact[ng] > 0) {
                    ModBackStress.gamdmax = 0.0;
                    for (int is = 1; is <= CommonGlobals.nact[ng]; is++) {
                        int iact_is_ng = CommonGlobals.iact[is][ng];
                        if (CommonGlobals.gamd[iact_is_ng][ng] > ModBackStress.gamdmax) {
                            ModBackStress.gamdmax = CommonGlobals.gamd[iact_is_ng][ng];
                            ModBackStress.iprsys[ng] = iact_is_ng;
                        }
                    }

                    int isx = ModBackStress.iprsys[ng];
                    ModBackStress.gmod[ng] = 0.0;
                    for (int i = 1; i <= 6; i++) {
                        for (int j = 1; j <= 6; j++) {
                            ModBackStress.gmod[ng] += CommonGlobals.mcs[i][isx][ng] * CommonGlobals.mcs[j][isx][ng]
                                                    * CommonGlobals.ccs2[i][j][ng] * CommonGlobals.profac[i] * CommonGlobals.profac[j];
                        }
                    }

                    if (ng == 1) {
                        IOUtils.writer77.println(" grain iprsy   nact     gmod");
                        IOUtils.writer77.printf("%6d%6d%6d%8.2f%n", ng, ModBackStress.iprsys[ng], CommonGlobals.nact[ng], ModBackStress.gmod[ng]);
                        System.out.println(" grain iprsy   nact     gmod");
                        System.out.printf("%6d%6d%6d%8.2f%n", ng, ModBackStress.iprsys[ng], CommonGlobals.nact[ng], ModBackStress.gmod[ng]);
                    }
                }

                // For a tagged grain which is deforming plastically...
                if (ModBackStress.iprsys[ng] > 0 && CommonGlobals.nact[ng] > 0) {
                    int isx = ModBackStress.iprsys[ng];
                    if (Math.abs(ModBackStress.gmod[ng]) > 1e-9) {
                        ModBackStress.gambsmax = ModBackStress.percent_tau * CommonGlobals.tau[isx][ng] / ModBackStress.gmod[ng];
                        
                        ModBackStress.gamdbs = ModBackStress.percent_gam * CommonGlobals.gamd[isx][ng];
                        ModBackStress.gambs[ng] += ModBackStress.gamdbs;

                        if (ModBackStress.gambs[ng] > ModBackStress.gambsmax) {
                            ModBackStress.gambs[ng] = ModBackStress.gambsmax;
                            ModBackStress.gamdbs = 0.0;
                        }

                        ModBackStress.rss = 0.0;
                        for (int i = 1; i <= 6; i++) {
                            ModBackStress.rss += CommonGlobals.mcs[i][isx][ng] * CommonGlobals.stcs[i][ng] * CommonGlobals.profac[i];
                        }

                        if (ng == 1) {
                            System.out.println(" grain iprsy   nact      rss    gamdbs     gambs");
                            System.out.printf("%6d%6d%6d%9.6f%9.6f%9.6f%n", ng, ModBackStress.iprsys[ng], CommonGlobals.nact[ng],
                                                ModBackStress.rss, ModBackStress.gamdbs, ModBackStress.gambs[ng]);
                        }
                    }
                }

                // *** During unloading of the primary system...
                if (ModBackStress.iprsys[ng] != 0 && CommonGlobals.nact[ng] == 0) {
                    int isx = ModBackStress.iprsys[ng];
                    ModBackStress.rss = 0.0;
                    for (int i = 1; i <= 6; i++) {
                        ModBackStress.rss += CommonGlobals.mcs[i][isx][ng] * CommonGlobals.stcs[i][ng] * CommonGlobals.profac[i];
                    }
                    ModBackStress.rssd = ModBackStress.rss - 0.99 * CommonGlobals.tau[isx][ng];
                    
                    // Initialize gamdbs for this block
                    double gamdbs = 0.0; 

                    if (ModBackStress.rssd < 0.0 && Math.abs(ModBackStress.gmod[ng]) > 1.0e-9) {
                        gamdbs = ModBackStress.rssd / ModBackStress.gmod[ng]; // Use local gamdbs
                        if ((ModBackStress.gambs[ng] + gamdbs) < 0.0) {
                            gamdbs = -ModBackStress.gambs[ng];
                            ModBackStress.iprsys[ng] = 0;
                        }
                        ModBackStress.gambs[ng] += gamdbs;
                    }

                    if (ng == 1) {
                        System.out.println(" grain iprsy   nact     rss     tau    gamdbs     gambs");
                        System.out.printf("%6d%6d%6d%7.3f%7.3f%9.6f%9.6f%n", ng, ModBackStress.iprsys[ng], CommonGlobals.nact[ng],
                                            ModBackStress.rss, CommonGlobals.tau[isx][ng], gamdbs, ModBackStress.gambs[ng]);
                    }
                }
            } // end of do over grains
        } // end of 'if ioption==1'
    } // end of backStress
    
/**
     * Rotates material properties from Crystal (CR) to Sample (SA) frame.
     * Translation of Fortran SUBROUTINE cr_to_sa.
     *
     * @param ng1   The starting grain index.
     * @param ng2   The ending grain index.
     * @param iopt  Option flag:
     * 0 = Rotates Schmid tensors (mcs, qcs), Burgers vectors (bcs, ncs),
     * and thermal coefficients (alfacs).
     * 1 = Rotates stiffness (ccs2) and compliance (scs2).
     */
    public static void crToSa(int ng1, int ng2, int iopt) {

        // iopt 0: Rotate Schmid tensors, thermal tensor, Burgers vector
        if (iopt == 0) {
            
            // --- Rotation of 2nd-order Schmid tensors (mcs, qcs) ---
            // T_sample = R * T_crystal * R_transpose
            for (int ng = ng1; ng <= ng2; ng++) {
                for (int ns = 1; ns <= CommonGlobals.nsys; ns++) {
                    for (int ij = 1; ij <= 6; ij++) {
                        CommonGlobals.mcs[ij][ns][ng] = 0.0;
                        CommonGlobals.qcs[ij][ns][ng] = 0.0;
                        int i = CommonGlobals.ijv[ij][1];
                        int j = CommonGlobals.ijv[ij][2];
                        for (int i1 = 1; i1 <= 3; i1++) {
                            for (int j1 = 1; j1 <= 3; j1++) {
                                // Note: Fortran R(j,i) is R_ji, i.e., R_transpose(i,j)
                                // This is T_s(i,j) = R(i,i1) * R(j,j1) * T_c(i1,j1)
                                // The code r(i1,i,ng) is R_i1_i (R_transpose(i,i1)).
                                // The formula is: T_s = R_transpose * T_c * R
                                // Let's re-check the Fortran: r(i1,i,ng) * r(j1,j,ng) * mc2(i1,j1,ns)
                                // This is R_i1,i * R_j1,j * T_i1,j1. This is T_ij = R_i1,i * R_j1,j * T_i1,j1
                                // This is the standard rotation T_s = R * T_c * R^T (using r(i1,i) = R_i1,i)
                                // Ah, the Fortran r(i,j) convention is often R_ij (row i, col j).
                                // Let's assume r(i1,i,ng) is R_i1,i (component i1 of axis i)
                                // V_sample = R * V_crystal
                                // R(i,i1) * R(j,j1) * T(i1,j1) -- this is T_ij = R_i_i1 * R_j_j1 * T_i1j1
                                // The code r(i1,i,ng) is R_i1,i
                                CommonGlobals.mcs[ij][ns][ng] += CommonGlobals.r[i1][i][ng] * CommonGlobals.r[j1][j][ng]
                                                              * CommonGlobals.mc2[i1][j1][ns];
                                CommonGlobals.qcs[ij][ns][ng] += CommonGlobals.r[i1][i][ng] * CommonGlobals.r[j1][j][ng]
                                                              * CommonGlobals.qc2[i1][j1][ns];
                            }
                        }
                    }
                }
            }

            // --- Rotation of Burgers vector (bcs) and slip normal (ncs) ---
            // V_sample = R * V_crystal
            for (int ng = ng1; ng <= ng2; ng++) {
                for (int ns = 1; ns <= CommonGlobals.nsys; ns++) {
                    for (int i = 1; i <= 3; i++) {
                        CommonGlobals.bcs[i][ns][ng] = 0.0;
                        CommonGlobals.ncs[i][ns][ng] = 0.0;
                        for (int j = 1; j <= 3; j++) {
                            // Fortran: bcs(i) = r(j,i) * bcc(j)
                            // This is V_s(i) = sum_j( R_ji * V_c(j) )
                            // which is V_s = R_transpose * V_c
                            CommonGlobals.bcs[i][ns][ng] += CommonGlobals.r[j][i][ng] * CommonGlobals.bcc[j][ns];
                            CommonGlobals.ncs[i][ns][ng] += CommonGlobals.r[j][i][ng] * CommonGlobals.ncc[j][ns];
                        }
                    }
                }
            }

            // --- Rotate thermal expansion tensor ---
            // 1. Convert 6x1 vector (alfacc) to 3x3 tensor (alfacc2)
            TensorUtils.voigt(CommonGlobals.alfacc, ModCrToSa.alfacc2, ModCrToSa.C2, ModCrToSa.C4, 1);
            
            // 2. Rotate the 3x3 tensor
            for (int ng = ng1; ng <= ng2; ng++) {
                for (int ij = 1; ij <= 6; ij++) {
                    int i = CommonGlobals.ijv[ij][1];
                    int j = CommonGlobals.ijv[ij][2];
                    CommonGlobals.alfacs[ij][ng] = 0.0;
                    for (int i1 = 1; i1 <= 3; i1++) {
                        for (int j1 = 1; j1 <= 3; j1++) {
                            CommonGlobals.alfacs[ij][ng] += CommonGlobals.r[i1][i][ng] * CommonGlobals.r[j1][j][ng]
                                                         * ModCrToSa.alfacc2[i1][j1];
                        }
                    }
                }
            }

        } else {
            // iopt 1: Rotate 4th-order stiffness (ccs2) and compliance (scs2)

            // 1. Convert 6x6 Voigt matrices to 3x3x3x3 full tensors
            TensorUtils.voigt(ModCrToSa.T1, ModCrToSa.T2, CommonGlobals.ccc2, ModCrToSa.ccc4, 3);
            TensorUtils.voigt(ModCrToSa.T1, ModCrToSa.T2, CommonGlobals.scc2, ModCrToSa.scc4, 3);

            // 2. Rotate the 3x3x3x3 tensors
            // T_sample = R * R * T_crystal * R_transpose * R_transpose
            for (int ng = ng1; ng <= ng2; ng++) {
                for (int ij = 1; ij <= 6; ij++) {
                    int i = CommonGlobals.ijv[ij][1];
                    int j = CommonGlobals.ijv[ij][2];
                    for (int kl = 1; kl <= 6; kl++) {
                        int k = CommonGlobals.ijv[kl][1];
                        int l = CommonGlobals.ijv[kl][2];
                        CommonGlobals.ccs2[ij][kl][ng] = 0.0;
                        CommonGlobals.scs2[ij][kl][ng] = 0.0;
                        for (int i1 = 1; i1 <= 3; i1++) {
                            for (int j1 = 1; j1 <= 3; j1++) {
                                for (int k1 = 1; k1 <= 3; k1++) {
                                    for (int l1 = 1; l1 <= 3; l1++) {
                                        
                                        double rotationFactor = CommonGlobals.r[i1][i][ng] * CommonGlobals.r[j1][j][ng]
                                                              * CommonGlobals.r[k1][k][ng] * CommonGlobals.r[l1][l][ng];
                                        
                                        CommonGlobals.ccs2[ij][kl][ng] += rotationFactor * ModCrToSa.ccc4[i1][j1][k1][l1];
                                        CommonGlobals.scs2[ij][kl][ng] += rotationFactor * ModCrToSa.scc4[i1][j1][k1][l1];
                                    }
                                }
                            }
                        }
                    } // kl
                } // ij
            } // ng
        } // end iopt
    }

/**
     * Generates crystal symmetry operators and calculates vectors.
     * Translation of Fortran SUBROUTINE crystal_symmetry.
     *
     * @param ioption Control flag:
     * 1: Read symmetry, generate operators.
     * 2: Calculate normal (sn) and Burgers (sb) vectors.
     * 3: Generate equivalent vectors (sneq) and find unique ones (nequiv).
     * @param ur1     The Scanner object for reading input (e.g., IOUtils.scanner1).
     * @param icrysym (In/Out) Wrapped integer for the crystal symmetry ID.
     * @param isn     (In) 4-element Miller-Bravais indices for the normal.
     * @param sn      (In/Out) 3-element vector for the normal.
     * @param sneq    (Out) Array to be filled with equivalent vectors [i][op].
     * @param isb     (In) 4-element Miller-Bravais indices for Burgers vector.
     * @param sb      (Out) 3-element vector for the Burgers vector.
     * @param nequiv  (Out) Wrapped integer for the count of equivalent vectors.
     */
    public static void crystalSymmetry(int ioption, Scanner ur1, IntHolder icrysym,
                                       int[] isn, double[] sn, double[][] sneq,
                                       int[] isb, double[] sb, IntHolder nequiv) {

        // ioption 1: Read symmetry, generate operators
        if (ioption == 1) {
            
            icrysym.value = 0;

            ur1.nextLine(); // Skip comment line
            String crysym = IOUtils.stringArrayFromLine(ur1.nextLine()).elementAt(0); // Read '(a)'
            System.out.println("Symmetry: " + crysym);

            if (crysym.equalsIgnoreCase("cubic")) icrysym.value = 1;
            if (crysym.equalsIgnoreCase("hexag")) icrysym.value = 2;
            if (crysym.equalsIgnoreCase("trigo")) icrysym.value = 3;
            if (crysym.equalsIgnoreCase("tetra")) icrysym.value = 4;
            if (crysym.equalsIgnoreCase("ortho")) icrysym.value = 5;
            if (crysym.equalsIgnoreCase("monoc")) icrysym.value = 6;
            if (crysym.equalsIgnoreCase("tricl")) icrysym.value = 7;

            if (icrysym.value == 0) {
                throw new RuntimeException(" *** CANNOT RECOGNIZE THE CRYSTAL SYMMETRY");
            }

            // Read cell dimensions and angles
            String[] line = ur1.nextLine().trim().split("\\s+");
            for (int i = 1; i <= 3; i++) {
                ModCrystalSymmetry.cdim[i] = Double.parseDouble(line[i - 1]);
              ModCrystalSymmetry.cang[i] = Double.parseDouble(line[i + 2]) * CommonConstants.DEG_TO_RAD;
            }

            // --- Calculate unit cell vectors ---
            // assumes 'c' coincident with 'z' and 'a' in the plane 'xz'
            ModCrystalSymmetry.cvec[1][1] = Math.sin(ModCrystalSymmetry.cang[2]);
            ModCrystalSymmetry.cvec[2][1] = 0.0;
            ModCrystalSymmetry.cvec[3][1] = Math.cos(ModCrystalSymmetry.cang[2]);

            ModCrystalSymmetry.cvec[3][2] = Math.cos(ModCrystalSymmetry.cang[1]);
            ModCrystalSymmetry.cvec[1][2] = (Math.cos(ModCrystalSymmetry.cang[3]) - ModCrystalSymmetry.cvec[3][2] * ModCrystalSymmetry.cvec[3][1])
                                          / ModCrystalSymmetry.cvec[1][1];
            ModCrystalSymmetry.cvec[2][2] = Math.sqrt(1.0 - ModCrystalSymmetry.cvec[1][2] * ModCrystalSymmetry.cvec[1][2]
                                                   - ModCrystalSymmetry.cvec[3][2] * ModCrystalSymmetry.cvec[3][2]);

            ModCrystalSymmetry.cvec[1][3] = 0.0;
            ModCrystalSymmetry.cvec[2][3] = 0.0;
            ModCrystalSymmetry.cvec[3][3] = 1.0;

            // Scale vectors by cell dimensions
            for (int j = 1; j <= 3; j++) {
                for (int i = 1; i <= 3; i++) {
                    ModCrystalSymmetry.cvec[i][j] = ModCrystalSymmetry.cdim[j] * ModCrystalSymmetry.cvec[i][j];
                }
            }

            // Initialize generator and operator matrices to zero
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                    for (int m = 1; m <= 6; m++) {
                        ModCrystalSymmetry.hx[i][j][m] = 0.0;
                    }
                    for (int n = 1; n <= 24; n++) {
                        ModCrystalSymmetry.hh[i][j][n] = 0.0;
                    }
                }
            }

            // --- Start generating symmetry operators ---
            // Identity (triclinic & all)
            for (int i = 1; i <= 3; i++) {
                ModCrystalSymmetry.hh[i][i][1] = 1.0;
            }
            ModCrystalSymmetry.nsymop = 1;
            int mn = 1; // Holder for the last index

            // 180 deg rot around (001) (ortho, mono)
            if (icrysym.value == 5 || icrysym.value == 6) {
                ModCrystalSymmetry.hh[1][1][2] = -1.0;
                ModCrystalSymmetry.hh[2][2][2] = -1.0;
                ModCrystalSymmetry.hh[3][3][2] = 1.0;
                ModCrystalSymmetry.hh[1][2][2] = 0.0; // -Math.sin(PI)
                ModCrystalSymmetry.hh[2][1][2] = 0.0; // Math.sin(PI)
                ModCrystalSymmetry.nsymop = 2;
            }

            // x-mirror & y-mirror (ortho)
            if (icrysym.value == 5) {
                ModCrystalSymmetry.hh[1][1][3] = -1.0;
                ModCrystalSymmetry.hh[2][2][3] = 1.0;
                ModCrystalSymmetry.hh[3][3][3] = 1.0;

                ModCrystalSymmetry.hh[1][1][4] = 1.0;
                ModCrystalSymmetry.hh[2][2][4] = -1.0;
                ModCrystalSymmetry.hh[3][3][4] = 1.0;
                ModCrystalSymmetry.nsymop = 4;
            }

            // --- Cubic symmetry ---
            if (icrysym.value == 1) {
                int old_nsymop;

                // rotations of (pi/3) & (2*pi/3) around <111>
                ModCrystalSymmetry.hx[1][3][1] = 1.0;
                ModCrystalSymmetry.hx[2][1][1] = 1.0;
                ModCrystalSymmetry.hx[3][2][1] = 1.0;

                ModCrystalSymmetry.hx[1][2][2] = 1.0;
                ModCrystalSymmetry.hx[2][3][2] = 1.0;
                ModCrystalSymmetry.hx[3][1][2] = 1.0;

                old_nsymop = ModCrystalSymmetry.nsymop; // nsymop=1
                for (int m = 1; m <= 2; m++) {
                    for (int n = 1; n <= old_nsymop; n++) {
                        mn = m * old_nsymop + n; // n=1: mn=2, mn=3
                        for (int i = 1; i <= 3; i++) {
                            for (int j = 1; j <= 3; j++) {
                                for (int k = 1; k <= 3; k++) {
                                    ModCrystalSymmetry.hh[i][j][mn] += ModCrystalSymmetry.hx[i][k][m] * ModCrystalSymmetry.hh[k][j][n];
                                }
                            }
                        }
                    }
                }
                ModCrystalSymmetry.nsymop = mn; // nsymop=3

                // mirror across the plane (110)
                ModCrystalSymmetry.hx[1][2][3] = 1.0;
                ModCrystalSymmetry.hx[2][1][3] = 1.0;
                ModCrystalSymmetry.hx[3][3][3] = 1.0;

                old_nsymop = ModCrystalSymmetry.nsymop; // nsymop=3
                for (int n = 1; n <= old_nsymop; n++) {
                    mn = old_nsymop + n; // mn = 4, 5, 6
                    for (int i = 1; i <= 3; i++) {
                        for (int j = 1; j <= 3; j++) {
                            for (int k = 1; k <= 3; k++) {
                                ModCrystalSymmetry.hh[i][j][mn] += ModCrystalSymmetry.hx[i][k][3] * ModCrystalSymmetry.hh[k][j][n];
                            }
                        }
                    }
                }
                ModCrystalSymmetry.nsymop = mn; // nsymop=6

                // rotations of 90, 180, 270 around x3
                for (int m = 1; m <= 3; m++) {
                    double ang = CommonConstants.PI_2 * m;
                    ModCrystalSymmetry.hx[1][1][m] = Math.cos(ang);
                    ModCrystalSymmetry.hx[2][2][m] = Math.cos(ang);
                    ModCrystalSymmetry.hx[3][3][m] = 1.0;
                    ModCrystalSymmetry.hx[1][2][m] = -Math.sin(ang);
                    ModCrystalSymmetry.hx[2][1][m] = Math.sin(ang);
                    // hx[1][3][m]...hx[3][2][m] are already 0
                }

                old_nsymop = ModCrystalSymmetry.nsymop; // nsymop=6
                for (int m = 1; m <= 3; m++) {
                    for (int n = 1; n <= old_nsymop; n++) {
                        mn = m * old_nsymop + n; // m=1: 7-12, m=2: 13-18, m=3: 19-24
                        for (int i = 1; i <= 3; i++) {
                            for (int j = 1; j <= 3; j++) {
                                for (int k = 1; k <= 3; k++) {
                                    ModCrystalSymmetry.hh[i][j][mn] += ModCrystalSymmetry.hx[i][k][m] * ModCrystalSymmetry.hh[k][j][n];
                                }
                            }
                        }
                    }
                }
                ModCrystalSymmetry.nsymop = mn; // nsymop=24
            } // end cubic

            // --- Hexagonal, Trigonal, Tetragonal ---
            if (icrysym.value >= 2 && icrysym.value <= 4) {
                int nrot = 0;
                if (icrysym.value == 2) nrot = 6;
                if (icrysym.value == 3) nrot = 3;
                if (icrysym.value == 4) nrot = 4;

                // mirror plane
                double ang = CommonConstants.PI / nrot;
                ModCrystalSymmetry.hh[1][1][2] = Math.cos(ang) * Math.cos(ang) - Math.sin(ang) * Math.sin(ang); // cos(2*ang)
                ModCrystalSymmetry.hh[2][2][2] = -ModCrystalSymmetry.hh[1][1][2];
                ModCrystalSymmetry.hh[3][3][2] = 1.0;
                ModCrystalSymmetry.hh[1][2][2] = 2.0 * Math.cos(ang) * Math.sin(ang); // sin(2*ang)
                ModCrystalSymmetry.hh[2][1][2] = ModCrystalSymmetry.hh[1][2][2];
                ModCrystalSymmetry.nsymop = 2;

                // rotations
                for (int nr = 1; nr <= nrot - 1; nr++) {
                    ang = nr * CommonConstants.PI2 / nrot;
                    ModCrystalSymmetry.hx[1][1][nr] = Math.cos(ang);
                    ModCrystalSymmetry.hx[2][2][nr] = Math.cos(ang);
                    ModCrystalSymmetry.hx[3][3][nr] = 1.0;
                    ModCrystalSymmetry.hx[1][2][nr] = -Math.sin(ang);
                    ModCrystalSymmetry.hx[2][1][nr] = Math.sin(ang);
                }

                int old_nsymop = ModCrystalSymmetry.nsymop; // nsymop=2
                for (int m = 1; m <= nrot - 1; m++) {
                    for (int n = 1; n <= old_nsymop; n++) {
                        mn = m * old_nsymop + n;
                        for (int i = 1; i <= 3; i++) {
                            for (int j = 1; j <= 3; j++) {
                                for (int k = 1; k <= 3; k++) {
                                    ModCrystalSymmetry.hh[i][j][mn] += ModCrystalSymmetry.hx[i][k][m] * ModCrystalSymmetry.hh[k][j][n];
                                }
                            }
                        }
                    }
                }
                ModCrystalSymmetry.nsymop = mn; // e.g., hex: 5*2+2=12
            } // end hex/tri/tetra

        } // end ioption == 1

        // ioption 2 or 3: Convert Miller-Bravais indices
        if (ioption == 2 || ioption == 3) {
            
            // 4-index to 3-index conversion
            if (icrysym.value == 2 || icrysym.value == 3) {
                isn[3] = isn[4];
                isb[1] = isb[1] - isb[3];
                isb[2] = isb[2] - isb[3];
                isb[3] = isb[4];
            }

            // --- Calculate plane normal vector 'sn' ---
            // assumes 'c' coincident with 'z' and 'a' in the plane 'xz'
            sn[3] = isn[3] / ModCrystalSymmetry.cvec[3][3];
            sn[1] = (isn[1] - ModCrystalSymmetry.cvec[3][1] * sn[3]) / ModCrystalSymmetry.cvec[1][1];
            sn[2] = (isn[2] - ModCrystalSymmetry.cvec[1][2] * sn[1] - ModCrystalSymmetry.cvec[3][2] * sn[3])
                  / ModCrystalSymmetry.cvec[2][2];

            // Normalize sn
            ModCrystalSymmetry.snnor = Math.sqrt(sn[1] * sn[1] + sn[2] * sn[2] + sn[3] * sn[3]);
            if (Math.abs(ModCrystalSymmetry.snnor) > 1.0e-9) {
                for (int j = 1; j <= 3; j++) {
                    sn[j] /= ModCrystalSymmetry.snnor;
                    if (Math.abs(sn[j]) < 1.0e-6) sn[j] = 0.0;
                }
            }

            // --- Calculate Burgers vector 'sb' ---
            if (ioption == 2) {
                for (int i = 1; i <= 3; i++) {
                    sb[i] = isb[1] * ModCrystalSymmetry.cvec[i][1]
                          + isb[2] * ModCrystalSymmetry.cvec[i][2]
                          + isb[3] * ModCrystalSymmetry.cvec[i][3];
                }
                // Normalize sb
                ModCrystalSymmetry.sbnor = Math.sqrt(sb[1] * sb[1] + sb[2] * sb[2] + sb[3] * sb[3]);
                if (Math.abs(ModCrystalSymmetry.sbnor) > 1.0e-9) {
                    for (int j = 1; j <= 3; j++) {
                        sb[j] /= ModCrystalSymmetry.sbnor;
                        if (Math.abs(sb[j]) < 1.0e-3) sb[j] = 0.0;
                    }
                }
            } // end ioption == 2
            
        } // end ioption == 2 or 3

        // ioption 3: Generate all symmetry related vectors sneq(i,n)
        if (ioption == 3) {
            
            // Generate all vectors
            for (int n = 1; n <= ModCrystalSymmetry.nsymop; n++) {
                ModCrystalSymmetry.itag[n] = 0; // Reset tag
                for (int i = 1; i <= 3; i++) {
                    sneq[i][n] = 0.0; // sneq is [coord][op_num]
                    for (int j = 1; j <= 3; j++) {
                        sneq[i][n] += ModCrystalSymmetry.hh[i][j][n] * sn[j];
                    }
                }
            }

            // Eliminate redundant poles (coincident and opposite)
            if (icrysym.value != 7) { // nsymop=1 for triclinic
                for (int m = 1; m <= ModCrystalSymmetry.nsymop - 1; m++) {
                    if (ModCrystalSymmetry.itag[m] == 0) { // If m is not already tagged as redundant
                        for (int n = m + 1; n <= ModCrystalSymmetry.nsymop; n++) {
                            // Check for coincident
                            double sndif = Math.abs(sneq[1][m] - sneq[1][n])
                                         + Math.abs(sneq[2][m] - sneq[2][n])
                                         + Math.abs(sneq[3][m] - sneq[3][n]);
                            if (sndif <= 0.0001) {
                                ModCrystalSymmetry.itag[n] = 1;
                            } else {
                                // Check for opposite
                                sndif = Math.abs(sneq[1][m] + sneq[1][n])
                                      + Math.abs(sneq[2][m] + sneq[2][n])
                                      + Math.abs(sneq[3][m] + sneq[3][n]);
                                if (sndif <= 0.0001) ModCrystalSymmetry.itag[n] = 1;
                            }
                        }
                    }
                }
            }

            // Create compact list of unique vectors, all with z > 0
            nequiv.value = 0;
            for (int n = 1; n <= ModCrystalSymmetry.nsymop; n++) {
                if (ModCrystalSymmetry.itag[n] == 0) {
                    nequiv.value++;
                    int isign = (sneq[3][n] < 0.0) ? -1 : 1;
                    
                    // Overwrite sneq array in-place with the compact list
                    sneq[1][nequiv.value] = isign * sneq[1][n];
                    sneq[2][nequiv.value] = isign * sneq[2][n];
                    sneq[3][nequiv.value] = isign * sneq[3][n];
                }
            }
            
        } // end ioption == 3
    }
    
/**
     * Reads CRYSTAL data (elastic, thermal, hardening) from "filecrys".
     * Calculates Schmid tensors and initializes hardening parameters.
     * Translation of Fortran SUBROUTINE data_crystal.
     *
     * @param filecrys The path to the crystal data file.
     * @param icrysym  A wrapper (IntHolder) to store the read crystal symmetry ID.
     */
    public static void dataCrystal(String filecrys, IntHolder icrysym) {
        
        String prosa;
        String namesys;
        
        // --- Open the file with the CRYSTAL data "filecrys" ---
        File inputFile = new File(filecrys);
        try {
            // This loop dumps the file to output_unit_11.txt
            if (IOUtils.writer11 != null) {
                IOUtils.writer11.println();
                IOUtils.writer11.println(" ****** CRYSTAL DATA FILE *******");
                Scanner dumpScanner = new Scanner(inputFile);
                dumpScanner.useLocale(Locale.US);
                while (dumpScanner.hasNextLine()) {
                    prosa = dumpScanner.nextLine();
                    IOUtils.writer11.println(prosa);
                }
                dumpScanner.close();
                IOUtils.writer11.println(" ****** END OF CRYSTAL DATA FILE ******");
                IOUtils.writer11.println();
            }

            // --- REWIND 1 ---
            // Create the scanner and ASSIGN IT to the static IOUtils.scanner1
            IOUtils.scanner1 = new Scanner(inputFile);
            IOUtils.scanner1.useLocale(Locale.US);
            
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Crystal file not found: " + filecrys, e);
        }

        Scanner s = IOUtils.scanner1; // Use the static scanner

        if (IOUtils.writer11 != null) {
            IOUtils.writer11.println("******* CRYSTAL data - File: " + filecrys + "********");
            IOUtils.writer11.println("******************************************************************************");
        }

        // --- Reads crystal symmetry and unit cell parameters. ---
        // Wrappers for output parameters from crystalSymmetry
        IntHolder npolesHolder = new IntHolder(0);
        
        // Note: We pass 'null' for scanner (ur1) in subsequent calls, 
        // as only iopt=1 actually reads from the file.
        CalculationRoutines.crystalSymmetry(1, s, icrysym,
            ModDataCrystal.isn, ModDataCrystal.sn, ModDataCrystal.sneq,
            ModDataCrystal.isb, ModDataCrystal.sb, npolesHolder);
        
        ModDataCrystal.npoles = npolesHolder.value;

        ModDataCrystal.nind = 3;
        if (icrysym.value == 2 || icrysym.value == 3) {
            ModDataCrystal.nind = 4;
        }

        // --- READS SINGLE CRYSTAL ELASTIC STIFFNESS ---
        prosa = s.nextLine(); // Skip comment
        for (int i = 1; i <= 6; i++) {
            String[] line = s.nextLine().trim().split("\\s+");
            for (int j = 1; j <= 6; j++) {
                CommonGlobals.ccc2[i][j] = Double.parseDouble(line[j - 1]);
            }
        }

        // Calculates elastic compliance matrix
        TensorUtils.invten(CommonGlobals.ccc2, CommonGlobals.scc2);

        // --- Merkel: 05/2010 --> Large elastic strain and pressure dependence ---
        prosa = s.nextLine(); // Skip comment
        CommonGlobals.kSM = Double.parseDouble(IOUtils.stringArrayFromLine(s.nextLine()).elementAt(0));
        if (CommonGlobals.kSM == 1.0) {
            // READS FIRST PRESSURE DERIVATIVE
            prosa = s.nextLine(); // Skip comment
            for (int i = 1; i <= 6; i++) {
                String[] line = s.nextLine().trim().split("\\s+");
                for (int j = 1; j <= 6; j++) {
                    CommonGlobals.ccc2dp[i][j] = Double.parseDouble(line[j - 1]);
                }
            }
            // Save zero pressure values
            for (int i = 1; i <= 6; i++) {
              System.arraycopy(CommonGlobals.ccc2[i], 1, CommonGlobals.ccc2p0[i], 1, 6);
            }
        }

        // --- READS SINGLE CRYSTAL THERMAL EXPANSION COEFFICIENTS ---
        prosa = s.nextLine(); // Skip comment
        String[] line = s.nextLine().trim().split("\\s+");
        for (int i = 1; i <= 6; i++) {
            CommonGlobals.alfacc[i] = Double.parseDouble(line[i - 1]);
        }

        // --- READS CRYSTALLOGRAPHIC MODE PARAMETERS ---
        prosa = s.nextLine(); // Skip comment
        ModDataCrystal.nmodesx = Integer.parseInt(IOUtils.stringArrayFromLine(s.nextLine()).elementAt(0));
        int nmodes_local = Integer.parseInt(IOUtils.stringArrayFromLine(s.nextLine()).elementAt(0)); // Local var

        CommonGlobals.NMOD = nmodes_local;
        AllocationRoutines.allocateForNMOD();
        ModDataCrystal.mode = new int[CommonGlobals.NMOD + 1]; // 1-based

        // The `if (nmodes.gt.NMOD)` check in Fortran is dead code
        // as NMOD was just set to nmodes.

        line = s.nextLine().trim().split("\\s+");
        for (int i = 1; i <= nmodes_local; i++) {
            ModDataCrystal.mode[i] = Integer.parseInt(line[i - 1]);
        }

        ModDataCrystal.im = 1;     // counter for number of modes
        ModDataCrystal.isys = 0;   // counter for number of systems
        CommonGlobals.nslsys = 0;
        CommonGlobals.ntwsys = 0;
        CommonGlobals.nslmod = 0;
        CommonGlobals.ntwmod = 0;
        
        // NSLS = 100 ! initially
        CommonGlobals.NSLS = 100;
        AllocationRoutines.allocateForNSLS();

        // --- loops over all modes in input file ---
        for (ModDataCrystal.iloop = 1; ModDataCrystal.iloop <= ModDataCrystal.nmodesx; ModDataCrystal.iloop++) {
            namesys = IOUtils.stringArrayFromLine(s.nextLine()).elementAt(0);
            line = s.nextLine().trim().split("\\s+");
            ModDataCrystal.modex = Integer.parseInt(line[0]);
            ModDataCrystal.nsmx = Integer.parseInt(line[1]);
            ModDataCrystal.nrsx = Integer.parseInt(line[2]);
            ModDataCrystal.iopsysx = Integer.parseInt(line[3]);
            ModDataCrystal.itwx = Integer.parseInt(line[4]);

            if (ModDataCrystal.itwx == 1) {
                ModDataCrystal.stwx = Double.parseDouble(IOUtils.stringArrayFromLine(s.nextLine()).elementAt(0));
            }

            if (ModDataCrystal.nsmx > CommonGlobals.NSLS) {
                CommonGlobals.NSLS = ModDataCrystal.nsmx;
                AllocationRoutines.resizeForNSLS();
            }

            if (ModDataCrystal.modex != ModDataCrystal.iloop) {
                throw new RuntimeException(" WARNING !!! MODE NUMBERS MUST BE SEQUENTIAL IN CRYSTAL FILE");
            }

            if (ModDataCrystal.iloop != ModDataCrystal.mode[ModDataCrystal.im]) {
                // This mode is not used, skip its system definitions
                for (int is = 1; is <= ModDataCrystal.nsmx; is++) {
                    s.nextLine(); // Skip line
                }
            } else {
                // This mode *is* used
                if (ModDataCrystal.iopsysx == 1 && ModDataCrystal.itwx == 1) {
                    throw new RuntimeException(" WARNING !!! IOPSYSX MUST BE =0 WHEN ITWX=1");
                }

                CommonGlobals.nsm[ModDataCrystal.im] = (ModDataCrystal.iopsysx + 1) * ModDataCrystal.nsmx;
                CommonGlobals.itw[ModDataCrystal.im] = ModDataCrystal.itwx;

                if (ModDataCrystal.itwx == 0) { // slip
                    CommonGlobals.stw[ModDataCrystal.im] = 0;
                    CommonGlobals.nslmod++;
                    CommonGlobals.nslsys += CommonGlobals.nsm[ModDataCrystal.im];
                } else { // twin
                    CommonGlobals.stw[ModDataCrystal.im] = ModDataCrystal.stwx;
                    CommonGlobals.itwinning = 1; // Setting flag
                    CommonGlobals.ntwmod++;
                    CommonGlobals.ntwsys += CommonGlobals.nsm[ModDataCrystal.im];
                }

                // --- Reads Miller indices of systems ---
                for (int is = 1; is <= ModDataCrystal.nsmx; is++) {
                    line = s.nextLine().trim().split("\\s+");
                    for (int i = 1; i <= ModDataCrystal.nind; i++) {
                        ModDataCrystal.isn[i] = Integer.parseInt(line[i - 1]);
                        ModDataCrystal.isb[i] = Integer.parseInt(line[i - 1 + ModDataCrystal.nind]);
                    }

                    // Call crystal_symmetry with iopt=2
                    CalculationRoutines.crystalSymmetry(2, null, icrysym,
                        ModDataCrystal.isn, ModDataCrystal.sn, ModDataCrystal.sneq,
                        ModDataCrystal.isb, ModDataCrystal.sb, npolesHolder);

                    ModDataCrystal.prod = ModDataCrystal.sn[1] * ModDataCrystal.sb[1]
                                        + ModDataCrystal.sn[2] * ModDataCrystal.sb[2]
                                        + ModDataCrystal.sn[3] * ModDataCrystal.sb[3];

                    if (ModDataCrystal.prod >= 0.000001) {
                        System.err.println(" SYSTEM IS NOT ORTHOGONAL !!");
                        System.err.printf(" ISN=%7d%7d%7d%n", ModDataCrystal.isn[1], ModDataCrystal.isn[2], ModDataCrystal.isn[3]);
                        System.err.printf(" ISB=%7d%7d%7d%n", ModDataCrystal.isb[1], ModDataCrystal.isb[2], ModDataCrystal.isb[3]);
                        System.err.printf("   N=%7.3f%7.3f%7.3f%n", ModDataCrystal.sn[1], ModDataCrystal.sn[2], ModDataCrystal.sn[3]);
                        System.err.printf("   B=%7.3f%7.3f%7.3f%n", ModDataCrystal.sb[1], ModDataCrystal.sb[2], ModDataCrystal.sb[3]);
                        throw new RuntimeException("SYSTEM IS NOT ORTHOGONAL !!");
                    }

                    ModDataCrystal.isys++;
                    for (int i = 1; i <= 3; i++) {
                        CommonGlobals.bcc[i][ModDataCrystal.isys] = ModDataCrystal.sb[i];
                        CommonGlobals.ncc[i][ModDataCrystal.isys] = ModDataCrystal.sn[i];
                        for (int j = 1; j <= 3; j++) {
                            CommonGlobals.mc2[i][j][ModDataCrystal.isys] = 0.5 * (ModDataCrystal.sb[i] * ModDataCrystal.sn[j]
                                                                                + ModDataCrystal.sb[j] * ModDataCrystal.sn[i]);
                            CommonGlobals.qc2[i][j][ModDataCrystal.isys] = 0.5 * (ModDataCrystal.sb[i] * ModDataCrystal.sn[j]
                                                                                - ModDataCrystal.sb[j] * ModDataCrystal.sn[i]);
                        }
                    }

                    if (ModDataCrystal.iopsysx == 1) {
                        // Define opposite system
                        ModDataCrystal.isys++;
                        for (int i = 1; i <= 3; i++) {
                            CommonGlobals.bcc[i][ModDataCrystal.isys] = -ModDataCrystal.sb[i];
                            CommonGlobals.ncc[i][ModDataCrystal.isys] = ModDataCrystal.sn[i];
                            for (int j = 1; j <= 3; j++) {
                                CommonGlobals.mc2[i][j][ModDataCrystal.isys] = -CommonGlobals.mc2[i][j][ModDataCrystal.isys - 1];
                                CommonGlobals.qc2[i][j][ModDataCrystal.isys] = -CommonGlobals.qc2[i][j][ModDataCrystal.isys - 1];
                            }
                        }
                    }
                } // end loop over systems 'is'

                ModDataCrystal.im++;
                if (ModDataCrystal.im > CommonGlobals.NMOD) {
                    // This dynamically resizes the NMOD-based arrays
                    CommonGlobals.NMOD = ModDataCrystal.im;
                    AllocationRoutines.resizeForNMOD();
                }
                if (ModDataCrystal.isys > CommonGlobals.NSLS) {
                    // This dynamically resizes the NSLS-based arrays
                    CommonGlobals.NSLS = ModDataCrystal.isys;
                    AllocationRoutines.resizeForNSLS();
                }

            } // end if(iloop == mode(im))
            
        } // end loop over modes 'iloop'

        CommonGlobals.nsys = ModDataCrystal.isys;

        // --- Set up system<->mode mapping arrays ---
        int nst = 0;
        for (int im_loop = 1; im_loop <= nmodes_local; im_loop++) {
            for (int is = 1; is <= CommonGlobals.nsm[im_loop]; is++) {
                nst++;
                CommonGlobals.iSysMode[nst] = im_loop;
                CommonGlobals.mode_slip[im_loop][is] = nst;
                if (CommonGlobals.itw[im_loop] == 1) {
                    CommonGlobals.iTwinSys[nst] = 1;
                } else {
                    CommonGlobals.iTwinSys[nst] = 0;
                }
            }
        }

        // --- Initialize values for fijph ---
        ModDataCrystal.da = CommonGlobals.eulerph[1];
        ModDataCrystal.db = CommonGlobals.eulerph[2];
        ModDataCrystal.dc = CommonGlobals.eulerph[3];

        for (int i = 1; i <= 3; i++) {
            CommonGlobals.axisph[0][i] = CommonGlobals.axis[i];
        }

        TensorUtils.eulerFromAngles(ModDataCrystal.da, ModDataCrystal.db, ModDataCrystal.dc, ModDataCrystal.aaa);
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                // fijx(i,j)=(i/j)*(j/i)*AXISPH(0,I)
                // This is (i==j ? 1 : 0) * axisph(0,i)
                ModDataCrystal.fijx[i][j] = (i == j) ? (int)CommonGlobals.axisph[0][i] : 0;
            }
        }

        for (int j = 1; j <= 3; j++) {
            for (int i = 1; i <= 3; i++) {
                ModDataCrystal.fnew[i][j] = 0;
                for (int m = 1; m <= 3; m++) {
                    // fnew(i,j) = sum_m( aaa(m,i) * fijx(m,j) )
                    // This is fnew = aaa^T * fijx
                    ModDataCrystal.fnew[i][j] += (int)(ModDataCrystal.aaa[m][i] * ModDataCrystal.fijx[m][j]);
                }
            }
        }

        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                CommonGlobals.fijph[i][j] = ModDataCrystal.fnew[i][j];
            }
        }

    }

/**
     * Reads the PROCESS data file.
     * Translation of Fortran SUBROUTINE data_process.
     *
     * @param fileproc   Path to the process file.
     * @param i_temp_cij (Out) Wrapper for i_temp_cij flag.
     * @param i_ref_et   (Out) Wrapper for i_ref_et flag.
     * @param i_ref_st   (Out) Wrapper for i_ref_st flag.
     * @param i_bc_mode  (Out) Wrapper for i_bc_mode flag.
     */
    public static void dataProcess(String fileproc, IntHolder i_temp_cij,
                                   IntHolder i_ref_et, IntHolder i_ref_st,
                                   IntHolder i_bc_mode) {

        Scanner s = null; // Local scanner for unit 1
        File inputFile = new File(fileproc);

        try {
            // --- Dump file to output_unit_11.txt ---
            if (IOUtils.writer11 != null) {
                IOUtils.writer11.println();
                IOUtils.writer11.println(" ****** PROCESS DATA FILE *******");
                Scanner dumpScanner = new Scanner(inputFile);
                dumpScanner.useLocale(Locale.US);
                while (dumpScanner.hasNextLine()) {
                    String prosa = dumpScanner.nextLine();
                    IOUtils.writer11.println(prosa);
                }
                dumpScanner.close();
                IOUtils.writer11.println(" ****** END OF PROCESS DATA FILE ******");
                IOUtils.writer11.println();
            }

            // --- REWIND 1 (Open the scanner for actual reading) ---
            // Close the *previous* scanner if it's open
            if (IOUtils.scanner1 != null) {
                IOUtils.scanner1.close();
            }
            // Open the new file and assign it to the static scanner
            s = new Scanner(inputFile);
            s.useLocale(Locale.US);
            IOUtils.scanner1 = s;

            // --- Read data ---
            String prosa;
            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment

            CommonGlobals.nsteps = Integer.parseInt(s.nextLine().trim());
            CommonGlobals.i_control_var = Integer.parseInt(s.nextLine().trim());
            i_bc_mode.value = Integer.parseInt(s.nextLine().trim());
//            i_bc_mode = i_bc_mode.value; // Also set global

            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment

            // Read 9 flags for full strain tensor
            String[] line1 = s.nextLine().trim().split("\\s+");
            String[] line2 = s.nextLine().trim().split("\\s+");
            String[] line3 = s.nextLine().trim().split("\\s+");
            CommonGlobals.ifulletbc[1][1] = Integer.parseInt(line1[0]);
            CommonGlobals.ifulletbc[1][2] = Integer.parseInt(line1[1]);
            CommonGlobals.ifulletbc[1][3] = Integer.parseInt(line1[2]);
            CommonGlobals.ifulletbc[2][1] = Integer.parseInt(line2[0]);
            CommonGlobals.ifulletbc[2][2] = Integer.parseInt(line2[1]);
            CommonGlobals.ifulletbc[2][3] = Integer.parseInt(line2[2]);
            CommonGlobals.ifulletbc[3][1] = Integer.parseInt(line3[0]);
            CommonGlobals.ifulletbc[3][2] = Integer.parseInt(line3[1]);
            CommonGlobals.ifulletbc[3][3] = Integer.parseInt(line3[2]);

            // Update old 6 component flags
            CommonGlobals.ietbc[1] = CommonGlobals.ifulletbc[1][1];
            CommonGlobals.ietbc[2] = CommonGlobals.ifulletbc[2][2];
            CommonGlobals.ietbc[3] = CommonGlobals.ifulletbc[3][3];
            CommonGlobals.ietbc[4] = CommonGlobals.ifulletbc[2][3] * CommonGlobals.ifulletbc[3][2];
            CommonGlobals.ietbc[5] = CommonGlobals.ifulletbc[1][3] * CommonGlobals.ifulletbc[3][1];
            CommonGlobals.ietbc[6] = CommonGlobals.ifulletbc[1][2] * CommonGlobals.ifulletbc[2][1];

            prosa = s.nextLine(); // Skip comment

            // Read full strain tensor
            line1 = s.nextLine().trim().split("\\s+");
            line2 = s.nextLine().trim().split("\\s+");
            line3 = s.nextLine().trim().split("\\s+");
            CommonGlobals.fulletbc[1][1] = Double.parseDouble(line1[0]);
            CommonGlobals.fulletbc[1][2] = Double.parseDouble(line1[1]);
            CommonGlobals.fulletbc[1][3] = Double.parseDouble(line1[2]);
            CommonGlobals.fulletbc[2][1] = Double.parseDouble(line2[0]);
            CommonGlobals.fulletbc[2][2] = Double.parseDouble(line2[1]);
            CommonGlobals.fulletbc[2][3] = Double.parseDouble(line2[2]);
            CommonGlobals.fulletbc[3][1] = Double.parseDouble(line3[0]);
            CommonGlobals.fulletbc[3][2] = Double.parseDouble(line3[1]);
            CommonGlobals.fulletbc[3][3] = Double.parseDouble(line3[2]);

            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment

            // Read istbc (stress flags) in triangular form
            line1 = s.nextLine().trim().split("\\s+");
            line2 = s.nextLine().trim().split("\\s+");
            line3 = s.nextLine().trim().split("\\s+");
            CommonGlobals.istbc[1] = Integer.parseInt(line1[0]);
            CommonGlobals.istbc[6] = Integer.parseInt(line1[1]);
            CommonGlobals.istbc[5] = Integer.parseInt(line1[2]);
            CommonGlobals.istbc[2] = Integer.parseInt(line2[0]);
            CommonGlobals.istbc[4] = Integer.parseInt(line2[1]);
            CommonGlobals.istbc[3] = Integer.parseInt(line3[0]);

            prosa = s.nextLine(); // Skip comment

            // Read stbc (stress values) in triangular form
            line1 = s.nextLine().trim().split("\\s+");
            line2 = s.nextLine().trim().split("\\s+");
            line3 = s.nextLine().trim().split("\\s+");
            CommonGlobals.stbc[1] = Double.parseDouble(line1[0]);
            CommonGlobals.stbc[6] = Double.parseDouble(line1[1]);
            CommonGlobals.stbc[5] = Double.parseDouble(line1[2]);
            CommonGlobals.stbc[2] = Double.parseDouble(line2[0]);
            CommonGlobals.stbc[4] = Double.parseDouble(line2[1]);
            CommonGlobals.stbc[3] = Double.parseDouble(line3[0]);

            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment
            CommonGlobals.temp_s = Double.parseDouble(s.nextLine().trim());
            prosa = s.nextLine(); // Skip comment
            CommonGlobals.deltemp = Double.parseDouble(s.nextLine().trim());
            prosa = s.nextLine(); // Skip comment
            i_temp_cij.value = Integer.parseInt(s.nextLine().trim());
            prosa = s.nextLine(); // Skip comment
            i_ref_et.value = Integer.parseInt(s.nextLine().trim());
            prosa = s.nextLine(); // Skip comment
            i_ref_st.value = Integer.parseInt(s.nextLine().trim());

        } catch (FileNotFoundException e) {
            throw new RuntimeException("Process file not found: " + fileproc, e);
        }
        // finally {
        //    if (s != null) {
        //        s.close(); // Close the local scanner
        //    }
        //}
        // NOTE: We do NOT close the scanner here in 'finally'.
        // It's closed by the *next* routine that opens unit 1,
        // or by closeIO() at the end.
        
        // --- UPDATE THIS: The Fortran code *does* close unit 1.
        // --- My previous assumption was wrong.
        // --- The code `CLOSE(unit=1)` is at the end.
        // --- This means `dataCrystal` is the *only* one that
        // --- needs to be special.
        
        // --- CORRECTED LOGIC ---
        // All routines *except* dataCrystal should open and close their
        // own local scanner.
        // dataCrystal opens IOUtils.scanner1.
        // crssVoce(0) *uses* IOUtils.scanner1.
        // The *main program* (which I don't have) must call CLOSE(unit=1)
        // after crssVoce(0).
        
        // THEREFORE: My original translations of dataProcess, dataSample,
        // difPlanes, and PoleFig (using a local `Scanner s` in a 
        // try-with-resources or finally-close block) were CORRECT.
        // My translation of dataCrystal (above) is CORRECT.
        
        // I will proceed with this logic.
    }

    /**
     * Reads SAMPLE data (texture), calculates grain rotation matrices,
     * and initializes grain state.
     * Translation of Fortran SUBROUTINE data_sample.
     *
     * @param filesamp    Path to the sample/texture file.
     * @param i_prev_proc Flag (1) to read state from a previous file.
     * @param fileprev    Path to the previous state file.
     */
    public static void dataSample(String filesamp, int i_prev_proc, String fileprev) {

        Scanner s = null; // Local scanner for unit 1
        File inputFile = new File(filesamp);
        String prosa;

        try {
            // --- Open the file with the SAMPLE data "filesamp" ---
            s = new Scanner(inputFile);
            s.useLocale(Locale.US);

            // Dump file to writers 11 and 12 (assuming 12 is System.out)
            if (IOUtils.writer11 != null) {
                IOUtils.writer11.println("******** SAMPLE data - File: " + filesamp + "*********");
                IOUtils.writer11.println("******************************************************************************");
            }
            System.out.println("******** SAMPLE data - File: " + filesamp + "*********");
            System.out.println("******************************************************************************");
            
            // --- WRITES TEXTURE DATA FILE INTO 'EPSC1.OUT' ---
            if (IOUtils.writer11 != null) {
                IOUtils.writer11.println();
                IOUtils.writer11.println(" ****** TEXTURE DATA FILE (partial) *******");
                Scanner dumpScanner = new Scanner(inputFile); // New scanner for dumping
                dumpScanner.useLocale(Locale.US);
                for (int idum = 1; idum <= 20 && dumpScanner.hasNextLine(); idum++) {
                    prosa = dumpScanner.nextLine();
                    IOUtils.writer11.println(prosa);
                }
                dumpScanner.close();
                IOUtils.writer11.println(" ****** END OF TEXTURE DATA FILE ******");
                IOUtils.writer11.println();
            }
            // (Scanner 's' is still open and at the start of the file)
            
            // --- Read texture data ---
            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment
            prosa = s.nextLine(); // Skip comment
            
            String[] line = s.nextLine().trim().split("\\s+");
            String eul_conv = line[0];
            int ngrain = Integer.parseInt(line[1]);

            CommonGlobals.NGR = ngrain;
            AllocationRoutines.allocateForNGR(); // Allocate all NGR-dependent arrays

          for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= CommonGlobals.NGR; j++) {
              CommonGlobals.axisgr[0][i][j] = CommonGlobals.axis[i];
            }
          }

          if (CommonGlobals.ishape >= 2) {
            for (int k = 1; k <= CommonGlobals.NGR; k++) {
              for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                  CommonGlobals.fijgr[i][j][k] = CommonGlobals.fijph[i][j];
                }
              }
            }
          }

            // The 'if (ngrain.gt.NGR)' check is handled by dynamic allocation
            
            CommonGlobals.ngParent = ngrain;
//            System.out.printf("%s%5d%5d%n", eul_conv, ngrain, 0); // Mimic format 11
            
            // Read all grain data
            for (int i = 1; i <= ngrain; i++) {
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.phi[i] = Double.parseDouble(line[0]);
                CommonGlobals.the[i] = Double.parseDouble(line[1]);
                CommonGlobals.ome[i] = Double.parseDouble(line[2]);
                CommonGlobals.wgt[i] = Double.parseDouble(line[3]);
            }

        } catch (FileNotFoundException e) {
            throw new RuntimeException("Sample file not found: " + filesamp, e);
        } finally {
            if (s != null) {
                s.close(); // Close the sample file scanner
            }
        }
        
        // --- Calculates rotation matrix for each grain ---
        ModDataSample.totwgt = 0.0;
        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            ModDataSample.totwgt += CommonGlobals.wgt[ng];
        }

        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            // call euler(2,phi(ng),the(ng),ome(ng),aux33)
            TensorUtils.eulerFromAngles(CommonGlobals.phi[ng], CommonGlobals.the[ng], CommonGlobals.ome[ng],
                                        ModDataSample.aux33);
            
            if (Math.abs(ModDataSample.totwgt) > 1.0e-9) {
                CommonGlobals.wgt[ng] /= ModDataSample.totwgt;
            }
            
            // Store rotation matrix r(i,j,ng)
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                    CommonGlobals.r[i][j][ng] = ModDataSample.aux33[i][j];
                }
            }

            // Initialize child/parent twin flags
            for (int i = 1; i <= CommonGlobals.NSLS; i++) {
                CommonGlobals.iChildGrain[i][ng] = 0;
            }
            CommonGlobals.iParentGrain[ng] = 0;
            CommonGlobals.iParentSystem[ng] = 0;
            CommonGlobals.iTwinLevel[ng] = 0;
        }

        // --- Initializes critical stress and accumulated shear in each grain. ---
        if (CommonGlobals.kCL == 0) {
            crssVoce(1, 0); // Pass 0 for dummy ng
        }
        if (CommonGlobals.kCL == 2) {
            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                crssDislocDens(1, ng);
            }
        }

        // --- Reads grains' & sample state from previous procedure ---
        if (i_prev_proc == 1) {
            File prevFile = new File(fileprev);
            try {
                s = new Scanner(prevFile); // Re-open unit 1 with fileprev
                s.useLocale(Locale.US);

                for (int i = 1; i <= 6; i++) prosa = s.nextLine(); // Skip headers

                for (int i = 1; i <= 6; i++) {
                    String[] line = s.nextLine().trim().split("\\s+");
                    for (int j = 1; j <= 6; j++) {
                        CommonGlobals.css2[i][j] = Double.parseDouble(line[j - 1]);
                    }
                }
                prosa = s.nextLine(); // Skip header
                String[] line = s.nextLine().trim().split("\\s+");
                for (int i = 1; i <= 6; i++) CommonGlobals.alfass[i] = Double.parseDouble(line[i - 1]);
                
                prosa = s.nextLine(); // Skip header
                for (int i = 1; i <= 6; i++) {
                    line = s.nextLine().trim().split("\\s+");
                    for (int j = 1; j <= 6; j++) {
                        CommonGlobals.ass2[i][j] = Double.parseDouble(line[j - 1]);
                    }
                }

                prosa = s.nextLine(); // Skip header
                for (int i = 1; i <= 6; i++) {
                    line = s.nextLine().trim().split("\\s+");
                    for (int j = 1; j <= 6; j++) {
                        CommonGlobals.aef[i][j] = Double.parseDouble(line[j - 1]);
                    }
                }
                
                prosa = s.nextLine(); // Skip header
                line = s.nextLine().trim().split("\\s+");
                for (int i = 1; i <= 6; i++) CommonGlobals.stss[i] = Double.parseDouble(line[i - 1]);
                line = s.nextLine().trim().split("\\s+");
                for (int i = 1; i <= 6; i++) CommonGlobals.etss[i] = Double.parseDouble(line[i - 1]);
                line = s.nextLine().trim().split("\\s+");
                for (int i = 1; i <= 6; i++) CommonGlobals.etelss[i] = Double.parseDouble(line[i - 1]);

                prosa = s.nextLine(); // Skip header
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    line = s.nextLine().trim().split("\\s+");
                    for (int i = 1; i <= 6; i++) CommonGlobals.stcs[i][ng] = Double.parseDouble(line[i - 1]);
                    line = s.nextLine().trim().split("\\s+");
                    for (int i = 1; i <= 6; i++) CommonGlobals.etcs[i][ng] = Double.parseDouble(line[i - 1]);
                }
                
                prosa = s.nextLine(); // Skip header
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    line = s.nextLine().trim().split("\\s+");
                    for (int ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
                        CommonGlobals.tau[ns1][ng] = Double.parseDouble(line[ns1 - 1]);
                    }
                    CommonGlobals.gamtot[ng] = Double.parseDouble(line[CommonGlobals.nsys]); // Last value
                    
                    // Renormalize tau
                    for (int ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
                        CommonGlobals.tau[ns1][ng] *= CommonGlobals.tau0[ns1];
                    }
                }

            } catch (FileNotFoundException e) {
                throw new RuntimeException("Previous state file not found: " + fileprev, e);
            } finally {
                if (s != null) {
                    s.close(); // Close the previous state file scanner
                }
            }
        } // end if (i_prev_proc == 1)
    }
    
/**
     * Reads diffraction plane data and calculates lattice strains.
     * Translation of Fortran SUBROUTINE dif_planes.
     *
     * @param icrysym Crystal symmetry ID (from dataCrystal).
     * @param filediff Path to the diffraction data file.
     * @param temp     Current temperature.
     * @param istep    Current process step.
     * @param iopt     Option flag (0=Init, 1=Calculate).
     */
    public static void difPlanes(int icrysym, String filediff, double temp, int istep, int iopt) {

        String prosa;
        Scanner s = null; // Local scanner for unit 1

        // iopt 0: Initialization
        if (iopt == 0) {
            File inputFile = new File(filediff);
            try {
                // --- Dump file to output_unit_11.txt ---
                if (IOUtils.writer11 != null) {
                    IOUtils.writer11.println();
                    IOUtils.writer11.println(" ****** DIFFRACTION DATA FILE *******");
                    Scanner dumpScanner = new Scanner(inputFile);
                    dumpScanner.useLocale(Locale.US);
                    while (dumpScanner.hasNextLine()) {
                        prosa = dumpScanner.nextLine();
                        IOUtils.writer11.println(prosa);
                    }
                    dumpScanner.close();
                    IOUtils.writer11.println(" ****** END OF DIFFRACTION DATA FILE ******");
                    IOUtils.writer11.println();
                }

                // --- REWIND 1 (Open scanner for actual reading) ---
                s = new Scanner(inputFile);
                s.useLocale(Locale.US);

                // --- Read headers ---
                prosa = s.nextLine(); if (IOUtils.writer19 != null) IOUtils.writer19.println(prosa);
                prosa = s.nextLine(); if (IOUtils.writer19 != null) IOUtils.writer19.println(prosa);

                String[] line = s.nextLine().trim().split("\\s+");
                int ndiff = Integer.parseInt(line[0]);
                ModDifPlanes.angdetector = Double.parseDouble(line[1]);

                CommonGlobals.NDIFFX = ndiff;
                AllocationRoutines.allocateForNDIFFX();

                // Allocate module arrays
                ModDifPlanes.nfamily = new int[CommonGlobals.NDIFFX + 1];
                ModDifPlanes.vs = new double[CommonGlobals.NDIFFX + 1][4];
                ModDifPlanes.para_w = new double[CommonGlobals.NDIFFX + 1];
                ModDifPlanes.vc = new double[CommonGlobals.NDIFFX + 1][4][25];
                ModDifPlanes.para_w_dev = new double[CommonGlobals.NDIFFX + 1];
                ModDifPlanes.para_sq_dev = new double[CommonGlobals.NDIFFX + 1];
                ModDifPlanes.para_sq = new double[CommonGlobals.NDIFFX + 1];

                prosa = s.nextLine(); if (IOUtils.writer19 != null) IOUtils.writer19.println(prosa);
                prosa = s.nextLine(); if (IOUtils.writer19 != null) IOUtils.writer19.println(prosa);

                ModDifPlanes.nind = 3;
                if (icrysym == 2 || icrysym == 3) ModDifPlanes.nind = 4;

                IntHolder nfamilyxHolder = new IntHolder(0);

                // --- Loop over all diffraction settings ---
                for (int n = 1; n <= ndiff; n++) {
                    line = s.nextLine().trim().split("\\s+");
                    for (int i = 1; i <= ModDifPlanes.nind; i++) {
                        ModDifPlanes.isn[i] = Integer.parseInt(line[i - 1]);
                    }
                    ModDifPlanes.chi = Double.parseDouble(line[ModDifPlanes.nind]);
                    ModDifPlanes.eta = Double.parseDouble(line[ModDifPlanes.nind + 1]);

                    // Print to writer 19
                    if (IOUtils.writer19 != null) {
                        if (ModDifPlanes.nind == 3) {
                            IOUtils.writer19.printf("%4d%4d%4d%10.1f%10.1f%n", 
                                ModDifPlanes.isn[1], ModDifPlanes.isn[2], ModDifPlanes.isn[3], 
                                ModDifPlanes.chi, ModDifPlanes.eta);
                        } else {
                            IOUtils.writer19.printf("%4d%4d%4d%4d%10.1f%10.1f%n", 
                                ModDifPlanes.isn[1], ModDifPlanes.isn[2], ModDifPlanes.isn[3], ModDifPlanes.isn[4], 
                                ModDifPlanes.chi, ModDifPlanes.eta);
                        }
                    }

                    double eta1 = ModDifPlanes.eta * CommonConstants.DEG_TO_RAD;
                    double chi1 = ModDifPlanes.chi * CommonConstants.DEG_TO_RAD;
                    ModDifPlanes.ps[1] = Math.cos(eta1) * Math.sin(chi1);
                    ModDifPlanes.ps[2] = Math.sin(eta1) * Math.sin(chi1);
                    ModDifPlanes.ps[3] = Math.cos(chi1);

                    // Call crystal_symmetry (iopt=3)
                    crystalSymmetry(3, null, new IntHolder(icrysym), 
                                    ModDifPlanes.isn, ModDifPlanes.sn, ModDifPlanes.pc, 
                                    ModDifPlanes.isb, ModDifPlanes.sb, nfamilyxHolder);
                    
                    ModDifPlanes.nfamily[n] = nfamilyxHolder.value;
                    ModDifPlanes.toler = Math.cos(ModDifPlanes.angdetector * CommonConstants.DEG_TO_RAD);
                    CommonGlobals.RAND_WGT[n] = ModDifPlanes.nfamily[n] * (1.0 - ModDifPlanes.toler);

                    // Store vectors
                    for (int i = 1; i <= 3; i++) {
                        ModDifPlanes.vs[n][i] = ModDifPlanes.ps[i];
                        for (int j = 1; j <= ModDifPlanes.nfamily[n]; j++) {
                            ModDifPlanes.vc[n][i][j] = ModDifPlanes.pc[i][j];
                        }
                    }
                } // end do n=1,ndiff
            } catch (FileNotFoundException e) {
                throw new RuntimeException("Diffraction file not found: " + filediff, e);
            } finally {
                if (s != null) s.close();
            }
        } // end iopt == 0

        // --- Determine grains in each hkl set ---
        ModDifPlanes.isw = 0;
        if (iopt == 1 && (CommonGlobals.itwinning + CommonGlobals.irot >= 1)) ModDifPlanes.isw = 1;
        if (iopt == 0 && (CommonGlobals.itwinning + CommonGlobals.irot == 0)) ModDifPlanes.isw = 1;

        if (ModDifPlanes.isw == 1) {
            for (int n = 1; n <= CommonGlobals.NDIFFX; n++) {
                CommonGlobals.wgtset[n] = 0.0;
                CommonGlobals.ngrset[n] = 0;
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    for (int ipl = 1; ipl <= ModDifPlanes.nfamily[n]; ipl++) {
                        // Rotate plane normal to sample coords: ps = R_transpose * vc
                        for (int i = 1; i <= 3; i++) {
                            ModDifPlanes.ps[i] = 0.0;
                            for (int j = 1; j <= 3; j++) {
                                ModDifPlanes.ps[i] += CommonGlobals.r[j][i][ng] * ModDifPlanes.vc[n][j][ipl];
                            }
                        }
                        // Dot product: prodesc = ps . vs
                        ModDifPlanes.prodesc = 0.0;
                        for (int i = 1; i <= 3; i++) {
                            ModDifPlanes.prodesc += ModDifPlanes.ps[i] * ModDifPlanes.vs[n][i];
                        }
                        
                        if (Math.abs(ModDifPlanes.prodesc) >= ModDifPlanes.toler) {
                            CommonGlobals.ngrset[n]++;
                            CommonGlobals.igrset[n][CommonGlobals.ngrset[n]] = ng;
                            CommonGlobals.wgtset[n] += CommonGlobals.wgt[ng];
                        }
                    } // ipl
                } // ng
            } // n

            if (CommonGlobals.itwinning + CommonGlobals.irot != 0) {
                for (int n = 1; n <= CommonGlobals.NDIFFX; n++) {
                    printDifSet(n);
                }
            }
            ModDifPlanes.heading = true;
        } // end isw == 1

        if (iopt == 1 && CommonGlobals.itwinning + CommonGlobals.irot == 0) {
            for (int n = 1; n <= CommonGlobals.NDIFFX; n++) {
                printDifSet(n);
            }
            ModDifPlanes.heading = true;
        }

        // --- iopt 1: Calculate average strain for the family ---
        if (iopt == 1) {
            
            // Merkel 05/2010: Update hydrostatic strain tensor
            if (CommonGlobals.kSM == 1.0) {
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    for (int i = 1; i <= 6; i++) {
                        ModDifPlanes.etelcsx[i] = 0.0;
                        ModDifPlanes.etelhycsx[i] = 0.0;
                        ModDifPlanes.p = (CommonGlobals.stcs[1][ng] + CommonGlobals.stcs[2][ng] + CommonGlobals.stcs[3][ng]) / 3.0;
                        ModDifPlanes.pref = (CommonGlobals.stcsref[1][ng] + CommonGlobals.stcsref[2][ng] + CommonGlobals.stcsref[3][ng]) / 3.0;
                        
                        ModDifPlanes.p_incr[1] = ModDifPlanes.p - ModDifPlanes.pref;
                        ModDifPlanes.p_incr[2] = ModDifPlanes.p - ModDifPlanes.pref;
                        ModDifPlanes.p_incr[3] = ModDifPlanes.p - ModDifPlanes.pref;
                        ModDifPlanes.p_incr[4] = 0.0;
                        ModDifPlanes.p_incr[5] = 0.0;
                        ModDifPlanes.p_incr[6] = 0.0;

                        for (int j = 1; j <= 6; j++) {
                            ModDifPlanes.etelhycsx[i] += CommonGlobals.scs2[i][j][ng] * ModDifPlanes.p_incr[j] * CommonGlobals.profac[j];
                            ModDifPlanes.etelcsx[i] += CommonGlobals.scs2[i][j][ng] * (CommonGlobals.stcs[j][ng] - CommonGlobals.stcsref[j][ng]) * CommonGlobals.profac[j];
                        }
                        
                        CommonGlobals.etelcs[i][ng] = (1.0 + CommonGlobals.etelcs[i][ng]) * ModDifPlanes.etelcsx[i] + CommonGlobals.etelcs[i][ng];
                        CommonGlobals.etelhycs[i][ng] = (1.0 + CommonGlobals.etelhycs[i][ng]) * ModDifPlanes.etelhycsx[i] + CommonGlobals.etelhycs[i][ng];
                    } // i
                } // ng
            } // kSM == 1

            // --- Loop over diffraction planes and calculate strains ---
            for (int n = 1; n <= CommonGlobals.NDIFFX; n++) {
                ModDifPlanes.para_w[n] = 0.0;
                ModDifPlanes.para_sq[n] = 0.0;
                ModDifPlanes.para_w_dev[n] = 0.0;
                ModDifPlanes.para_sq_dev[n] = 0.0;

                for (int ng_idx = 1; ng_idx <= CommonGlobals.ngrset[n]; ng_idx++) {
                    int igset = CommonGlobals.igrset[n][ng_idx];
                    ModDifPlanes.eps = 0.0;
                    ModDifPlanes.eps_dev = 0.0;

                    if (CommonGlobals.kSM == 1.0) {
                        ModDifPlanes.tmpepsdev1 = 0.0;
                        ModDifPlanes.tmpepsdev2 = 0.0;
                        for (int ij = 1; ij <= 6; ij++) {
                            int i = CommonGlobals.ijv[ij][1];
                            int j = CommonGlobals.ijv[ij][2];
                            ModDifPlanes.tmpepsdev1 += ModDifPlanes.vs[n][i] * ModDifPlanes.vs[n][j] * (CommonGlobals.etelcs[ij][igset] - CommonGlobals.etelhycs[ij][igset]) * CommonGlobals.profac[ij];
                            ModDifPlanes.tmpepsdev2 += ModDifPlanes.vs[n][i] * ModDifPlanes.vs[n][j] * CommonGlobals.etelhycs[ij][igset] * CommonGlobals.profac[ij];
                            
                            ModDifPlanes.eps += ModDifPlanes.vs[n][i] * ModDifPlanes.vs[n][j] * CommonGlobals.etelcs[ij][igset] * CommonGlobals.profac[ij];
                        }
                        if (Math.abs(1.0 + ModDifPlanes.tmpepsdev2) > 1.0e-9) {
                            ModDifPlanes.eps_dev = ModDifPlanes.tmpepsdev1 / (1.0 + ModDifPlanes.tmpepsdev2);
                        }
                    } else { // Regular case (kSM != 1)
                        for (int i = 1; i <= 6; i++) {
                            ModDifPlanes.etelcsx[i] = 0.0;
                            for (int j = 1; j <= 6; j++) {
                                ModDifPlanes.etelcsx[i] += CommonGlobals.scs2[i][j][igset] * (CommonGlobals.stcs[j][igset] - CommonGlobals.stcsref[j][igset]) * CommonGlobals.profac[j];
                            }
                            // Add thermal strains
                            ModDifPlanes.etelcsx[i] += CommonGlobals.etthcs[i][igset];
                        }
                        for (int ij = 1; ij <= 6; ij++) {
                            int i = CommonGlobals.ijv[ij][1];
                            int j = CommonGlobals.ijv[ij][2];
                            ModDifPlanes.eps += ModDifPlanes.vs[n][i] * ModDifPlanes.vs[n][j] * ModDifPlanes.etelcsx[ij] * CommonGlobals.profac[ij];
                        }
                    }

                    // Accumulate weighted averages
                    ModDifPlanes.para_w[n] += ModDifPlanes.eps * CommonGlobals.wgt[igset];
                    ModDifPlanes.para_sq[n] += ModDifPlanes.eps * ModDifPlanes.eps * CommonGlobals.wgt[igset];
                    ModDifPlanes.para_w_dev[n] += ModDifPlanes.eps_dev * CommonGlobals.wgt[igset];
                    ModDifPlanes.para_sq_dev[n] += ModDifPlanes.eps_dev * ModDifPlanes.eps_dev * CommonGlobals.wgt[igset];
                } // end loop over grains in set

                // Normalize averages and calculate std deviation
                if (Math.abs(CommonGlobals.wgtset[n]) > 1.0e-12) {
                    ModDifPlanes.para_w[n] /= CommonGlobals.wgtset[n];
                    ModDifPlanes.tmp = ModDifPlanes.para_sq[n] / CommonGlobals.wgtset[n] - ModDifPlanes.para_w[n] * ModDifPlanes.para_w[n];
                    if (Math.abs(ModDifPlanes.tmp) < 1.0e-12) ModDifPlanes.tmp = 0.0;
                    ModDifPlanes.para_sq[n] = Math.sqrt(ModDifPlanes.tmp);

                    ModDifPlanes.para_w_dev[n] /= CommonGlobals.wgtset[n];
                    ModDifPlanes.tmp = ModDifPlanes.para_sq_dev[n] / CommonGlobals.wgtset[n] - ModDifPlanes.para_w_dev[n] * ModDifPlanes.para_w_dev[n];
                    if (Math.abs(ModDifPlanes.tmp) < 1.0e-12) ModDifPlanes.tmp = 0.0;
                    ModDifPlanes.para_sq_dev[n] = Math.sqrt(ModDifPlanes.tmp);
                }
            } // end loop over diffraction planes

            // Merkel, 05/2010: Reset stress references
            if (CommonGlobals.kSM == 1.0) {
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    for (int i = 1; i <= 6; i++) {
                        CommonGlobals.stcsref[i][ng] = CommonGlobals.stcs[i][ng];
                    }
                }
            }

            // --- Write results to "EPSC9.OUT" (unit 19) ---
            if (CommonGlobals.icvx >= 7) {
                ModDifPlanes.eref = CommonGlobals.etss[CommonGlobals.icvx - 6] - CommonGlobals.etssref[CommonGlobals.icvx - 6] * 1e2;
                ModDifPlanes.sref = CommonGlobals.stss[CommonGlobals.icvx - 6];
            } else if (CommonGlobals.icvx >= 1) {
                ModDifPlanes.eref = CommonGlobals.etss[CommonGlobals.icvx] - CommonGlobals.etssref[CommonGlobals.icvx] * 1e2;
                ModDifPlanes.sref = CommonGlobals.stss[CommonGlobals.icvx];
            } else {
                ModDifPlanes.eref = temp;
            }

            if (ModDifPlanes.heading) {
                if (IOUtils.writer19 != null) {
                    IOUtils.writer19.println();
                    IOUtils.writer19.println(" Temp       Eref       Sref       dif1       dif2       dif3       dif4" +
                                             "       dif5       dif6       dif7       dif8       dif9      dif10");
                }
                ModDifPlanes.heading = false;
            }

            // --- Write to unit 20 ---
            if (IOUtils.writer20 != null) {
                PrintWriter out = IOUtils.writer20;
                out.printf("%13.5e ", ModDifPlanes.eref);
                for (int i = 1; i <= 3; i++) out.printf("%13.5e ", (CommonGlobals.etss[i] - CommonGlobals.etssref[i]) * 1e2);
                for (int i = 1; i <= 3; i++) out.printf("%13.5e ", CommonGlobals.stss[i]);
                for (int i = 1; i <= 3; i++) out.printf("%13.5e ", CommonGlobals.etav[i]);
                for (int i = 1; i <= 3; i++) out.printf("%13.5e ", CommonGlobals.stav[i]);
                for (int nd = 1; nd <= CommonGlobals.NDIFFX; nd++) out.printf("%13.5e ", ModDifPlanes.para_w[nd]);
                for (int nd = 1; nd <= CommonGlobals.NDIFFX; nd++) out.printf("%13.5e ", ModDifPlanes.para_sq[nd]);
                for (int nd = 1; nd <= CommonGlobals.NDIFFX; nd++) out.printf("%13.5e ", ModDifPlanes.para_w_dev[nd]);
                for (int nd = 1; nd <= CommonGlobals.NDIFFX; nd++) out.printf("%13.5e ", ModDifPlanes.para_sq_dev[nd]);
                for (int nd = 1; nd <= CommonGlobals.NDIFFX; nd++) out.printf("%13.5e ", CommonGlobals.wgtset[nd] / CommonGlobals.RAND_WGT[nd]);
                out.println();
            }

            // --- Write to unit 19 ---
            if (IOUtils.writer19 != null) {
                PrintWriter out = IOUtils.writer19;
                out.printf("%13.5e ", temp);
                for (int i = 1; i <= 3; i++) out.printf("%13.5e ", (CommonGlobals.etss[i] - CommonGlobals.etssref[i]));
                for (int i = 1; i <= 3; i++) out.printf("%13.5e ", CommonGlobals.stss[i]);
                for (int nd = 1; nd <= CommonGlobals.NDIFFX; nd++) out.printf("%13.5e ", ModDifPlanes.para_w[nd]);
                out.println();
            }

        } // end iopt == 1
    }

    /**
     * Helper method to print diffraction set info to writer19.
     * Mimics Fortran formats 6 and 7.
     */
    private static void printDifSet(int n) {
        if (IOUtils.writer19 != null) {
            IOUtils.writer19.println();
            IOUtils.writer19.printf("   SET #:%6d   NGRSET:%6d   VOLFRAC:%10.6f%n",
                                    n, CommonGlobals.ngrset[n], CommonGlobals.wgtset[n]);
            if (CommonGlobals.iwrite9 == 1) {
                int count = 0;
                for (int n1 = 1; n1 <= CommonGlobals.ngrset[n]; n1++) {
                    IOUtils.writer19.printf("%6d", CommonGlobals.igrset[n][n1]);
                    count++;
                    if (count % 15 == 0) {
                        IOUtils.writer19.println();
                    }
                }
                if (count % 15 != 0) {
                    IOUtils.writer19.println();
                }
            }
        }
    }
    
/**
     * Calculates and writes equivalent states and energies to unit #18.
     * Translation of Fortran SUBROUTINE EFFECTIVE_MAGNITUDES.
     *
     * @param iproc The current process number.
     * @param istep The current step number within the process.
     * @param step  The increment size (e.g., time or strain step).
     */
    public static void effectiveMagnitudes(int iproc, int istep, double step) {

        // Initialize total work only on the very first step
        if (iproc == 1 && istep == 1) {
          ModEffectiveMagnitudes.wtotal = 0.0;
          ModEffectiveMagnitudes.wplastic = 0.0;
        }

        // Reset accumulators
      ModEffectiveMagnitudes.stsseq = 0.0;
      ModEffectiveMagnitudes.strsseq = 0.0;
      ModEffectiveMagnitudes.etsseq = 0.0;
      ModEffectiveMagnitudes.etrsseq = 0.0;
      ModEffectiveMagnitudes.etsspleq = 0.0;
      ModEffectiveMagnitudes.etrsspleq = 0.0;

        for (int i = 1; i <= 6; i++) {
            // Calculate plastic strain: epsilon_pl = epsilon_total - S:sigma
          ModEffectiveMagnitudes.etsspl[i] = CommonGlobals.etss[i];
          ModEffectiveMagnitudes.etrsspl[i] = CommonGlobals.etrss[i];
            for (int j = 1; j <= 6; j++) {
              ModEffectiveMagnitudes.etsspl[i] -= CommonGlobals.sss2[i][j] * CommonGlobals.stss[j] * CommonGlobals.profac[j];
              ModEffectiveMagnitudes.etrsspl[i] -= CommonGlobals.sss2[i][j] * CommonGlobals.strss[j] * CommonGlobals.profac[j];
            }

            // Accumulate squared sums (Voigt-aware inner products)
          ModEffectiveMagnitudes.stsseq += CommonGlobals.stss[i] * CommonGlobals.stss[i] * CommonGlobals.profac[i];
          ModEffectiveMagnitudes.strsseq += CommonGlobals.strss[i] * CommonGlobals.strss[i] * CommonGlobals.profac[i];
          ModEffectiveMagnitudes.etsseq += CommonGlobals.etss[i] * CommonGlobals.etss[i] * CommonGlobals.profac[i];
          ModEffectiveMagnitudes.etrsseq += CommonGlobals.etrss[i] * CommonGlobals.etrss[i] * CommonGlobals.profac[i];
          ModEffectiveMagnitudes.etsspleq += ModEffectiveMagnitudes.etsspl[i] * ModEffectiveMagnitudes.etsspl[i] * CommonGlobals.profac[i];
          ModEffectiveMagnitudes.etrsspleq += ModEffectiveMagnitudes.etrsspl[i] * ModEffectiveMagnitudes.etrsspl[i] * CommonGlobals.profac[i];
        }

        // Finalize equivalent magnitudes with square roots and factors
      ModEffectiveMagnitudes.stsseq = Math.sqrt(3.0 / 2.0 * ModEffectiveMagnitudes.stsseq);
      ModEffectiveMagnitudes.strsseq = Math.sqrt(3.0 / 2.0 * ModEffectiveMagnitudes.strsseq);
      ModEffectiveMagnitudes.etsseq = Math.sqrt(2.0 / 3.0 * ModEffectiveMagnitudes.etsseq);
      ModEffectiveMagnitudes.etrsseq = Math.sqrt(2.0 / 3.0 * ModEffectiveMagnitudes.etrsseq);
      ModEffectiveMagnitudes.etsspleq = Math.sqrt(2.0 / 3.0 * ModEffectiveMagnitudes.etsspleq);
      ModEffectiveMagnitudes.etrsspleq = Math.sqrt(2.0 / 3.0 * ModEffectiveMagnitudes.etrsspleq);

        // Pressure and Volume
      ModEffectiveMagnitudes.pressure = -(1.0 / 3.0) * (CommonGlobals.stss[1] + CommonGlobals.stss[2] + CommonGlobals.stss[3]);
      ModEffectiveMagnitudes.volume = CommonGlobals.etss[1] + CommonGlobals.etss[2] + CommonGlobals.etss[3];

        // Work and Energy
      ModEffectiveMagnitudes.stet = 0.0;
        for (int i = 1; i <= 6; i++) {
            // Increment accumulated work: dW = (sigma : d_epsilon) * step
          ModEffectiveMagnitudes.wtotal += CommonGlobals.stss[i] * CommonGlobals.etrss[i] * CommonGlobals.profac[i] * step;
          ModEffectiveMagnitudes.wplastic += CommonGlobals.stss[i] * ModEffectiveMagnitudes.etrsspl[i] * CommonGlobals.profac[i] * step;
            
            // Strain energy: E = sigma : epsilon
          ModEffectiveMagnitudes.stet += CommonGlobals.stss[i] * CommonGlobals.etss[i] * CommonGlobals.profac[i];
        }

        // Volume-averaged strain energy
      ModEffectiveMagnitudes.stetav = 0.0;
        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            for (int i = 1; i <= 6; i++) {
              ModEffectiveMagnitudes.stetav += CommonGlobals.stcs[i][ng] * CommonGlobals.etcs[i][ng]
                        * CommonGlobals.profac[i] * CommonGlobals.wgt[ng];
            }
        }

        // Write to unit 18
        if (IOUtils.writer18 != null) {
            IOUtils.writer18.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e" +
                                    " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                ModEffectiveMagnitudes.etsseq, ModEffectiveMagnitudes.etsspleq, ModEffectiveMagnitudes.stsseq, ModEffectiveMagnitudes.etrsseq, ModEffectiveMagnitudes.etrsspleq,
                ModEffectiveMagnitudes.strsseq, ModEffectiveMagnitudes.volume, ModEffectiveMagnitudes.pressure, ModEffectiveMagnitudes.wtotal, ModEffectiveMagnitudes.wplastic, ModEffectiveMagnitudes.stet, ModEffectiveMagnitudes.stetav);
        }
    }
    
    /**
     * Initializes Eshelby integration points or calculates Eshelby tensors.
     * Translation of Fortran SUBROUTINE ESHELBYB.
     *
     * @param axis    Ellipsoid axes (1-based, [4]).
     * @param c4      Stiffness tensor (1-based, [4][4][4][4]).
     * @param keff    Effective bulk modulus.
     * @param esim    (Out) Symmetric Eshelby tensor (1-based, [4][4][4][4]).
     * @param escr    (Out) Antisymmetric Eshelby tensor (1-based, [4][4][4][4]).
     * @param ioption Control flag (0=Init, 1-5=Calculate).
     */
    public static void eshelbyb(double[] axis, double[][][][] c4, double keff,
                              double[][][][] esim, double[][][][] escr, int ioption) {

        CommonEshelby.IGAUSSLEG = 0; // Hardwires Gauss-Lobatto

        // ioption 0: INITIALIZATION RUN
        if (ioption == 0) {
            
            for (int i = 1; i <= 11; i++) {
                CommonEshelby.ngaussph[i] = 10; // Gauss-Lobatto
                CommonEshelby.ngaussth[i] = 10;
            }
            CommonEshelby.ngaussph[12] = 16; // Gauss-Legendre
            CommonEshelby.ngaussth[12] = 16;

            // --- CASE 1 ---
            CommonEshelby.punti[1][1] = 4.71236594d-02;
            CommonEshelby.punti[2][1] = 0.241774723;
            CommonEshelby.punti[3][1] = 0.565131843;
            CommonEshelby.punti[4][1] = 0.968887568;
            CommonEshelby.punti[5][1] = 1.37937832;
            CommonEshelby.punti[6][1] = 1.76221442;
            CommonEshelby.punti[7][1] = 2.17270517;
            CommonEshelby.punti[8][1] = 2.57646084;
            CommonEshelby.punti[9][1] = 2.89981818;
            CommonEshelby.punti[10][1] = 3.09446883;
            CommonEshelby.pesi[1][1] = 0.120191820;
            CommonEshelby.pesi[2][1] = 0.264987558;
            CommonEshelby.pesi[3][1] = 0.373805553;
            CommonEshelby.pesi[4][1] = 0.420841277;
            CommonEshelby.pesi[5][1] = 0.390970200;
            CommonEshelby.pesi[6][1] = 0.390970260;
            CommonEshelby.pesi[7][1] = 0.420841366;
            CommonEshelby.pesi[8][1] = 0.373805553;
            CommonEshelby.pesi[9][1] = 0.264987499;
            CommonEshelby.pesi[10][1] = 0.120192111;

            // --- CASE 2 ---
            CommonEshelby.punti[1][2] = 1.57080423d-02;
            CommonEshelby.punti[2][2] = 0.144995824;
            CommonEshelby.punti[3][2] = 0.425559640;
            CommonEshelby.punti[4][2] = 0.829968274;
            CommonEshelby.punti[5][2] = 1.31460333;
            CommonEshelby.punti[6][2] = 1.82698941;
            CommonEshelby.punti[7][2] = 2.31162453;
            CommonEshelby.punti[8][2] = 2.71603298;
            CommonEshelby.punti[9][2] = 2.99659705;
            CommonEshelby.punti[10][2] = 3.12588477;
            CommonEshelby.pesi[1][2] = 5.41692823d-02;
            CommonEshelby.pesi[2][2] = 0.207461149;
            CommonEshelby.pesi[3][2] = 0.348739326;
            CommonEshelby.pesi[4][2] = 0.452716887;
            CommonEshelby.pesi[5][2] = 0.507709801;
            CommonEshelby.pesi[6][2] = 0.507709682;
            CommonEshelby.pesi[7][2] = 0.452716798;
            CommonEshelby.pesi[8][2] = 0.348738998;
            CommonEshelby.pesi[9][2] = 0.207461327;
            CommonEshelby.pesi[10][2] = 5.41692935d-02;

            // --- CASE 3 ---
            CommonEshelby.punti[1][3] = 3.76990959d-02;
            CommonEshelby.punti[2][3] = 0.198626831;
            CommonEshelby.punti[3][3] = 0.483041346;
            CommonEshelby.punti[4][3] = 0.871647120;
            CommonEshelby.punti[5][3] = 1.32964790;
            CommonEshelby.punti[6][3] = 1.81194484;
            CommonEshelby.punti[7][3] = 2.26994562;
            CommonEshelby.punti[8][3] = 2.65855122;
            CommonEshelby.punti[9][3] = 2.94296598;
            CommonEshelby.punti[10][3] = 3.10389376;
            CommonEshelby.pesi[1][3] = 9.68142375d-02;
            CommonEshelby.pesi[2][3] = 0.224478707;
            CommonEshelby.pesi[3][3] = 0.341134071;
            CommonEshelby.pesi[4][3] = 0.430180043;
            CommonEshelby.pesi[5][3] = 0.478189558;
            CommonEshelby.pesi[6][3] = 0.478189170;
            CommonEshelby.pesi[7][3] = 0.430180043;
            CommonEshelby.pesi[8][3] = 0.341134191;
            CommonEshelby.pesi[9][3] = 0.224478647;
            CommonEshelby.pesi[10][3] = 9.68143344d-02;

            // --- CASE 4 ---
            CommonEshelby.punti[1][4] = 3.45576368d-02;
            CommonEshelby.punti[2][4] = 0.187556863;
            CommonEshelby.punti[3][4] = 0.468425453;
            CommonEshelby.punti[4][4] = 0.859980166;
            CommonEshelby.punti[5][4] = 1.32527423;
            CommonEshelby.punti[6][4] = 1.81631863;
            CommonEshelby.punti[7][4] = 2.28161263;
            CommonEshelby.punti[8][4] = 2.67316723;
            CommonEshelby.punti[9][4] = 2.95403576;
            CommonEshelby.punti[10][4] = 3.10703516;
            CommonEshelby.pesi[1][4] = 8.95763785d-02;
            CommonEshelby.pesi[2][4] = 0.217725381;
            CommonEshelby.pesi[3][4] = 0.341026783;
            CommonEshelby.pesi[4][4] = 0.435772508;
            CommonEshelby.pesi[5][4] = 0.486694932;
            CommonEshelby.pesi[6][4] = 0.486695170;
            CommonEshelby.pesi[7][4] = 0.435772508;
            CommonEshelby.pesi[8][4] = 0.341026902;
            CommonEshelby.pesi[9][4] = 0.217725128;
            CommonEshelby.pesi[10][4] = 8.95764604d-02;

            // --- CASE 5 ---
            CommonEshelby.punti[1][5] = 3.14158052d-02;
            CommonEshelby.punti[2][5] = 0.177928671;
            CommonEshelby.punti[3][5] = 0.457155794;
            CommonEshelby.punti[4][5] = 0.851592362;
            CommonEshelby.punti[5][5] = 1.32222414;
            CommonEshelby.punti[6][5] = 1.81936860;
            CommonEshelby.punti[7][5] = 2.29000044;
            CommonEshelby.punti[8][5] = 2.68443704;
            CommonEshelby.punti[9][5] = 2.96366405;
            CommonEshelby.punti[10][5] = 3.11017680;
            CommonEshelby.pesi[1][5] = 8.26927349d-02;
            CommonEshelby.pesi[2][5] = 0.213228315;
            CommonEshelby.pesi[3][5] = 0.342008322;
            CommonEshelby.pesi[4][5] = 0.440196186;
            CommonEshelby.pesi[5][5] = 0.492670894;
            CommonEshelby.pesi[6][5] = 0.492670983;
            CommonEshelby.pesi[7][5] = 0.440195888;
            CommonEshelby.pesi[8][5] = 0.342008322;
            CommonEshelby.pesi[9][5] = 0.213227972;
            CommonEshelby.pesi[10][5] = 8.26930404d-02;

            // --- CASE 6 ---
            CommonEshelby.punti[1][6] = 2.98452154d-02;
            CommonEshelby.punti[2][6] = 0.173592165;
            CommonEshelby.punti[3][6] = 0.452448040;
            CommonEshelby.punti[4][6] = 0.848216832;
            CommonEshelby.punti[5][6] = 1.32101476;
            CommonEshelby.punti[6][6] = 1.82057810;
            CommonEshelby.punti[7][6] = 2.29337597;
            CommonEshelby.punti[8][6] = 2.68914461;
            CommonEshelby.punti[9][6] = 2.96800065;
            CommonEshelby.punti[10][6] = 3.11174774;
            CommonEshelby.pesi[1][6] = 7.93928578d-02;
            CommonEshelby.pesi[2][6] = 0.211627841;
            CommonEshelby.pesi[3][6] = 0.342669785;
            CommonEshelby.pesi[4][6] = 0.442057431;
            CommonEshelby.pesi[5][6] = 0.495048553;
            CommonEshelby.pesi[6][6] = 0.495048642;
            CommonEshelby.pesi[7][6] = 0.442057490;
            CommonEshelby.pesi[8][6] = 0.342670023;
            CommonEshelby.pesi[9][6] = 0.211627468;
            CommonEshelby.pesi[10][6] = 7.93929026d-02;

            // --- CASE 7 ---
            CommonEshelby.punti[1][7] = 2.67036632d-02;
            CommonEshelby.punti[2][7] = 0.165752888;
            CommonEshelby.punti[3][7] = 0.444431901;
            CommonEshelby.punti[4][7] = 0.842614472;
            CommonEshelby.punti[5][7] = 1.31902647;
            CommonEshelby.punti[6][7] = 1.82256627;
            CommonEshelby.punti[7][7] = 2.29897833;
            CommonEshelby.punti[8][7] = 2.69716072;
            CommonEshelby.punti[9][7] = 2.97583985;
            CommonEshelby.punti[10][7] = 3.11488938;
            CommonEshelby.pesi[1][7] = 7.30879456d-02;
            CommonEshelby.pesi[2][7] = 0.209402516;
            CommonEshelby.pesi[3][7] = 0.344104946;
            CommonEshelby.pesi[4][7] = 0.445234656;
            CommonEshelby.pesi[5][7] = 0.498966068;
            CommonEshelby.pesi[6][7] = 0.498966306;
            CommonEshelby.pesi[7][7] = 0.445234746;
            CommonEshelby.pesi[8][7] = 0.344104946;
            CommonEshelby.pesi[9][7] = 0.209402665;
            CommonEshelby.pesi[10][7] = 7.30878562d-02;

            // --- CASE 8 ---
            CommonEshelby.punti[1][8] = 2.67036632d-02;
            CommonEshelby.punti[2][8] = 0.165752888;
            CommonEshelby.punti[3][8] = 0.444431901;
            CommonEshelby.punti[4][8] = 0.842614472;
            CommonEshelby.punti[5][8] = 1.31902647;
            CommonEshelby.punti[6][8] = 1.82256627;
            CommonEshelby.punti[7][8] = 2.29897833;
            CommonEshelby.punti[8][8] = 2.69716072;
            CommonEshelby.punti[9][8] = 2.97583985;
            CommonEshelby.punti[10][8] = 3.11488938;
            CommonEshelby.pesi[1][8] = 7.30879456d-02;
            CommonEshelby.pesi[2][8] = 0.209402516;
            CommonEshelby.pesi[3][8] = 0.344104946;
            CommonEshelby.pesi[4][8] = 0.445234656;
            CommonEshelby.pesi[5][8] = 0.498966068;
            CommonEshelby.pesi[6][8] = 0.498966306;
            CommonEshelby.pesi[7][8] = 0.445234746;
            CommonEshelby.pesi[8][8] = 0.344104946;
            CommonEshelby.pesi[9][8] = 0.209402665;
            CommonEshelby.pesi[10][8] = 7.30878562d-02;

            // --- CASE 9 ---
            CommonEshelby.punti[1][9] = 2.43473575d-02;
            CommonEshelby.punti[2][9] = 0.160516247;
            CommonEshelby.punti[3][9] = 0.439386278;
            CommonEshelby.punti[4][9] = 0.839168847;
            CommonEshelby.punti[5][9] = 1.31781363;
            CommonEshelby.punti[6][9] = 1.82377899;
            CommonEshelby.punti[7][9] = 2.30242372;
            CommonEshelby.punti[8][9] = 2.70220637;
            CommonEshelby.punti[9][9] = 2.98107672;
            CommonEshelby.punti[10][9] = 3.11724544;
            CommonEshelby.pesi[1][9] = 6.86219111d-02;
            CommonEshelby.pesi[2][9] = 0.208388865;
            CommonEshelby.pesi[3][9] = 0.345189095;
            CommonEshelby.pesi[4][9] = 0.447236270;
            CommonEshelby.pesi[5][9] = 0.501360059;
            CommonEshelby.pesi[6][9] = 0.501359940;
            CommonEshelby.pesi[7][9] = 0.447236151;
            CommonEshelby.pesi[8][9] = 0.345189214;
            CommonEshelby.pesi[9][9] = 0.208388969;
            CommonEshelby.pesi[10][9] = 6.86219335d-02;

            // --- CASE 10 ---
            CommonEshelby.punti[1][10] = 2.19910536d-02;
            CommonEshelby.punti[2][10] = 0.155757755;
            CommonEshelby.punti[3][10] = 0.434985727;
            CommonEshelby.punti[4][10] = 0.836206555;
            CommonEshelby.punti[5][10] = 1.31677616;
            CommonEshelby.punti[6][10] = 1.82481658;
            CommonEshelby.punti[7][10] = 2.30538607;
            CommonEshelby.punti[8][10] = 2.70660710;
            CommonEshelby.punti[9][10] = 2.98583508;
            CommonEshelby.punti[10][10] = 3.11960149;
            CommonEshelby.pesi[1][10] = 6.43825606d-02;
            CommonEshelby.pesi[2][10] = 0.207786217;
            CommonEshelby.pesi[3][10] = 0.346235514;
            CommonEshelby.pesi[4][10] = 0.448981822;
            CommonEshelby.pesi[5][10] = 0.503410578;
            CommonEshelby.pesi[6][10] = 0.503410578;
            CommonEshelby.pesi[7][10] = 0.448981792;
            CommonEshelby.pesi[8][10] = 0.346235693;
            CommonEshelby.pesi[9][10] = 0.207785636;
            CommonEshelby.pesi[10][10] = 6.43827692d-02;

            // --- CASE 11 ---
            CommonEshelby.punti[1][11] = 2.04204638d-02;
            CommonEshelby.punti[2][11] = 0.152822554;
            CommonEshelby.punti[3][11] = 0.432348520;
            CommonEshelby.punti[4][11] = 0.834448099;
            CommonEshelby.punti[5][11] = 1.31616223;
            CommonEshelby.punti[6][11] = 1.82543063;
            CommonEshelby.punti[7][11] = 2.30714464;
            CommonEshelby.punti[8][11] = 2.70924401;
            CommonEshelby.punti[9][11] = 2.98877001;
            CommonEshelby.punti[10][11] = 3.12117243;
            CommonEshelby.pesi[1][11] = 6.16818815d-02;
            CommonEshelby.pesi[2][11] = 0.207559645;
            CommonEshelby.pesi[3][11] = 0.346902698;
            CommonEshelby.pesi[4][11] = 0.450027168;
            CommonEshelby.pesi[5][11] = 0.504624724;
            CommonEshelby.pesi[6][11] = 0.504624426;
            CommonEshelby.pesi[7][11] = 0.450027317;
            CommonEshelby.pesi[8][11] = 0.346902847;
            CommonEshelby.pesi[9][11] = 0.207559645;
            CommonEshelby.pesi[10][11] = 6.16819337d-02;

            // --- CASE 12: GAULEG ---
            // Call the stub
            IntegrationUtils.gauleg(0.0, CommonConstants.PI, CommonEshelby.puntigl,
                                    CommonEshelby.pesigl, CommonEshelby.ngaussph[12]);

            // --- Pre-calculate arrays that depend on integration points ---
            for (int icase = 1; icase <= 12; icase++) {
                
                if (icase == 12) {
                    for (int i = 1; i <= 16; i++) {
                        CommonEshelby.xph[i] = CommonEshelby.puntigl[i];
                        CommonEshelby.xth[i] = CommonEshelby.puntigl[i];
                        CommonEshelby.wph[i] = CommonEshelby.pesigl[i];
                        CommonEshelby.wth[i] = CommonEshelby.pesigl[i];
                    }
                } else {
                    for (int i = 1; i <= 10; i++) {
                        CommonEshelby.xph[i] = CommonEshelby.punti[i][icase];
                        CommonEshelby.xth[i] = CommonEshelby.punti[i][icase];
                        CommonEshelby.wph[i] = CommonEshelby.pesi[i][icase];
                        CommonEshelby.wth[i] = CommonEshelby.pesi[i][icase];
                    }
                }
                
                // Integration [0,pi][0,pi]
                for (int ith = 1; ith <= CommonEshelby.ngaussth[icase]; ith++) {
                    double sinth = Math.sin(CommonEshelby.xth[ith]);
                    double costh = Math.cos(CommonEshelby.xth[ith]);
                    double simbtet = CommonEshelby.wth[ith] * sinth / CommonConstants.PI2;

                    for (int iph = 1; iph <= CommonEshelby.ngaussph[icase]; iph++) {
                        int ny = iph + (ith - 1) * CommonEshelby.ngaussph[icase];
                        CommonEshelby.ww[icase][ny] = simbtet * CommonEshelby.wph[iph];
                        CommonEshelby.alpha[icase][1][ny] = sinth * Math.cos(CommonEshelby.xph[iph]);
                        CommonEshelby.alpha[icase][2][ny] = sinth * Math.sin(CommonEshelby.xph[iph]);
                        CommonEshelby.alpha[icase][3][ny] = costh;

                        for (int i = 1; i <= 3; i++) {
                            for (int j = 1; j <= 3; j++) {
                                CommonEshelby.aa2x[i][j] = CommonEshelby.alpha[icase][i][ny] * CommonEshelby.alpha[icase][j][ny];
                                CommonEshelby.aaww2x[i][j] = CommonEshelby.aa2x[i][j] * CommonEshelby.ww[icase][ny];
                            }
                        }

                        // call voigt(aa1x, aa2x, c2, c4, 2)
                        TensorUtils.voigt(CommonEshelby.aa1x, CommonEshelby.aa2x, CommonEshelby.c2, CommonEshelby.gamma4, 2);
                        // call voigt(aaww1x, aaww2x, c2, c4, 2)
                        TensorUtils.voigt(CommonEshelby.aaww1x, CommonEshelby.aaww2x, CommonEshelby.c2, CommonEshelby.gamma4, 2);

                        for (int i = 1; i <= 6; i++) {
                            CommonEshelby.aa1[icase][i][ny] = CommonEshelby.aa1x[i];
                            CommonEshelby.aaww1[icase][i][ny] = CommonEshelby.aaww1x[i];
                        }

                        // Array AWW (only for ICAUCHY=1)
                        for (int i = 1; i <= 3; i++) {
                            CommonEshelby.aww[icase][i][ny] = CommonEshelby.alpha[icase][i][ny] * CommonEshelby.ww[icase][ny];
                        }
                    } // iph
                } // ith
            } // icase
            
        } // ENDIF FOR IOPTION=0

        // *************************************************************
        // * CALCULATION OF ESHELBY TENSORS
        // *************************************************************
        if (ioption >= 1) {
            
            // Note: axis is 1-based [4]
            CommonEshelby.abc = axis[1] * axis[2] * axis[3];
            CommonEshelby.ratio1 = axis[2] / axis[3];
            CommonEshelby.ratio2 = axis[1] / axis[3];

            int icase;
            if (CommonEshelby.IGAUSSLEG == 1) {
                icase = 12;
            } else {
                CommonEshelby.dte[0] = 0.0;
                CommonEshelby.dte[1] = -0.7 * CommonEshelby.ratio1 + 7.0;
                CommonEshelby.dte[2] = -CommonEshelby.ratio1 + 17.0;
                CommonEshelby.dte[3] = -CommonEshelby.ratio1 + 23.0;
                CommonEshelby.dte[4] = -CommonEshelby.ratio1 + 26.0;
                CommonEshelby.dte[5] = -CommonEshelby.ratio1 + 29.3;
                CommonEshelby.dte[6] = -CommonEshelby.ratio1 + 32.0;
                CommonEshelby.dte[7] = -CommonEshelby.ratio1 + 34.85;
                CommonEshelby.dte[8] = -CommonEshelby.ratio1 + 37.0;
                CommonEshelby.dte[9] = -CommonEshelby.ratio1 + 41.9;
                CommonEshelby.dte[10] = -CommonEshelby.ratio1 + 44.5;
                icase = 11;
                for (int i = 1; i <= 10; i++) {
                    if (CommonEshelby.ratio2 >= CommonEshelby.dte[i - 1] && CommonEshelby.ratio2 < CommonEshelby.dte[i]) {
                        icase = i;
                        break;
                    }
                }
            }
            
            CommonEshelby.npoints = CommonEshelby.ngaussph[icase] * CommonEshelby.ngaussth[icase];

            CommonEshelby.pdil = 0.0;
            for (int j = 1; j <= 3; j++) {
                for (int i = 1; i <= 3; i++) {
                    CommonEshelby.p[i][j] = 0.0;
                }
            }
            for (int j = 1; j <= 6; j++) {
                for (int i = 1; i <= 6; i++) {
                    CommonEshelby.gamma2[i][j] = 0.0;
                }
            }

            // call voigt(aa1x, aa2x, c2, c4, 4)
            // This converts the 4D tensor c4 to the 6x6 matrix CommonEshelby.c2
            TensorUtils.voigt(CommonEshelby.aa1x, CommonEshelby.aa2x, CommonEshelby.c2, c4, 4);

            // --- Main integration loop ---
            for (int ny = 1; ny <= CommonEshelby.npoints; ny++) {
                
                // Get pre-calculated Voigt tensor A(i,j)
                for (int i = 1; i <= 6; i++) {
                    CommonEshelby.aa1x[i] = CommonEshelby.aa1[icase][i][ny];
                }

                // call esh_mult_voigt(c2, aa1x, a1)
                IntegrationUtils.esh_mult_voigt(CommonEshelby.c2, CommonEshelby.aa1x, CommonEshelby.a1);
                
                if (ioption == 1) {
                    // call esh_inv3_voigt(a1, a1inv)
                    IntegrationUtils.esh_inv3_voigt(CommonEshelby.a1, CommonEshelby.a1inv);
                    for (int i = 1; i <= 6; i++) {
                        CommonEshelby.x1[i] = CommonEshelby.a1inv[i];
                    }
                }
                
                double alpha1 = CommonEshelby.alpha[icase][1][ny];
                double alpha2 = CommonEshelby.alpha[icase][2][ny];
                double alpha3 = CommonEshelby.alpha[icase][3][ny];
                
                double ro3_den = (alpha1 * axis[1]) * (alpha1 * axis[1])
                               + (alpha2 * axis[2]) * (alpha2 * axis[2])
                               + (alpha3 * axis[3]) * (alpha3 * axis[3]);
                
                CommonEshelby.ro3 = Math.pow(ro3_den, 1.5);
                CommonEshelby.abcoro3 = CommonEshelby.abc / CommonEshelby.ro3;

                // Compute Eshelby integral Gamma(m,j,n,i)
                for (int i = 1; i <= 6; i++) {
                    for (int j = 1; j <= 6; j++) {
                        CommonEshelby.gamma2[i][j] += CommonEshelby.aaww1[icase][i][ny] * CommonEshelby.x1[j] * CommonEshelby.abcoro3;
                    }
                }
            } // end loop over ny (integration points)

            // Go back to 3x3x3x3 notation
            // call voigt(aa1x, aa2x, gamma2, gamma4, 3)
            TensorUtils.voigt(CommonEshelby.aa1x, CommonEshelby.aa2x, CommonEshelby.gamma2, CommonEshelby.gamma4, 3);
            
            // Compute symmetric (esim) and anti-symmetric (escr) Eshelby tensors
            for (int l = 1; l <= 3; l++) {
                for (int k = 1; k <= 3; k++) {
                    for (int m = 1; m <= 3; m++) {
                        for (int n = 1; n <= 3; n++) {
                            
                            double dumsim = 0.0;
                            double dumscr = 0.0;
                            
                            for (int j = 1; j <= 3; j++) {
                                for (int i = 1; i <= 3; i++) {
                                    dumsim += (CommonEshelby.gamma4[m][j][n][i] + CommonEshelby.gamma4[n][j][m][i]) * c4[i][j][k][l];
                                    dumscr += (CommonEshelby.gamma4[m][j][n][i] - CommonEshelby.gamma4[n][j][m][i]) * c4[i][j][k][l];
                                }
                            }
                            esim[n][m][k][l] = 0.5 * dumsim;
                            escr[n][m][k][l] = 0.5 * dumscr;
                        }
                    }
                }
            }
            
        } // endif for IOPTION.GE.1
    }
    
/**
     * Identifies active slip systems in each grain.
     * If stress is outside the yield surface, it modifies the critical stress.
     * Translation of Fortran SUBROUTINE g_actsys.
     *
     * @param nout_old The number of 'out' grains from the previous iteration.
     * @return The new number of 'out' grains for this iteration.
     */
    public static int gActsys(int nout_old) {
        
        final double slack = 0.98;
        int nout = 0;
        
        // Local work array, 1-based
        double[] rss = new double[CommonGlobals.nsys + 1];

        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            double delt = 0.0;
            int nout_flag = 0;
            CommonGlobals.nact[ng] = 0;

            for (int ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
                rss[ns1] = 0.0;
                for (int i = 1; i <= 6; i++) {
                    rss[ns1] += CommonGlobals.mcs[i][ns1][ng] * CommonGlobals.stcs[i][ng] * CommonGlobals.profac[i];
                }
                
                // Normalize RSS by the current critical stress
                rss[ns1] /= CommonGlobals.tau[ns1][ng];

                if (rss[ns1] >= slack) {
                    // System is active
                    CommonGlobals.nact[ng]++;
                    CommonGlobals.iact[CommonGlobals.nact[ng]][ng] = ns1;

                    if (rss[ns1] > 1.0) {
                        // System is outside the yield surface
                        nout_flag = 1;
                        CommonGlobals.tau[ns1][ng] = rss[ns1] * CommonGlobals.tau[ns1][ng]; // Update CRSS
                        delt += (rss[ns1] - 1.0) * (rss[ns1] - 1.0);
                        // iflag=1
                        CommonGlobals.ng_update[ng] = 1;
                    }
                }
            } // end loop over systems
            
            delt = Math.sqrt(delt);
            CommonGlobals.tau_update[1][ng] *= (delt + 1.0); // Note: This uses index 1 explicitly

            if (nout_flag == 1) {
                nout++;
            }
        } // end loop over grains

        if (nout > nout_old) {
            // Note: unit 12 is assumed to be System.out
            System.out.println();
            System.out.println("WARNING");
            System.out.println("STRESS IS OUT OF THE SCYS FOR " + nout + " GRAINS");
            nout_old = nout; // Update nout_old (though it's returned)
        }
        
        return nout;
    }

    /**
     * Calculates averages and deviations for stress, strain, and their rates.
     * Translation of Fortran SUBROUTINE g_average.
     */
    public static void gAverage() {
        
        // Local 1-based arrays
        double[] strav = new double[7];
        double[] strdev = new double[7];
        double[] etrdev = new double[7];
        double[] stdev = new double[7];
        double[] etdev = new double[7];
        
        double wgtPA = 0.0;
        double wgtCH = 0.0;

        double presss = (CommonGlobals.stss[1] + CommonGlobals.stss[2] + CommonGlobals.stss[3]) / 3.0;
        double presav = 0.0;
        double presdev = 0.0;

        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            double prescs = (CommonGlobals.stcs[1][ng] + CommonGlobals.stcs[2][ng] + CommonGlobals.stcs[3][ng]) / 3.0;
            presav += prescs * CommonGlobals.wgt[ng];
            presdev += prescs * prescs * CommonGlobals.wgt[ng];
        }
        presdev = Math.sqrt(Math.abs(presdev - presav * presav));

        for (int i = 1; i <= 6; i++) {
            strav[i] = 0.0;
            strdev[i] = 0.0;
            CommonGlobals.etrav[i] = 0.0;
            etrdev[i] = 0.0;
            CommonGlobals.stav[i] = 0.0;
            stdev[i] = 0.0;
            CommonGlobals.etav[i] = 0.0;
            etdev[i] = 0.0;
            CommonGlobals.stPGav[i] = 0.0;
            CommonGlobals.etPGav[i] = 0.0;
            CommonGlobals.stCHav[i] = 0.0;
            CommonGlobals.etCHav[i] = 0.0;

            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                strav[i] += CommonGlobals.strcs[i][ng] * CommonGlobals.wgt[ng];
                strdev[i] += CommonGlobals.strcs[i][ng] * CommonGlobals.strcs[i][ng] * CommonGlobals.wgt[ng];
                CommonGlobals.etrav[i] += CommonGlobals.etrcs[i][ng] * CommonGlobals.wgt[ng];
                etrdev[i] += CommonGlobals.etrcs[i][ng] * CommonGlobals.etrcs[i][ng] * CommonGlobals.wgt[ng];
                CommonGlobals.stav[i] += CommonGlobals.stcs[i][ng] * CommonGlobals.wgt[ng];
                stdev[i] += CommonGlobals.stcs[i][ng] * CommonGlobals.stcs[i][ng] * CommonGlobals.wgt[ng];
                CommonGlobals.etav[i] += CommonGlobals.etcs[i][ng] * CommonGlobals.wgt[ng];
                etdev[i] += CommonGlobals.etcs[i][ng] * CommonGlobals.etcs[i][ng] * CommonGlobals.wgt[ng];
            }
            for (int ng = 1; ng <= CommonGlobals.ngParent; ng++) {
                CommonGlobals.stPGav[i] += CommonGlobals.stcs[i][ng] * CommonGlobals.wgt[ng];
                CommonGlobals.etPGav[i] += CommonGlobals.etcs[i][ng] * CommonGlobals.wgt[ng];
            }
            for (int ng = CommonGlobals.ngParent + 1; ng <= CommonGlobals.ngrain; ng++) {
                CommonGlobals.stCHav[i] += CommonGlobals.stcs[i][ng] * CommonGlobals.wgt[ng];
                CommonGlobals.etCHav[i] += CommonGlobals.etcs[i][ng] * CommonGlobals.wgt[ng];
            }
        }
        
        wgtPA = 0.0;
        for (int ng = 1; ng <= CommonGlobals.ngParent; ng++) {
            wgtPA += CommonGlobals.wgt[ng];
        }
        if (wgtPA > 0) {
            for (int i = 1; i <= 6; i++) {
                CommonGlobals.stPGav[i] /= wgtPA;
                CommonGlobals.etPGav[i] /= wgtPA;
            }
        }
        
        wgtCH = 0.0;
        for (int ng = CommonGlobals.ngParent + 1; ng <= CommonGlobals.ngrain; ng++) {
            wgtCH += CommonGlobals.wgt[ng];
        }
        if (wgtCH > 0) {
            for (int i = 1; i <= 6; i++) {
                CommonGlobals.stCHav[i] /= wgtCH;
                CommonGlobals.etCHav[i] /= wgtCH;
            }
        }

        double strnorm = 0.0;
        double etrnorm = 0.0;
        double stnorm = 0.0;
        double etnorm = 0.0;
        for (int i = 1; i <= 6; i++) {
            strnorm += strav[i] * strav[i] * CommonGlobals.profac[i];
            etrnorm += CommonGlobals.etrav[i] * CommonGlobals.etrav[i] * CommonGlobals.profac[i];
            stnorm += CommonGlobals.stav[i] * CommonGlobals.stav[i] * CommonGlobals.profac[i];
            etnorm += CommonGlobals.etav[i] * CommonGlobals.etav[i] * CommonGlobals.profac[i];
        }

        // Avoid division by zero if norm is zero
        double strnorm_safe = (strnorm == 0) ? 1.0 : Math.sqrt(strnorm);
        double etrnorm_safe = (etrnorm == 0) ? 1.0 : Math.sqrt(etrnorm);
        double stnorm_safe = (stnorm == 0) ? 1.0 : Math.sqrt(stnorm);
        double etnorm_safe = (etnorm == 0) ? 1.0 : Math.sqrt(etnorm);

        presdev /= stnorm_safe;
        for (int i = 1; i <= 6; i++) {
            strdev[i] = Math.sqrt(Math.abs(strdev[i] - strav[i] * strav[i])) / strnorm_safe;
            etrdev[i] = Math.sqrt(Math.abs(etrdev[i] - CommonGlobals.etrav[i] * CommonGlobals.etrav[i])) / etrnorm_safe;
            stdev[i] = Math.sqrt(Math.abs(stdev[i] - CommonGlobals.stav[i] * CommonGlobals.stav[i])) / stnorm_safe;
            etdev[i] = Math.sqrt(Math.abs(etdev[i] - CommonGlobals.etav[i] * CommonGlobals.etav[i])) / etnorm_safe;
        }

        // --- Write to output files ---
        PrintWriter out11 = IOUtils.writer11;
        PrintWriter out15 = IOUtils.writer15;
        PrintWriter out16 = IOUtils.writer16;

        if (out11 != null) {
            out11.println();
            out11.println("Bound. Cond., Av. and Dev. STRESS RATE (normalized)");
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.strss[1], CommonGlobals.strss[2], CommonGlobals.strss[3], 
                         CommonGlobals.strss[4], CommonGlobals.strss[5], CommonGlobals.strss[6]);
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         strav[1], strav[2], strav[3], strav[4], strav[5], strav[6]);
            out11.printf(" %12.4f %12.4f %12.4f %12.4f %12.4f %12.4f%n", 
                         strdev[1], strdev[2], strdev[3], strdev[4], strdev[5], strdev[6]);
            
            out11.println("Bound. Cond., Av. and Dev. STRAIN RATE (normalized)");
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.etrss[1], CommonGlobals.etrss[2], CommonGlobals.etrss[3], 
                         CommonGlobals.etrss[4], CommonGlobals.etrss[5], CommonGlobals.etrss[6]);
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.etrav[1], CommonGlobals.etrav[2], CommonGlobals.etrav[3], 
                         CommonGlobals.etrav[4], CommonGlobals.etrav[5], CommonGlobals.etrav[6]);
            out11.printf(" %12.4f %12.4f %12.4f %12.4f %12.4f %12.4f%n", 
                         etrdev[1], etrdev[2], etrdev[3], etrdev[4], etrdev[5], etrdev[6]);
            
            out11.println();
            out11.println("Bound. Cond., Av. and Dev. STRESS (normalized)");
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.stss[1], CommonGlobals.stss[2], CommonGlobals.stss[3], 
                         CommonGlobals.stss[4], CommonGlobals.stss[5], CommonGlobals.stss[6]);
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.stav[1], CommonGlobals.stav[2], CommonGlobals.stav[3], 
                         CommonGlobals.stav[4], CommonGlobals.stav[5], CommonGlobals.stav[6]);
            out11.printf(" %12.4f %12.4f %12.4f %12.4f %12.4f %12.4f%n", 
                         stdev[1], stdev[2], stdev[3], stdev[4], stdev[5], stdev[6]);
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.stPGav[1], CommonGlobals.stPGav[2], CommonGlobals.stPGav[3], 
                         CommonGlobals.stPGav[4], CommonGlobals.stPGav[5], CommonGlobals.stPGav[6]);
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.stCHav[1], CommonGlobals.stCHav[2], CommonGlobals.stCHav[3], 
                         CommonGlobals.stCHav[4], CommonGlobals.stCHav[5], CommonGlobals.stCHav[6]);

            out11.println("Bound. Cond., Av. and Dev. STRAIN (normalized)");
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.etss[1], CommonGlobals.etss[2], CommonGlobals.etss[3], 
                         CommonGlobals.etss[4], CommonGlobals.etss[5], CommonGlobals.etss[6]);
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.etav[1], CommonGlobals.etav[2], CommonGlobals.etav[3], 
                         CommonGlobals.etav[4], CommonGlobals.etav[5], CommonGlobals.etav[6]);
            out11.printf(" %12.4f %12.4f %12.4f %12.4f %12.4f %12.4f%n", 
                         etdev[1], etdev[2], etdev[3], etdev[4], etdev[5], etdev[6]);
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.etPGav[1], CommonGlobals.etPGav[2], CommonGlobals.etPGav[3], 
                         CommonGlobals.etPGav[4], CommonGlobals.etPGav[5], CommonGlobals.etPGav[6]);
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         CommonGlobals.etCHav[1], CommonGlobals.etCHav[2], CommonGlobals.etCHav[3], 
                         CommonGlobals.etCHav[4], CommonGlobals.etCHav[5], CommonGlobals.etCHav[6]);
            
            out11.println("Bound. pressure, Av. Press. and Dev. (normalized)");
            out11.printf(" %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n", 
                         presss, presav, presdev, 0.0, 0.0, 0.0); // Pad with zeros
        }

        // Write to unit 15
        if (out15 != null) {
            out15.printf(" %11.3e %11.3e %11.3e %11.3e %11.3e %11.3e",
                         CommonGlobals.etrss[1], CommonGlobals.etrss[2], CommonGlobals.etrss[3],
                         CommonGlobals.etrss[4], CommonGlobals.etrss[5], CommonGlobals.etrss[6]);
            out15.printf(" %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f",
                         etrdev[1], etrdev[2], etrdev[3], etrdev[4], etrdev[5], etrdev[6]);
            out15.printf(" %11.3e %11.3e %11.3e %11.3e %11.3e %11.3e",
                         CommonGlobals.etss[1], CommonGlobals.etss[2], CommonGlobals.etss[3],
                         CommonGlobals.etss[4], CommonGlobals.etss[5], CommonGlobals.etss[6]);
            out15.printf(" %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f%n",
                         etdev[1], etdev[2], etdev[3], etdev[4], etdev[5], etdev[6]);
        }

        // Write to unit 16
        if (out16 != null) {
            out16.printf(" %11.3e %11.3e %11.3e %11.3e %11.3e %11.3e",
                         CommonGlobals.strss[1], CommonGlobals.strss[2], CommonGlobals.strss[3],
                         CommonGlobals.strss[4], CommonGlobals.strss[5], CommonGlobals.strss[6]);
            out16.printf(" %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f",
                         strdev[1], strdev[2], strdev[3], strdev[4], strdev[5], strdev[6]);
            out16.printf(" %11.3e %11.3e %11.3e %11.3e %11.3e %11.3e",
                         CommonGlobals.stss[1], CommonGlobals.stss[2], CommonGlobals.stss[3],
                         CommonGlobals.stss[4], CommonGlobals.stss[5], CommonGlobals.stss[6]);
            out16.printf(" %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f%n",
                         stdev[1], stdev[2], stdev[3], stdev[4], stdev[5], stdev[6]);
        }
    }
    
    /**
     * Calculates the single-crystal elasto-plastic incremental stiffness 'acs2'.
     * Translation of Fortran SUBROUTINE g_modulus.
     */
    public static void gModulus() {

        final double TOLER_DET = 1.0e-20;
        
        // Local work arrays. Sized to NSLS+1 for 1-based indexing.
        double[][] aux21 = new double[7][7];
        double[][] x = new double[CommonGlobals.NSLS + 1][CommonGlobals.NSLS + 1];
        double[][] y = new double[CommonGlobals.NSLS + 1][CommonGlobals.NSLS + 1];
        int[] indx = new int[CommonGlobals.NSLS + 1];
        
        int jran = 0; // Dummy seed for ran2 stub

        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            int igverify = 0;
            if (CommonGlobals.nact[ng] == 0) {
                // No active systems, use elastic stiffness
                for (int i = 1; i <= 6; i++) {
                    for (int j = 1; j <= 6; j++) {
                        CommonGlobals.acs2[i][j][ng] = CommonGlobals.ccs2[i][j][ng]
                                                     - CommonGlobals.stcs[i][ng] * CommonGlobals.i6[j];
                    }
                }
                igverify = 1;
                gState(ng);
            } else {
                // Grain has active systems, start iterative search
                while (igverify == 0 && CommonGlobals.nact[ng] > 0) {
                    
                    // Call hardening routine
                    if (CommonGlobals.kCL == 0) crssVoce(2, ng);
                    if (CommonGlobals.kCL == 2) crssDislocDens(2, ng);
                    
                    // Build the 'x' matrix
                    for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                        int n1 = CommonGlobals.iact[ns1][ng];
                        for (int ns2 = 1; ns2 <= CommonGlobals.nact[ng]; ns2++) {
                            int n2 = CommonGlobals.iact[ns2][ng];
                            x[ns1][ns2] = CommonGlobals.hd[n1][n2];
                            for (int i = 1; i <= 6; i++) {
                                for (int j = 1; j <= 6; j++) {
                                    x[ns1][ns2] += CommonGlobals.mcs[i][n1][ng] * CommonGlobals.mcs[j][n2][ng]
                                                 * CommonGlobals.ccs2[i][j][ng] * CommonGlobals.profac[i] * CommonGlobals.profac[j];
                                }
                            }
                        }
                    }

                    // Decompose 'x'
                    double d = TensorUtils.ludcmpc(x, CommonGlobals.nact[ng], CommonGlobals.NSLS, indx);
                    
                    // Calculate determinant
                    for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                        d *= x[ns1][ns1];
                    }

                    if (Math.abs(d) < TOLER_DET) {
                        // Singular matrix: remove a random active system
                        int idelsys = (int) (RandomUtils.ran2(CommonGlobals.jran) * CommonGlobals.nact[ng]) + 1;
                        if (idelsys < CommonGlobals.nact[ng]) { // Fortran: .lt.
                            for (int ns1 = idelsys; ns1 <= CommonGlobals.nact[ng] - 1; ns1++) {
                                CommonGlobals.iact[ns1][ng] = CommonGlobals.iact[ns1 + 1][ng];
                            }
                        }
                        CommonGlobals.nact[ng]--;
                        igverify = 0;
                        System.out.println(ng + " DET=0 --> look for other combination");
                    } else {
                        // Matrix is invertible, proceed
                        // Create identity matrix 'y'
                        for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                            for (int ns2 = 1; ns2 <= CommonGlobals.nact[ng]; ns2++) {
                                y[ns1][ns2] = (ns1 == ns2) ? 1.0 : 0.0;
                            }
                        }

                        // Invert 'x' by solving X*y_col = I_col for each column
                        // (Result is stored in 'y')
                        for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                            // We need to pass the column y(1,ns1)
                            // Java equivalent: create a temp column vector
                            double[] col = new double[CommonGlobals.NSLS + 1];
                            for(int row=1; row <= CommonGlobals.nact[ng]; row++) {
                                col[row] = y[row][ns1];
                            }
                            
                            TensorUtils.lubksbc(x, CommonGlobals.nact[ng], CommonGlobals.NSLS, indx, col);
                            
                            // Copy result back into 'y'
                            for(int row=1; row <= CommonGlobals.nact[ng]; row++) {
                                y[row][ns1] = col[row];
                            }
                        }
                        
                        // Calculate 'f' matrix
                        for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                            for (int j = 1; j <= 6; j++) {
                                CommonGlobals.f[j][ns1][ng] = 0.0;
                                for (int ns2 = 1; ns2 <= CommonGlobals.nact[ng]; ns2++) {
                                    int n2 = CommonGlobals.iact[ns2][ng];
                                    for (int i = 1; i <= 6; i++) {
                                        CommonGlobals.f[j][ns1][ng] += y[ns1][ns2] * CommonGlobals.mcs[i][n2][ng] * CommonGlobals.profac[i]
                                                                     * (CommonGlobals.ccs2[i][j][ng] - CommonGlobals.stcs[i][ng] * CommonGlobals.i6[j]);
                                    }
                                }
                            }
                        }

                        // Calculate aux21 = I - m*f
                        for (int i = 1; i <= 6; i++) {
                            for (int j = 1; j <= 6; j++) {
                                aux21[i][j] = CommonGlobals.id2[i][j]; // id2 is identity
                                for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                                    int n1 = CommonGlobals.iact[ns1][ng];
                                    aux21[i][j] -= CommonGlobals.mcs[i][n1][ng] * CommonGlobals.f[j][ns1][ng];
                                }
                            }
                        }

                        // Calculate final acs2 = ccs2 * aux21
                        for (int i = 1; i <= 6; i++) {
                            for (int j = 1; j <= 6; j++) {
                                CommonGlobals.acs2[i][j][ng] = 0.0;
                                for (int k = 1; k <= 6; k++) {
                                    CommonGlobals.acs2[i][j][ng] += CommonGlobals.ccs2[i][k][ng] * aux21[k][j] * CommonGlobals.profac[k];
                                }
                                CommonGlobals.acs2[i][j][ng] -= CommonGlobals.stcs[i][ng] * CommonGlobals.i6[j];
                            }
                        }
                        
                        gState(ng);
                        gVerify(ng, new IntHolder(igverify)); // igverify is set by gVerify
                    }
                } // end while

                if (CommonGlobals.nact[ng] == 0) {
                    // All active systems were removed, reset to elastic
                    for (int i = 1; i <= 6; i++) {
                        for (int j = 1; j <= 6; j++) {
                            CommonGlobals.acs2[i][j][ng] = CommonGlobals.ccs2[i][j][ng] 
                                                         - CommonGlobals.stcs[i][ng] * CommonGlobals.i6[j];
                        }
                    }
                    igverify = 1;
                    gState(ng);
                }
            } // end else
        } // end loop over grains
    }

    /**
     * Calculates statistics on Single Crystal Yield Surface (SCYS) updates.
     * Translation of Fortran SUBROUTINE G_SCYS_STAT.
     *
     * @param iproc The current process number.
     */
    public static void gScysStat(int iproc) {
        
        // ioption 1: Initialization
        if (iproc == 1) {
            ModGScysStat.wgt_up_int = new double[101]; // 0 to 100
            ModGScysStat.tau_update_max = new double[CommonGlobals.NGR + 1]; // 1-based
            ModGScysStat.tau_up_max = 0.0;
            ModGScysStat.ng_up_tot = 0;
            ModGScysStat.wgt_up_tot = 0.0;
        }

        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            ModGScysStat.ng_up_tot += CommonGlobals.ng_update[ng];
            ModGScysStat.wgt_up_tot += CommonGlobals.wgt[ng] * CommonGlobals.ng_update[ng];
            for (int n = 1; n <= CommonGlobals.nsys; n++) {
                if (CommonGlobals.tau_update[n][ng] > ModGScysStat.tau_up_max) {
                    ModGScysStat.tau_up_max = CommonGlobals.tau_update[n][ng];
                }
                if (CommonGlobals.tau_update[n][ng] > ModGScysStat.tau_update_max[ng]) {
                    ModGScysStat.tau_update_max[ng] = CommonGlobals.tau_update[n][ng];
                }
            }
        }

        int ntau_up_write = 0;
        int[] ng_up_int = new int[101]; // Local array for counts
        ng_up_int[0] = CommonGlobals.ngrain; // Start with all grains
        ModGScysStat.wgt_up_int[0] = 1.0;  // Start with 1.0 weight

        if (ModGScysStat.tau_up_max > 1.0) {
            ntau_up_write = 1;
            double tau_up_int = 0.01;
            
            // Initialize bins
            for (int nint = 0; nint <= 100; nint++) {
                ng_up_int[nint] = 0;
                ModGScysStat.wgt_up_int[nint] = 0.0;
            }

            // Bin the grains
            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                boolean binned = false;
                for (int nint = 1; nint <= 100; nint++) {
                    if (ModGScysStat.tau_update_max[ng] > 1.0 + (nint - 1) * tau_up_int &&
                        ModGScysStat.tau_update_max[ng] <= 1.0 + nint * tau_up_int) {
                        ng_up_int[nint]++;
                        ModGScysStat.wgt_up_int[nint] += CommonGlobals.wgt[ng];
                        binned = true;
                        break;
                    }
                }
                if (!binned) {
                    ng_up_int[0]++; // Count grains not updated (or over 100)
                    ModGScysStat.wgt_up_int[0] += CommonGlobals.wgt[ng];
                }
            }
            // Fix counts (Fortran logic was subtracting, Java logic is to add)
            ng_up_int[0] = CommonGlobals.ngrain;
            ModGScysStat.wgt_up_int[0] = 1.0;
             for (int nint = 1; nint <= 100; nint++) {
                 ng_up_int[0] -= ng_up_int[nint];
                 ModGScysStat.wgt_up_int[0] -= ModGScysStat.wgt_up_int[nint];
             }

        }
        
        // Write to unit 12 (System.out)
        System.out.println();
        System.out.println("TAU_UPDATE_MAX = " + ModGScysStat.tau_up_max);
        System.out.println("TOTAL NUMBER OF GRAINS UPDATED = " + ModGScysStat.ng_up_tot);
        System.out.println("VOLUME FRACTION UPDATED = " + ModGScysStat.wgt_up_tot);
        System.out.println("STATISTIC FOR NON-HARDENING UPDATE OF SCYS:");
        System.out.println("(DONE OVER MAXIMUM VALUE IN EACH GRAIN)");
        System.out.printf(" %5d grains that represent %6.2f%% volume fraction were not updated%n",
                          ng_up_int[0], ModGScysStat.wgt_up_int[0] * 100.0);
        
        if (ntau_up_write == 1) {
            System.out.println(" # GRAINS       WGT   INITIAL      FINAL");
            double tau_up_int = 0.01;
            for (int nint = 1; nint <= 100; nint++) {
                System.out.printf("   %6d%10.4f%10.4f%10.4f%n",
                                  ng_up_int[nint], ModGScysStat.wgt_up_int[nint],
                                  1.0 + (nint - 1) * tau_up_int, 1.0 + nint * tau_up_int);
            }
        }
        System.out.println();
        System.out.println("NOTE: Update stats include all previous processes");
    }

    /**
     * Evaluates the stress rate and strain rate in each grain.
     * Translation of Fortran SUBROUTINE G_STATE.
     *
     * @param ng The grain index to update.
     */
    public static void gState(int ng) {
        
        // aux21 = aef + acs2
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                if (CommonGlobals.ishape == 0 || CommonGlobals.ishape == 1) {
                    ModGState.aux21[i][j] = CommonGlobals.aef[i][j] + CommonGlobals.acs2[i][j][ng];
                } else { // ISHAPE >= 2
                    ModGState.aux21[i][j] = CommonGlobals.aefgr[i][j][ng] + CommonGlobals.acs2[i][j][ng];
                }
            }
        }

        // aux22 = inv(aux21)
        TensorUtils.invten(ModGState.aux21, ModGState.aux22);

        // aux11 = acs2 * alfacs * deltemp
        for (int i = 1; i <= 6; i++) {
            ModGState.aux11[i] = 0.0;
            for (int j = 1; j <= 6; j++) {
                ModGState.aux11[i] += CommonGlobals.acs2[i][j][ng] * CommonGlobals.alfacs[j][ng]
                                    * CommonGlobals.deltemp * CommonGlobals.profac[j];
            }
        }

        // etrcs = aux22 * (auxsample + aux11)
        for (int i = 1; i <= 6; i++) {
            CommonGlobals.etrcs[i][ng] = 0.0;
            for (int j = 1; j <= 6; j++) {
                CommonGlobals.etrcs[i][ng] += ModGState.aux22[i][j] * CommonGlobals.profac[j]
                                            * (CommonGlobals.auxsample[j] + ModGState.aux11[j]);
            }
        }

        // strcs = acs2 * (etrcs - alfacs * deltemp)
        for (int i = 1; i <= 6; i++) {
            CommonGlobals.strcs[i][ng] = 0.0;
            for (int j = 1; j <= 6; j++) {
                CommonGlobals.strcs[i][ng] += CommonGlobals.acs2[i][j][ng]
                                            * (CommonGlobals.etrcs[j][ng] - CommonGlobals.alfacs[j][ng] * CommonGlobals.deltemp)
                                            * CommonGlobals.profac[j];
            }
        }
    }

    /**
     * Verifies the active loading condition for the tentative set in g_modulus.
     * Translation of Fortran SUBROUTINE G_VERIFY.
     *
     * @param ng       The grain index.
     * @param igverify (Out) Wrapped integer. Set to 0 if verification fails, 1 if OK.
     */
    public static void gVerify(int ng, IntHolder igverify) {
        
        final double ERROR_LOAD = 0.001;
        int jran = 0; // Dummy seed for ran2 stub
        
        igverify.value = 1; // Assume OK
        if (CommonGlobals.nact[ng] == 0) {
            return;
        }

        for (int ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
            CommonGlobals.gamd[ns1][ng] = 0.0;
        }

        // Calculate shear rates (gamd)
        for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
            int n1 = CommonGlobals.iact[ns1][ng];
            for (int i = 1; i <= 6; i++) {
                CommonGlobals.gamd[n1][ng] += CommonGlobals.f[i][ns1][ng]
                                            * (CommonGlobals.etrcs[i][ng] - CommonGlobals.alfacs[i][ng] * CommonGlobals.deltemp)
                                            * CommonGlobals.profac[i];
            }
        }
        
        // Check for negative shear rates
        int ns1 = 1;
        while (ns1 <= CommonGlobals.nact[ng]) {
            int n1 = CommonGlobals.iact[ns1][ng];
            if (CommonGlobals.gamd[n1][ng] < 0.0) {
                igverify.value = 0; // Verification failed
                CommonGlobals.nact[ng]--;
                for (int ns2 = ns1; ns2 <= CommonGlobals.nact[ng]; ns2++) {
                    CommonGlobals.iact[ns2][ng] = CommonGlobals.iact[ns2 + 1][ng];
                }
                ns1 = CommonGlobals.nact[ng]; // End the loop
            }
            ns1++;
        }

        if (igverify.value != 0) {
            // All shear rates are positive, now check loading condition
            int neload = 0;
            
            // Get hardening rates
            if (CommonGlobals.kCL == 0) crssVoce(2, ng);
            if (CommonGlobals.kCL == 2) crssDislocDens(2, ng);

            // Calculate taud
            for (ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
                CommonGlobals.taud[ns1][ng] = 0.0;
                for (int ns2 = 1; ns2 <= CommonGlobals.nact[ng]; ns2++) {
                    int n2 = CommonGlobals.iact[ns2][ng];
                    CommonGlobals.taud[ns1][ng] += CommonGlobals.hd[ns1][n2] * CommonGlobals.gamd[n2][ng];
                }
            }
            
            // Check if stress rate (rssd) matches hardening rate (taud)
            for (ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                int n1 = CommonGlobals.iact[ns1][ng];
                double rssd = 0.0;
                for (int i = 1; i <= 6; i++) {
                    rssd += CommonGlobals.mcs[i][n1][ng] * CommonGlobals.strcs[i][ng] * CommonGlobals.profac[i];
                }
                
                double control_load = Math.abs((rssd - CommonGlobals.taud[n1][ng]) / CommonGlobals.taud[n1][ng]);
                
                if (control_load > ERROR_LOAD) {
                    neload++;
                }
            }

            if (neload != 0) {
                // Mismatch: remove a random system and try again
                int idelsys = (int) (RandomUtils.ran2(CommonGlobals.jran) * CommonGlobals.nact[ng]) + 1;
                CommonGlobals.nact[ng]--;
                if (idelsys > 0 && idelsys <= CommonGlobals.nact[ng]) { // Note: modified logic from Fortran
                    for (ns1 = idelsys; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                        CommonGlobals.iact[ns1][ng] = CommonGlobals.iact[ns1 + 1][ng];
                    }
                }
                igverify.value = 0; // Verification failed
            }
        }
    }
    
    /**
     * Calculates the effective 'm' interaction parameter for each grain.
     * <p>
     * This method projects the average and per-grain stiffness tensors
     * onto the current strain rate direction. It then calculates a relative
     * stiffness ratio (RC) for each grain and interpolates an 'm' value
     * (MEFFC) between 1.0 (for the softest grain) and xmmin (for the
     * stiffest grain).
     * <p>
     * Translation of Fortran SUBROUTINE M_EFFECTIVE.
     *
     * @param ng1   The starting grain index.
     * @param ng2   The ending grain index.
     * @param xmmin The minimum 'm' value (typically 0.0 or 0.1),
     * representing tangent stiffness.
     */
    public static void mEffective(int ng1, int ng2, double xmmin) {

        // --- Local work arrays (1-based) ---
        double[][] ass2p = new double[7][7];
        double[][] acs2p = new double[7][7];
        double[][] proj = new double[7][7];
        // RC(NGR) -> Local array sized by global NGR
        double[] rc = new double[CommonGlobals.NGR + 1];

        // --- 1. Calculate projector over strain-rate direction ---
        double etrssnorm = 0.0;
        for (int i = 1; i <= 6; i++) {
            etrssnorm += CommonGlobals.etrss[i] * CommonGlobals.etrss[i] * CommonGlobals.profac[i];
        }
        etrssnorm = Math.sqrt(etrssnorm);

        double etrssnormSq = etrssnorm * etrssnorm;
        // Avoid division by zero if strain rate is zero
        if (etrssnormSq == 0.0) etrssnormSq = 1.0; 

        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                // PROJ = (etrss . etrss) / |etrss|^2
                proj[i][j] = CommonGlobals.etrss[i] * CommonGlobals.etrss[j] / etrssnormSq;
            }
        }

        // --- 2. Calculate projection of overall stiffness (ASS2P = PROJ * ASS2) ---
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                ass2p[i][j] = 0.0;
                for (int k = 1; k <= 6; k++) {
                    ass2p[i][j] += proj[i][k] * CommonGlobals.ass2[k][j] * CommonGlobals.profac[k];
                }
            }
        }

        // --- 3. Calculate norm of the projected overall stiffness ---
        double ass2pnorm = 0.0;
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                ass2pnorm += ass2p[i][j] * ass2p[i][j] * CommonGlobals.profac[i] * CommonGlobals.profac[j];
            }
        }
        ass2pnorm = Math.sqrt(ass2pnorm);

        // --- 4. Calculate projection for each grain and the ratio RC ---
        for (int kkk = ng1; kkk <= ng2; kkk++) {
            
            // ACS2P = PROJ * ACS2(grain kkk)
            for (int i = 1; i <= 6; i++) {
                for (int j = 1; j <= 6; j++) {
                    acs2p[i][j] = 0.0;
                    for (int k = 1; k <= 6; k++) {
                        acs2p[i][j] += proj[i][k] * CommonGlobals.acs2[k][j][kkk] * CommonGlobals.profac[k];
                    }
                }
            }

            // Norm of projected grain stiffness
            double acs2pnorm = 0.0;
            for (int i = 1; i <= 6; i++) {
                for (int j = 1; j <= 6; j++) {
                    acs2pnorm += acs2p[i][j] * acs2p[i][j] * CommonGlobals.profac[i] * CommonGlobals.profac[j];
                }
            }
            acs2pnorm = Math.sqrt(acs2pnorm);

            // Calculate ratio RC
            if (ass2pnorm == 0.0) {
                rc[kkk] = 1.0; // Avoid division by zero
            } else {
                rc[kkk] = acs2pnorm / ass2pnorm;
            }
        }

        // --- 5. Make statistic of relative compliance over all grains ---
        double rcave = 0.0;
        double rcdev = 0.0;
        double rcmin = rc[ng1]; // Initialize with first grain
        double rcmax = rc[ng1];

        for (int kkk = ng1; kkk <= ng2; kkk++) {
            rcave += rc[kkk] * CommonGlobals.wgt[kkk];
            rcdev += rc[kkk] * rc[kkk] * CommonGlobals.wgt[kkk];
            if (rc[kkk] < rcmin) rcmin = rc[kkk];
            if (rc[kkk] > rcmax) rcmax = rc[kkk];
        }
        rcdev = Math.sqrt(rcdev - rcave * rcave);
        
        // (Superfluous) calculation
        double rctop = rcave + rcdev;
        double rcbot = rcave - rcdev;
        if (rctop > rcmax) rctop = rcmax;
        if (rcbot < rcmin) rcbot = rcmin;
        
        double volfrac = 0.0;
        for (int kkk = ng1; kkk <= ng2; kkk++) {
            if (rc[kkk] >= rcbot && rc[kkk] <= rctop) {
                volfrac += CommonGlobals.wgt[kkk];
            }
        }

        // --- 6. Linear interpolation for MEFFC ---
        double avmeff = 0.0;
        double rcRange = rcmax - rcmin;
        if (rcRange == 0.0) rcRange = 1.0; // Avoid division by zero

        for (int kkk = ng1; kkk <= ng2; kkk++) {
            double xinterp = (rc[kkk] - rcmin) / rcRange;
            
            // MEFFC = 1.0 for min stiffness (rcmin)
            // MEFFC = xmmin for max stiffness (rcmax)
            CommonGlobals.meffc[kkk] = 1.0 + (xmmin - 1.0) * xinterp;
            
            avmeff += CommonGlobals.meffc[kkk] * CommonGlobals.wgt[kkk];
        }

        // Write to unit 12 (System.out)
        System.out.println("avmeff, rcmin, rcave, rcmax, rcdev,volfrac");
        System.out.printf("%7.3f%7.3f%7.3f%7.3f%7.3f%7.3f%n",
                          avmeff, rcmin, rcave, rcmax, rcdev, volfrac);
    }
    
    /**
     * Evaluates plastic activity, shear distribution, and twinning fractions.
     * Translation of Fortran SUBROUTINE plasticity.
     *
     * @param iproc   The current process number.
     * @param istep   The current step number.
     * @param step    The step increment.
     * @param temp    The current temperature.
     * @param ioption Control flag (0=Init accumulators, 1=Calc step, 2=Calc summary).
     */
    public static void plasticity(int iproc, int istep, double step, double temp, int ioption) {

        double xref = 0.0;
        
        // ioption 0: Initialize accumulators
        if (ioption == 0) {
            if (CommonGlobals.ndiff > 0) {
                for (int nd = 1; nd <= CommonGlobals.ndiff; nd++) {
                  CommonPlastic.shear_dif_acum[nd] = 0.0;
                  CommonPlastic.shear_dif_acum_ch[nd] = 0.0;
                  CommonPlastic.shear_dif_acum_pa[nd] = 0.0;
                }
            }
            for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
              CommonPlastic.shear_mod_acum[mo] = 0.0;
              CommonPlastic.shear_mod_acum_ch[mo] = 0.0;
              CommonPlastic.shear_mod_acum_pa[mo] = 0.0;
                CommonGlobals.vfrac_mod_acum[mo] = 0.0;
            }
        }

        // ioption 1: Calculate and write step data
        if (ioption == 1) {
            System.out.println(); // write(12,*)
            int ngtotact = 0;
            CommonGlobals.actav = 0.0;
            for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
              CommonPlastic.shear_mod[mo] = 0.0;
            }

            /* // --- Start commented-out section (jact, actwgt) ---
            int[] jact = new int[11]; // 0-10
            double[] actwgt = new double[11];
            for (int i=0; i<=10; i++) {
                jact[i]=0;
                actwgt[i]=0.0;
            }
            int jminact = 25;
            int jmaxact = 0;
            */ // --- End commented-out section ---

            // --- First the total average ---
            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                /* // --- Start commented-out section (jact, actwgt) ---
                if (CommonGlobals.nact[ng] > jmaxact) jmaxact = CommonGlobals.nact[ng];
                if (CommonGlobals.nact[ng] < jminact) jminact = CommonGlobals.nact[ng];
                jact[CommonGlobals.nact[ng]]++;
                actwgt[CommonGlobals.nact[ng]] += CommonGlobals.wgt[ng];
                */ // --- End commented-out section ---

                if (CommonGlobals.nact[ng] != 0) {
                    ngtotact++;
                    CommonGlobals.actav += CommonGlobals.nact[ng] * CommonGlobals.wgt[ng];
                    int nst = 0;
                    for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                        for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                            nst++;
                          CommonPlastic.shear_mod[mo] += CommonGlobals.gamd[nst][ng] * CommonGlobals.wgt[ng];
                          CommonPlastic.shear_mod_acum[mo] += CommonGlobals.gamd[nst][ng] * CommonGlobals.wgt[ng];
                        }
                    }
                }
            }
            
            /* // --- Start commented-out section (write 21) ---
            if (istep == 1 && iproc == 1) {
                // WRITE(21,*) ... (Header for James)
            }
            if (istep >= 1) {
                // WRITE(21, '(I6,2E12.4,F9.5,2I7,11I5,11F7.3)') ... (Data for James)
            }
            */ // --- End commented-out section ---
            
            // --- Then the parent grains ---
            int ngtotact_pa = 0;
            double actav_pa = 0.0;
            for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
              CommonPlastic.shear_mod_pa[mo] = 0.0;
            }
            double twp = 0.0;
            for (int ng = 1; ng <= CommonGlobals.ngParent; ng++) {
                twp += CommonGlobals.wgt[ng];
            }
            // Avoid division by zero if twp == 0
            if (twp == 0.0) twp = 1.0; 
            
            for (int ng = 1; ng <= CommonGlobals.ngParent; ng++) {
              CommonPlastic.WP[ng] = CommonGlobals.wgt[ng] / twp;
            }
            for (int ng = 1; ng <= CommonGlobals.ngParent; ng++) {
                if (CommonGlobals.nact[ng] != 0) {
                    ngtotact_pa++;
                    actav_pa += CommonGlobals.nact[ng] * CommonPlastic.WP[ng];
                    int nst = 0;
                    for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                        for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                            nst++;
                          CommonPlastic.shear_mod_pa[mo] += CommonGlobals.gamd[nst][ng] * CommonPlastic.WP[ng];
                          CommonPlastic.shear_mod_acum_pa[mo] += CommonGlobals.gamd[nst][ng] * CommonPlastic.WP[ng];
                        }
                    }
                }
            }
            
            // --- Then the child grains ---
            int ngtotact_ch = 0;
            double actav_ch = 0.0;
            for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
              CommonPlastic.shear_mod_ch[mo] = 0.0;
            }
            double twc = 0.0;
            for (int ng = CommonGlobals.ngParent + 1; ng <= CommonGlobals.ngrain; ng++) {
                twc += CommonGlobals.wgt[ng];
            }
            // Avoid division by zero if twc == 0
            if (twc == 0.0) twc = 1.0; 

            for (int ng = CommonGlobals.ngParent + 1; ng <= CommonGlobals.ngrain; ng++) {
              CommonPlastic.WC[ng] = CommonGlobals.wgt[ng] / twc;
            }
            for (int ng = CommonGlobals.ngParent + 1; ng <= CommonGlobals.ngrain; ng++) {
                if (CommonGlobals.nact[ng] != 0) {
                    ngtotact_ch++;
                    actav_ch += CommonGlobals.nact[ng] * CommonPlastic.WC[ng];
                    int nst = 0;
                    for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                        for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                            nst++;
                          CommonPlastic.shear_mod_ch[mo] += CommonGlobals.gamd[nst][ng] * CommonPlastic.WC[ng];
                          CommonPlastic.shear_mod_acum_ch[mo] += CommonGlobals.gamd[nst][ng] * CommonGlobals.wgt[ng];
                        }
                    }
                }
            }
            
            // --- Calculate total shears ---
            double shear_tot = 0.0;
            double shear_tot_ch = 0.0;
            double shear_tot_pa = 0.0;
            for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                shear_tot += CommonPlastic.shear_mod[mo];
                shear_tot_ch += CommonPlastic.shear_mod_ch[mo];
                shear_tot_pa += CommonPlastic.shear_mod_pa[mo];
            }

            // --- Write results to "EPSC?.OUT" ---
            if (CommonGlobals.icvx == 0) xref = temp;
            if (CommonGlobals.icvx >= 1 && CommonGlobals.icvx <= 6) xref = CommonGlobals.etss[CommonGlobals.icvx] - CommonGlobals.etssref[CommonGlobals.icvx];
            if (CommonGlobals.icvx >= 7) xref = CommonGlobals.stss[CommonGlobals.icvx - 6];

            // Calculate normalized shear arrays
            if (shear_tot != 0.0) {
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) CommonPlastic.aux[mo] = CommonPlastic.shear_mod[mo] / shear_tot;
            } else {
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) CommonPlastic.aux[mo] = CommonPlastic.shear_mod[mo];
            }
            if (shear_tot_pa != 0.0) {
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) CommonPlastic.aux_pa[mo] = CommonPlastic.shear_mod_pa[mo] / shear_tot_pa;
            } else {
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) CommonPlastic.aux_pa[mo] = CommonPlastic.shear_mod_pa[mo];
            }
            if (shear_tot_ch != 0.0) {
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) CommonPlastic.aux_ch[mo] = CommonPlastic.shear_mod_ch[mo] / shear_tot_ch;
            } else {
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) CommonPlastic.aux_ch[mo] = CommonPlastic.shear_mod_ch[mo];
            }

            // Write to unit 17
            PrintWriter out17 = IOUtils.writer17;
            if (out17 != null) {
                out17.printf("%15.5e", xref);
                for (int i = 1; i <= 3; i++) out17.printf("%15.5e", (CommonGlobals.etss[i] - CommonGlobals.etssref[i]));
                for (int i = 1; i <= 3; i++) out17.printf("%15.5e", CommonGlobals.stss[i]);
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) out17.printf("%15.5e", CommonPlastic.aux[mo]);
                out17.printf("%15.5e", CommonGlobals.actav);
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) out17.printf("%15.5e", CommonPlastic.aux_pa[mo]);
                out17.printf("%15.5e", actav_pa);
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) out17.printf("%15.5e", CommonPlastic.aux_ch[mo]);
                out17.printf("%15.5e", actav_ch);
                out17.printf("%15.5e", CommonGlobals.TVF);
                out17.printf("%15.5e", CommonGlobals.CTVF);
                out17.printf("%15.5e", (double)CommonGlobals.ngrain);
                out17.printf("%15.5e", (double)CommonGlobals.MaxTwins);
                out17.println();
            }

            /* // --- Start commented-out section (diffraction sets) ---
            if (CommonGlobals.ndiff > 0) {
                for (int nd = 1; nd <= CommonGlobals.ndiff; nd++) {
                    double dummy = 0.0;
                    for (int ng_idx = 1; ng_idx <= CommonGlobals.ngrset[nd]; ng_idx++) {
                        int ngset = CommonGlobals.igrset[nd][ng_idx];
                        int nst = 0;
                        for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                            for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                                nst++;
                                dummy += Math.abs(CommonGlobals.gamd[nst][ngset]);
                            }
                        }
                    }
                    if (CommonGlobals.ngrset[nd] > 0) { // Avoid division by zero
                        shear_dif_acum[nd] += dummy / CommonGlobals.ngrset[nd];
                    }
                }
                // WRITE(20, ...)
            }
            */ // --- End commented-out section ---

        } // end ioption == 1

        // ioption 2: Write summary of accumulated data
        if (ioption == 2) {
            System.out.println(); // write(12,*)
            double shear_tot_acum = 0.0;
            double vfrac_tot_acum = 0.0;
            for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                shear_tot_acum += CommonPlastic.shear_mod_acum[mo];
                vfrac_tot_acum += CommonGlobals.vfrac_mod_acum[mo];
            }
            
            if (shear_tot_acum == 0.0) {
                System.out.println("NO PLASTIC ACTIVITY");
            } else {
                System.out.println("Percent respect to total shear:");
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                    System.out.printf("Mode #:%3d  Activity:%8.4f%%%n",
                                      mo, (CommonPlastic.shear_mod_acum[mo] / shear_tot_acum) * 100.0);
                }
            }
            
            if (vfrac_tot_acum == 0.0) {
                System.out.println("NO TWIN ACTIVITY");
            } else {
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                    if (CommonGlobals.itw[mo] == 1) {
                        System.out.printf("Twin mode:%3d  Volume fraction:%10.6f%n",
                                          mo, CommonGlobals.vfrac_mod_acum[mo]);
                        System.out.printf("                     Percent:%8.4f%n",
                                          (CommonGlobals.vfrac_mod_acum[mo] / vfrac_tot_acum) * 100.0);
                    }
                }
            }
            
        } // end ioption == 2
    }
    
/**
     * Calculates the relative difference (Frobenius norm) between two matrices.
     * The difference is normalized by the norm of the average of the two matrices.
     *
     * This method replaces the 'tmismatch' stub.
     * Translation of Fortran FUNCTION tmismatch.
     *
     * @param v1    First 6x6 matrix (1-based, [7][7]).
     * @param v2    Second 6x6 matrix (1-based, [7][7]).
     * @param nrows Number of rows (e.g., 6).
     * @param ncols Number of columns (e.g., 6).
     * @return The relative mismatch (error) as a double.
     */
    public static double tmismatch(double[][] v1, double[][] v2, int nrows, int ncols) {
        
        double v_dif_sq_sum = 0.0;
        double v_ave_sq_sum = 0.0;

        // The Fortran code passes 6x6 matrices as 1D arrays of 36 elements
        // and calculates the Frobenius norm of the difference and the average.
        // We can do this by iterating over the 2D arrays.
        for (int i = 1; i <= nrows; i++) {
            for (int j = 1; j <= ncols; j++) {
                
                double dif = v1[i][j] - v2[i][j];
                double ave = 0.5 * (v1[i][j] + v2[i][j]);

                v_dif_sq_sum += dif * dif;
                v_ave_sq_sum += ave * ave;
            }
        }

        double norm_dif = Math.sqrt(v_dif_sq_sum);
        double norm_ave = Math.sqrt(v_ave_sq_sum);

        if (norm_ave == 0.0) {
            return (norm_dif == 0.0) ? 0.0 : 1.0; // Avoid division by zero
        }

        return norm_dif / norm_ave;
    }
    
    /**
     * Evaluates the macroscopic (sample) stress and strain rates.
     * It solves the linear system defined by the boundary conditions.
     * It also calculates 'auxsample' for use in g_state.
     * Translation of Fortran SUBROUTINE s_state.
     */
    public static void sState() {
        
        // Local 1-based arrays
        double[] aux11 = new double[7];
        double[][] aux21 = new double[7][7];
        int[] indx = new int[7];

        // --- Build the linear system Ax = b ---
        // A = aux21, b = aux11
        for (int i = 1; i <= 6; i++) {
            // Build RHS vector 'b' (aux11)
            aux11[i] = -1.0 * CommonGlobals.istbc[i] * CommonGlobals.strbc[i];
            for (int j = 1; j <= 6; j++) {
                aux11[i] += CommonGlobals.ass2[i][j]
                          * (CommonGlobals.ietbc[j] * CommonGlobals.etrbc[j] - CommonGlobals.alfass[j] * CommonGlobals.deltemp)
                          * CommonGlobals.profac[j];
                
                // Build LHS matrix 'A' (aux21)
                // (i/j)*(j/i) is a Fortran trick for (i == j ? 1 : 0)
                aux21[i][j] = CommonGlobals.ietbc[j] * ((i == j) ? 1.0 : 0.0)
                            - CommonGlobals.istbc[j] * CommonGlobals.ass2[i][j] * CommonGlobals.profac[j];
            }
        }

        // --- Solve the system Ax = b for x ---
        // Decompose A
        double d = TensorUtils.ludcmpc(aux21, 6, 6, indx);
        
        // Solve for x (solution stored back in aux11)
        TensorUtils.lubksbc(aux21, 6, 6, indx, aux11);

        // --- Reconstruct full stress/strain rate tensors ---
        for (int i = 1; i <= 6; i++) {
            CommonGlobals.etrss[i] = CommonGlobals.ietbc[i] * CommonGlobals.etrbc[i] 
                                   + CommonGlobals.istbc[i] * aux11[i];
                                   
            CommonGlobals.strss[i] = CommonGlobals.istbc[i] * CommonGlobals.strbc[i] 
                                   + CommonGlobals.ietbc[i] * aux11[i];
        }

        // --- Calculate the 'auxsample' vector for g_state ---
        // auxsample = (aef + ass2) * etrss - (ass2 * alfass * deltemp)
        for (int i = 1; i <= 6; i++) {
            aux11[i] = 0.0; // Re-use aux11 for first term
            CommonGlobals.auxsample[i] = 0.0; // This is the second term
            
            for (int j = 1; j <= 6; j++) {
                aux11[i] += (CommonGlobals.aef[i][j] + CommonGlobals.ass2[i][j]) 
                          * CommonGlobals.etrss[j] * CommonGlobals.profac[j];
                          
                CommonGlobals.auxsample[i] += CommonGlobals.ass2[i][j] 
                                            * CommonGlobals.alfass[j] * CommonGlobals.deltemp * CommonGlobals.profac[j];
            }
            // Final calculation for auxsample
            CommonGlobals.auxsample[i] = aux11[i] - CommonGlobals.auxsample[i];
        }
    }
    
    /**
     * Calculates the orientation matrix 'A' for a twinned crystal.
     * It applies a 180-degree rotation around the Burgers vector (twin shear direction).
     * The input matrix 'A' is modified in place.
     * Translation of Fortran SUBROUTINE TWINOR.
     *
     * @param bur The Burgers vector (twin shear direction) in crystal axes (1-based, size [4]).
     * @param A   (In/Out) The grain orientation matrix (1-based, [4][4]).
     */
    public static void twinor(double[] bur, double[][] A) {
        
        // Local 1-based arrays
        double[][] hpi = new double[4][4];
        double[][] htw = new double[4][4];
        double[][] aux = new double[4][4];
        double[][] atw = new double[4][4];

        // DATA HPI/-1.d0,0.d0,0.d0,0.d0,-1.d0,0.d0,0.d0,0.d0,1.d0/
        // This is a 180-degree rotation around Z-axis (HPI)
        hpi[1][1] = -1.0; hpi[1][2] = 0.0;  hpi[1][3] = 0.0;
        hpi[2][1] = 0.0;  hpi[2][2] = -1.0; hpi[2][3] = 0.0;
        hpi[3][1] = 0.0;  hpi[3][2] = 0.0;  hpi[3][3] = 1.0;

        // Calculate Euler angles to align Z-axis with BUR vector
        double ang1 = Math.atan2(bur[2], bur[1]) + CommonConstants.PI_2;
        double ang2 = Math.sqrt(bur[1] * bur[1] + bur[2] * bur[2]);
        ang2 = Math.atan2(ang2, bur[3]);
        
        // CALL EULER (2,ANG1*RAD_TO_DEG,ANG2*RAD_TO_DEG,0.d0,AUX)
        // This gives rotation matrix 'aux'
        TensorUtils.eulerFromAngles(ang1 * CommonConstants.RAD_TO_DEG,
                                    ang2 * CommonConstants.RAD_TO_DEG, 0.0, aux);

        // Calculate the twin rotation matrix HTW
        // HTW = AUX_transpose * HPI * AUX
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                htw[i][j] = 0.0;
                for (int k1 = 1; k1 <= 3; k1++) {
                    for (int k2 = 1; k2 <= 3; k2++) {
                        htw[i][j] += aux[k1][i] * hpi[k1][k2] * aux[k2][j];
                    }
                }
            }
        }
        
        // Apply the twin rotation to the grain orientation
        // ATW = A * HTW
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                atw[i][j] = 0.0;
                for (int k = 1; k <= 3; k++) {
                    atw[i][j] += A[i][k] * htw[k][j];
                }
            }
        }
        
        // Update the grain orientation matrix 'A'
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                A[i][j] = atw[i][j];
            }
        }
    }
    
    /**
     * Handles dislocation pileup and recoverable strain logic.
     * Translation of Fortran SUBROUTINE pileup.
     *
     * @param iopt Control flag (0=Init, 1=Calculate, 2=Write stats).
     */
    public static void pileup(int iopt) {

        // iopt 0: Initialization
        if (iopt == 0) {
            ModPileup.gamrec = new double[CommonGlobals.NGR + 1];
            ModPileup.gamrev = new double[CommonGlobals.NGR + 1];
            ModPileup.rsspr = new double[CommonGlobals.NGR + 1];
            ModPileup.iprsys = new int[CommonGlobals.NGR + 1];
            ModPileup.irec = new int[CommonGlobals.NGR + 1];
            
            ModPileup.ipoly = 3;
            ModPileup.grecmax = 0.01;
            ModPileup.frac_bs = 0.9;
            ModPileup.tau_bs = ModPileup.frac_bs * CommonGlobals.tau0[1]; // Uses tau0 of 1st system

            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                for (int i = 1; i <= 6; i++) {
                    CommonGlobals.alfacs[i][ng] = 0.0;
                }
            }
        }

        // iopt 1: Calculation
        if (iopt == 1) {
            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                if (ModPileup.irec[ng] == 0) {
                    // Identify primary slip system
                    if (ModPileup.iprsys[ng] == 0 && CommonGlobals.nact[ng] > 0) {
                        ModPileup.gamdmax = 0.0;
                        for (int is = 1; is <= CommonGlobals.nact[ng]; is++) {
                            int iact_is_ng = CommonGlobals.iact[is][ng];
                            if (CommonGlobals.gamd[iact_is_ng][ng] > ModPileup.gamdmax) {
                                ModPileup.gamdmax = CommonGlobals.gamd[iact_is_ng][ng];
                                ModPileup.iprsys[ng] = iact_is_ng;
                            }
                        }
                    }
                    
                    // Assign recoverable shear strain increments
                    if (ModPileup.iprsys[ng] != 0) {
                        if (CommonGlobals.gamd[ModPileup.iprsys[ng]][ng] > 0.0) {
                            if (ModPileup.gamrec[ng] < ModPileup.grecmax) {
                                ModPileup.gamrec[ng] += CommonGlobals.gamd[ModPileup.iprsys[ng]][ng];
                                if (ModPileup.gamrec[ng] > ModPileup.grecmax) {
                                    ModPileup.gamrec[ng] = ModPileup.grecmax;
                                }
                            }
                        }
                    }
                } // end if (irec(ng) == 0)

                CommonGlobals.deltemp = 1.0; // This seems... wrong. But it's in the code.
                for (int i = 1; i <= 6; i++) {
                    CommonGlobals.alfacs[i][ng] = 0.0;
                }

                if (ModPileup.iprsys[ng] != 0) {
                    // Calculate RSS on primary system
                    ModPileup.rss = 0.0;
                    for (int i = 1; i <= 6; i++) {
                        ModPileup.rss += CommonGlobals.mcs[i][ModPileup.iprsys[ng]][ng] 
                                       * CommonGlobals.stcs[i][ng] * CommonGlobals.profac[i];
                    }

                    // Check if recovery should start
                    if (ModPileup.irec[ng] == 0 && Math.abs(ModPileup.rss) <= ModPileup.tau_bs) {
                        ModPileup.irec[ng] = 1;
                        ModPileup.rsspr[ng] = ModPileup.rss;
                    }

                    // Check if recovery should end
                    if (ModPileup.irec[ng] == 1 && ModPileup.rss < -ModPileup.tau_bs) {
                        ModPileup.irec[ng] = 0;
                        ModPileup.iprsys[ng] = 0;
                        ModPileup.gamrec[ng] = 0.0;
                        ModPileup.gamrev[ng] = 0.0;
                    }

                    // If in recovery
                    if (ModPileup.irec[ng] == 1) {
                        ModPileup.drssprx = ModPileup.rss - ModPileup.rsspr[ng];
                        if (ModPileup.drssprx > 0.0) {
                            ModPileup.dgamrecx = 0.0;
                            // System.out.println("WARNING - GRAIN: " + ng + " - PRIMARY SYSTEM IS NOT UNLOADING");
                        } else {
                            // Call STUB
                            double grec_new = grec_law(ModPileup.ipoly, ModPileup.tau_bs, ModPileup.tau_bs,
                                                       ModPileup.rss, ModPileup.gamrec[ng]);
                            double grec_old = grec_law(ModPileup.ipoly, ModPileup.tau_bs, ModPileup.tau_bs,
                                                       ModPileup.rsspr[ng], ModPileup.gamrec[ng]);
                            
                            ModPileup.dgamrecx = -(grec_new - grec_old);
                            ModPileup.gamrev[ng] -= ModPileup.dgamrecx;
                            ModPileup.rsspr[ng] = ModPileup.rss;
                        }
                        
                        for (int i = 1; i <= 6; i++) {
                            CommonGlobals.alfacs[i][ng] = CommonGlobals.mcs[i][ModPileup.iprsys[ng]][ng] * ModPileup.dgamrecx;
                        }

                        if (ModPileup.gamrev[ng] > ModPileup.gamrec[ng]) {
                            System.out.println("WARNING: gamrev>gamrec in GRAIN: " + ng);
                            System.out.println("gamrec:" + ModPileup.gamrec[ng] + " gamrev:" + ModPileup.gamrev[ng]);
                            System.out.println("tau_bs-abs(rss):" + (ModPileup.tau_bs - Math.abs(ModPileup.rss)));
                        }
                    }
                } // end if (iprsys != 0)
            } // end loop over grains
        } // end iopt == 1

        // iopt 2: Write stats to unit 20
        if (iopt == 2) {
            if (IOUtils.writer20 != null) {
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    IOUtils.writer20.printf("%4d%4d%4d%10.6f%10.6f%n",
                                            ng, ModPileup.irec[ng], ModPileup.iprsys[ng],
                                            ModPileup.gamrec[ng], ModPileup.gamrev[ng]);
                }
            }
        }
    }
    
    
/**
     * Calculates the recoverable shear strain based on a polynomial function.
     * Full translation of Fortran FUNCTION grec_law.
     *
     * @param n     Polynomial order.
     * @param tau1  Parameter tau1.
     * @param tau2  Parameter tau2.
     * @param tau   Current stress.
     * @param gmax  Maximum recoverable shear.
     * @return The calculated recoverable strain.
     */
    public static double grec_law(int n, double tau1, double tau2,
                                  double tau, double gmax) {
        
        // grec_law=gmax*((tau1-tau)/(tau1+tau2))**real(n)
        return gmax * Math.pow((tau1 - tau) / (tau1 + tau2), (double) n);
    }
    
    
/**
     * Calculates the initial stress and strain state in a new child twin.
     * It uses continuity conditions across the twin-parent boundary.
     * Full translation of Fortran SUBROUTINE InitializeChild.
     *
     * @param iChild The index of the new child grain.
     */
    public static void initializeChild(int iChild) {
        
        // Local 1-based arrays
        double[] etplParent = new double[7];
        double[] aux6 = new double[7];
        double[] backStress = new double[7];
        double[] twinStrain = new double[7];

        int imo = CommonGlobals.iParentMode[iChild];
        int igr = CommonGlobals.iParentGrain[iChild];
        int ist = CommonGlobals.iParentSystem[iChild];

        if (CommonGlobals.TwinFrac[imo] > 0.0) {
            double gamma0 = CommonGlobals.TwinFrac[imo] * CommonGlobals.stw[imo];
            for (int i = 1; i <= 6; i++) {
                twinStrain[i] = CommonGlobals.mcs[i][ist][igr] * gamma0;
            }
            for (int i = 1; i <= 6; i++) {
                backStress[i] = 0.0;
                for (int j = 1; j <= 6; j++) {
                    backStress[i] -= CommonGlobals.ccs2[i][j][iChild] * twinStrain[j] * CommonGlobals.profac[j];
                }
                // Merkel, 05/2010: stress in child is stress in parent + backstress
                CommonGlobals.stcs[i][iChild] = CommonGlobals.stcs[i][igr] + backStress[i];
            }
        } else {
            // Call STUB
            calcChildStress(iChild);
        }

        // --- Calculate the plastic strain of the parent ---
        int iParent = CommonGlobals.iParentGrain[iChild];
        if (CommonGlobals.kSM == 1.0) {
            for (int i = 1; i <= 6; i++) {
                etplParent[i] = CommonGlobals.etcs[i][iParent] - CommonGlobals.etelcs[i][iParent];
            }
        } else {
            // Calculate elastic strain using Hooke's Law
            for (int i = 1; i <= 6; i++) {
                aux6[i] = 0.0;
                for (int j = 1; j <= 6; j++) {
                    aux6[i] += CommonGlobals.scs2[i][j][iParent] * CommonGlobals.stcs[j][iParent] * CommonGlobals.profac[j];
                }
            }
            // Plastic strain = Total - Elastic
            for (int i = 1; i <= 6; i++) {
                etplParent[i] = CommonGlobals.etcs[i][iParent] - aux6[i];
            }
        }

        // --- Calculate the strain in the twin (elastic + parent plastic) ---
        // 1. Child's elastic strain
        for (int i = 1; i <= 6; i++) {
            aux6[i] = 0.0;
            for (int j = 1; j <= 6; j++) {
                aux6[i] += CommonGlobals.scs2[i][j][iChild] * CommonGlobals.stcs[j][iChild] * CommonGlobals.profac[j];
            }
        }
        // 2. Child's total strain = Child's elastic + Parent's plastic
        for (int i = 1; i <= 6; i++) {
            CommonGlobals.etcs[i][iChild] = aux6[i] + etplParent[i];
        }

        // If large strains, store elastic and hydrostatic strains
        if (CommonGlobals.kSM == 1.0) {
            for (int i = 1; i <= 6; i++) {
                CommonGlobals.etelcs[i][iChild] = aux6[i];
                CommonGlobals.etelhycs[i][iChild] = CommonGlobals.etelhycs[i][iParent];
            }
        }

        // --- Initialize the state of the new grain ---
        CommonGlobals.wgt[iChild] = 0.0;
        CommonGlobals.wgtd[iChild] = 0.0;
        CommonGlobals.gamtot[iChild] = 0.0;

        int ns1 = 0;
        for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
            for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                ns1++;
                // Initialize internal state
                if (CommonGlobals.kCL != 2) {
                    CommonGlobals.tau[ns1][iChild] = CommonGlobals.tau0[ns1];
                } else {
                    CommonGlobals.tau[ns1][iChild] = CommonGlobals.tau[ns1][iParent];
                    CommonGlobals.rho_for[ns1][iChild] = CommonGlobals.rho_for[ns1][iParent];
                    CommonGlobals.rho_deb[iChild] = CommonGlobals.rho_deb[iParent];
                }
            }
        }

        // Ensure yield surface of child is not exceeded by initial stress
        for (ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
            double rss = 0.0;
            for (int i = 1; i <= 6; i++) {
                rss += CommonGlobals.mcs[i][ns1][iChild] * CommonGlobals.stcs[i][iChild] * CommonGlobals.profac[i];
            }
            if (rss > CommonGlobals.tau[ns1][iChild]) {
                CommonGlobals.tau[ns1][iChild] = rss;
            }
        }
        
        // Zero out rates and references
        for (int i = 1; i <= 6; i++) {
            CommonGlobals.etrcs[i][iChild] = 0.0;
            CommonGlobals.strcs[i][iChild] = 0.0;
            CommonGlobals.stcsref[i][iChild] = 0.0;
        }

        // For large strains, reference stress must be set
        if (CommonGlobals.kSM == 1.0) {
            for (int i = 1; i <= 6; i++) {
                CommonGlobals.stcsref[i][iChild] = CommonGlobals.stcs[i][iChild];
            }
        }

        for (ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
            CommonGlobals.taud[ns1][iChild] = 0.0;
            CommonGlobals.gamd[ns1][iChild] = 0.0;
        }
    }
    
/**
     * Generates a new "child" grain by twinning a parent grain.
     * This increments the global grain count and calculates its orientation.
     * <p>
     * <b>NOTE:</b> This routine does NOT dynamically resize the grain arrays.
     * It will throw an exception if the new grain count exceeds the
     * allocated size (NGR).
     * <p>
     * Translation of Fortran SUBROUTINE GenerateChildGrain.
     *
     * @param igr The index of the parent grain.
     * @param ist The index of the active twin system.
     * @param imo The index of the active twin mode.
     */
    public static void generateChildGrain(int igr, int ist, int imo) {
        
        // Local 1-based arrays
        double[] bur = new double[4];
        double[][] aux33 = new double[4][4];
        double[][] aux = new double[4][4];

        CommonGlobals.ngrain++;
        if (CommonGlobals.ngrain > CommonGlobals.NGR) {
            String msg = "ERROR: Number of grains greater than code dimension !!! DIMENSION in code = " + CommonGlobals.NGR;
            System.err.println(msg);
            throw new RuntimeException(msg);
        }

        CommonGlobals.iChildGrain[ist][igr] = -CommonGlobals.ngrain;
        CommonGlobals.iParentGrain[CommonGlobals.ngrain] = igr;
        CommonGlobals.iParentSystem[CommonGlobals.ngrain] = ist;
        CommonGlobals.iParentMode[CommonGlobals.ngrain] = imo;
        CommonGlobals.iTwinLevel[CommonGlobals.ngrain] = CommonGlobals.iTwinLevel[igr] + 1;

        for (int i = 1; i <= CommonGlobals.NSLS; i++) {
            CommonGlobals.iChildGrain[i][CommonGlobals.ngrain] = 0;
        }

        // Get Burgers vector and R_transpose of parent
        for (int i = 1; i <= 3; i++) {
            bur[i] = CommonGlobals.bcc[i][ist];
            for (int j = 1; j <= 3; j++) {
                aux33[i][j] = CommonGlobals.r[j][i][igr]; // R_transpose
            }
        }

        // Calculate twinned orientation matrix (modifies aux33 in place)
        twinor(bur, aux33); // aux33 now holds R_twin_transpose

        // Store new orientation matrix R_twin
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                CommonGlobals.r[i][j][CommonGlobals.ngrain] = aux33[j][i];
                aux[i][j] = aux33[j][i]; // Copy for euler call
            }
        }

        // Back-calculate Euler angles for the new grain
        // call euler(1,phi(ngrain),the(ngrain),ome(ngrain),aux)
        TensorUtils.EulerAngles angles = TensorUtils.matrixToEuler(aux);
        CommonGlobals.phi[CommonGlobals.ngrain] = angles.phi;
        CommonGlobals.the[CommonGlobals.ngrain] = angles.theta;
        CommonGlobals.ome[CommonGlobals.ngrain] = angles.omega;

        // Merkel 05/2010: update elastic constants using pressure in parent
        if (CommonGlobals.kSM == 1.0) {
            double pressure = -(CommonGlobals.stcs[1][igr] + CommonGlobals.stcs[2][igr] + CommonGlobals.stcs[3][igr]) / 3.0;
            pressureCij(pressure);
        }

        // Rotate properties to sample axes for the new grain
        crToSa(CommonGlobals.ngrain, CommonGlobals.ngrain, 0);
        crToSa(CommonGlobals.ngrain, CommonGlobals.ngrain, 1);

        // Set elasto-plastic stiffness to elastic stiffness
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                CommonGlobals.acs2[i][j][CommonGlobals.ngrain] = CommonGlobals.ccs2[i][j][CommonGlobals.ngrain];
            }
        }
        
        initializeChild(CommonGlobals.ngrain);
    }

    /**
     * Manages the grain twinning process.
     * Calculates volume fractions transferred from parent to child grains.
     * Translation of Fortran SUBROUTINE TWINNING_BJCL.
     *
     * @param step The step increment.
     */
    public static void twinningBjcl(double step) {

        // Local 1-based arrays
        double[][] vchange = new double[CommonGlobals.NMOD + 1][CommonGlobals.NSLS + 1];
        int[] iactivtwmod = new int[CommonGlobals.NMOD + 1];

        // Initialize grain weight deltas
        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            CommonGlobals.wgtd[ng] = 0.0;
        }

        // Use a fixed loop boundary, as ngrain changes inside the loop
        int currentMaxGrains = CommonGlobals.ngrain; 
        for (int ng = 1; ng <= currentMaxGrains; ng++) {
            int iTwins = 0;
            int nst = 0;
            
            if (CommonGlobals.iTwinLevel[ng] < CommonGlobals.iMaxTwinLevel) {
                // --- Check all systems in this grain ---
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                    iactivtwmod[mo] = 0;
                    if (CommonGlobals.itw[mo] == 0) { // Slip mode
                        nst += CommonGlobals.nsm[mo];
                        // vchange is implicitly 0
                    } else { // Twin mode
                        for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                            nst++;
                            if (CommonGlobals.gamd[nst][ng] > 0) {
                                // This twin system is active
                                iTwins++;
                                if (CommonGlobals.iChildGrain[nst][ng] == 0) {
                                    // First time twinning: generate child grain
                                    generateChildGrain(ng, nst, mo);
                                    vchange[mo][isys] = CommonGlobals.wgt[ng] * CommonGlobals.TwinFrac[mo];
                                } else {
                                    // Subsequent twinning: incremental volume change
                                    vchange[mo][isys] = CommonGlobals.wgt[ng] * CommonGlobals.gamd[nst][ng] * step / CommonGlobals.stw[mo];
                                }
                                iactivtwmod[mo] = 1;
                            } else {
                                vchange[mo][isys] = 0.0;
                            }
                        } // isys
                    } // itw(mo)
                } // mo

                // --- Sum and scale the volume change ---
                double sumvchange = 0.0;
                for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                    if (CommonGlobals.itw[mo] == 1) {
                        for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                            sumvchange += vchange[mo][isys];
                        }
                    }
                }

                if (CommonGlobals.wgt[ng] > 0) {
                    // Scale volume change if it exceeds parent's weight
                    if (sumvchange > CommonGlobals.wgt[ng]) {
                        double scale = CommonGlobals.wgt[ng] / sumvchange;
                        for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                            if (CommonGlobals.itw[mo] == 1) {
                                for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                                    vchange[mo][isys] *= scale;
                                }
                            }
                        }
                        sumvchange = CommonGlobals.wgt[ng];
                    }

                    // Update total twin volume fraction counters
                    if (ng <= CommonGlobals.ngParent) {
                        CommonGlobals.TVF += sumvchange;
                    } else {
                        CommonGlobals.CTVF += sumvchange;
                    }

                    // Apply weight change: parent loses, child gains
                    CommonGlobals.wgtd[ng] -= sumvchange;
                    nst = 0;
                    for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                        if (CommonGlobals.itw[mo] == 0) {
                            nst += CommonGlobals.nsm[mo];
                        } else { // Twin mode
                            for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                                nst++;
                                if (vchange[mo][isys] > 0) {
                                    int nch = Math.abs(CommonGlobals.iChildGrain[nst][ng]);
                                    if (nch != 0) {
                                        CommonGlobals.wgtd[nch] += vchange[mo][isys];
                                    } else {
                                        System.out.println("WARNING: Weight change to a non active twin system");
                                    }
                                }
                            }
                        }
                    }
                } else { // wgt(ng) == 0
                    // Parent grain is exhausted, harden its twin systems
                    nst = 0;
                    for (int mo = 1; mo <= CommonGlobals.nmodes; mo++) {
                        if (CommonGlobals.itw[mo] == 0) {
                            nst += CommonGlobals.nsm[mo];
                        } else {
                            for (int isys = 1; isys <= CommonGlobals.nsm[mo]; isys++) {
                                nst++;
                                if (iactivtwmod[mo] == 1) {
                                    CommonGlobals.tau[nst][ng] = 1.0e10; // Harden
                                }
                            }
                        }
                    }
                } // end if wgt(ng) > 0
            } // end if iTwinLevel

            if (iTwins > CommonGlobals.MaxTwins) {
                CommonGlobals.MaxTwins = iTwins;
            }
        } // end loop over grains
    }
    
    /**
     * Writes the current texture (Euler angles and weights) to a new file.
     * The file is named 'texNNNN.out' where NNNN is the nFile number.
     * Translation of Fortran SUBROUTINE WriteTexFile.
     *
     * @param nFile The file number (e.g., 1 -> "tex0001.out").
     * @param iProc The current process number.
     * @param iStep The current step number.
     */
    public static void writeTexFile(int nFile, int iProc, int iStep) {
        
        // Local 1-based work array
        double[][] rg = new double[4][4];
        
        if (nFile > 9999) {
            System.err.println("Maximum number of texture files is 9999");
            throw new RuntimeException("Maximum number of texture files exceeded.");
        }
        
        // Format filename as "texNNNN.out"
        String filename = String.format(Locale.US, "tex%04d.out", nFile);
        
        try (PrintWriter out22 = new PrintWriter(new FileWriter(filename))) {
            
            out22.println("relative axes and angles of the rep. ellipsoid");
            out22.printf(Locale.US, "%9.3f%9.3f%9.3f%9.3f%9.3f%9.3f%n",
                         CommonGlobals.axisph[0][1], CommonGlobals.axisph[0][2], CommonGlobals.axisph[0][3],
                         CommonGlobals.eulerph[1], CommonGlobals.eulerph[2], CommonGlobals.eulerph[3]);
            
            out22.printf(Locale.US, " EPSC Step= %5d%5d%5d%13.4e%13.4e%13.4e%13.4e%13.4e%13.4e%n",
                         CommonGlobals.iTotStep, iProc, iStep,
                         CommonGlobals.etss[1], CommonGlobals.etss[2], CommonGlobals.etss[3],
                         CommonGlobals.stss[1], CommonGlobals.stss[2], CommonGlobals.stss[3]);
            
            out22.printf(Locale.US, " B %10d 0%n", CommonGlobals.ngrain);

            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                // Copy rotation matrix to local rg
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                        rg[i][j] = CommonGlobals.r[i][j][ng];
                    }
                }
                
                // CALL EULER(1,PHI(NG),THE(NG),OME(NG),RG)
                // This call calculates angles and updates the global arrays
                TensorUtils.EulerAngles angles = TensorUtils.matrixToEuler(rg);
                CommonGlobals.phi[ng] = angles.phi;
                CommonGlobals.the[ng] = angles.theta;
                CommonGlobals.ome[ng] = angles.omega;

                // write(22,30) phi(ng),the(ng),ome(ng),wgt(ng)
                out22.printf(Locale.US, "%12.2f%12.2f%12.2f%12.8f%n",
                             CommonGlobals.phi[ng], CommonGlobals.the[ng],
                             CommonGlobals.ome[ng], CommonGlobals.wgt[ng]);
            }
            
        } catch (IOException e) {
            System.err.println("Error writing texture file: " + filename);
            e.printStackTrace();
        }
    }
    
    /**
     * Calculates the stress and elastic strain state in a new child twin
     * using continuity conditions across the twin/parent boundary.
     * Full translation of Fortran SUBROUTINE CalcChildStress.
     *
     * @param iChild The index of the new child grain.
     */
    public static void calcChildStress(int iChild) {
        
        // --- Local 1-based arrays ---
        double[] stParent = new double[7];
        double[] etelParent = new double[7];
        double[] stChild = new double[7];
        double[] etelChild = new double[7];
        double[][] aChild = new double[7][7];
        double[][] aux66 = new double[7][7];
        double[] aux6 = new double[7];
        double[][] rTW2SA = new double[4][4]; // Twin-to-Sample rotation
        double[][] rSA2TW = new double[4][4]; // Sample-to-Twin rotation
        double[][] aux33 = new double[4][4];
        double[][][][] aux3333 = new double[4][4][4][4];
        int[] indx = new int[7];

        // Boundary conditions: 1 = component is known, 0 = unknown
        // Sig33, Sig13, Sig23 are continuous (known)
        final int[] istTW = {0, 0, 0, 1, 1, 1, 0}; // 1-based: {0,0,1,1,1,0}
        // Eps11, Eps22, Eps12 are continuous (known)
        final int[] ietTW = {0, 1, 1, 0, 0, 0, 1}; // 1-based: {1,1,0,0,0,1}

        int iParent = CommonGlobals.iParentGrain[iChild];
        int isys = CommonGlobals.iParentSystem[iChild];

        // --- 1. Define rTW2SA (Twin-to-Sample rotation matrix) ---
        // Axis 1 = Burgers vector (normalized)
        double vlen = 0.0;
        for (int i = 1; i <= 3; i++) {
            vlen += CommonGlobals.bcs[i][isys][iParent] * CommonGlobals.bcs[i][isys][iParent];
        }
        vlen = Math.sqrt(vlen);
        for (int i = 1; i <= 3; i++) {
            rTW2SA[i][1] = CommonGlobals.bcs[i][isys][iParent] / vlen;
        }

        // Axis 3 = Twin plane normal (normalized)
        vlen = 0.0;
        for (int i = 1; i <= 3; i++) {
            vlen += CommonGlobals.ncs[i][isys][iParent] * CommonGlobals.ncs[i][isys][iParent];
        }
        vlen = Math.sqrt(vlen);
        for (int i = 1; i <= 3; i++) {
            rTW2SA[i][3] = CommonGlobals.ncs[i][isys][iParent] / vlen;
        }

        // Axis 2 = Cross product (3 x 1)
        rTW2SA[1][2] = rTW2SA[2][3] * rTW2SA[3][1] - rTW2SA[3][3] * rTW2SA[2][1];
        rTW2SA[2][2] = rTW2SA[3][3] * rTW2SA[1][1] - rTW2SA[1][3] * rTW2SA[3][1];
        rTW2SA[3][2] = rTW2SA[1][3] * rTW2SA[2][1] - rTW2SA[2][3] * rTW2SA[1][1];

        // Define rSA2TW (Sample-to-Twin) as the transpose
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                rSA2TW[i][j] = rTW2SA[j][i];
            }
        }

        // --- 2. Rotate Parent stress to twin coordinates (stParent) ---
        for (int i = 1; i <= 6; i++) {
            aux6[i] = CommonGlobals.stcs[i][iParent];
        }
        TensorUtils.voigt(aux6, aux33, aux66, aux3333, 1); // 6-vec -> 3x3 tensor
        
        // 2nd-order tensor rotation: T_twin = R_sa2tw * T_sample * R_sa2tw^T
        for (int ij = 1; ij <= 6; ij++) {
            int i = CommonGlobals.ijv[ij][1];
            int j = CommonGlobals.ijv[ij][2];
            stParent[ij] = 0.0;
            for (int i1 = 1; i1 <= 3; i1++) {
                for (int j1 = 1; j1 <= 3; j1++) {
                    stParent[ij] += rSA2TW[i][i1] * rSA2TW[j][j1] * aux33[i1][j1];
                }
            }
        }

        // --- 3. Rotate Parent elastic strain to twin coordinates (etelParent) ---
        if (CommonGlobals.kSM == 1.0) {
            for (int i = 1; i <= 6; i++) {
                aux6[i] = CommonGlobals.etelcs[i][iParent];
            }
        } else {
            // Calculate elastic strain from stress: e_el = S_parent : s_parent
            for (int i = 1; i <= 6; i++) {
                aux6[i] = 0.0;
                for (int j = 1; j <= 6; j++) {
                    aux6[i] += CommonGlobals.scs2[i][j][iParent] * CommonGlobals.stcs[j][iParent] * CommonGlobals.profac[j];
                }
            }
        }
        TensorUtils.voigt(aux6, aux33, aux66, aux3333, 1); // 6-vec -> 3x3 tensor
        
        // 2nd-order tensor rotation
        for (int ij = 1; ij <= 6; ij++) {
            int i = CommonGlobals.ijv[ij][1];
            int j = CommonGlobals.ijv[ij][2];
            etelParent[ij] = 0.0;
            for (int i1 = 1; i1 <= 3; i1++) {
                for (int j1 = 1; j1 <= 3; j1++) {
                    etelParent[ij] += rSA2TW[i][i1] * rSA2TW[j][j1] * aux33[i1][j1];
                }
            }
        }

        // --- 4. Rotate Child stiffness to twin coordinates (aChild) ---
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                aux66[i][j] = CommonGlobals.acs2[i][j][iChild];
            }
        }
        TensorUtils.voigt(aux6, aux33, aux66, aux3333, 3); // 6x6 -> 4th-order tensor
        
        // 4th-order tensor rotation: C_twin = R_sa2tw * R_sa2tw * C_sample * R_sa2tw^T * R_sa2tw^T
        for (int ij = 1; ij <= 6; ij++) {
            int i = CommonGlobals.ijv[ij][1];
            int j = CommonGlobals.ijv[ij][2];
            for (int kl = 1; kl <= 6; kl++) {
                int k = CommonGlobals.ijv[kl][1];
                int l = CommonGlobals.ijv[kl][2];
                aChild[ij][kl] = 0.0;
                for (int i1 = 1; i1 <= 3; i1++) {
                    for (int j1 = 1; j1 <= 3; j1++) {
                        for (int k1 = 1; k1 <= 3; k1++) {
                            for (int l1 = 1; l1 <= 3; l1++) {
                                aChild[ij][kl] += rSA2TW[i][i1] * rSA2TW[j][j1] * rSA2TW[k][k1] * rSA2TW[l][l1]
                                                * aux3333[i1][j1][k1][l1];
                            }
                        }
                    }
                }
            }
        }

        // --- 5. Solve the linear system for unknown child components ---
        // Build system Ax = b  (where A=aux66, b=aux6, x=solution)
        for (int i = 1; i <= 6; i++) {
            aux6[i] = -1.0 * istTW[i] * stParent[i]; // RHS vector b
            for (int j = 1; j <= 6; j++) {
                aux6[i] += aChild[i][j] * ietTW[j] * etelParent[j] * CommonGlobals.profac[j];
                // LHS matrix A
                aux66[i][j] = ietTW[j] * ((i == j) ? 1.0 : 0.0) - istTW[j] * aChild[i][j] * CommonGlobals.profac[j];
            }
        }
        
        // Solve system (solution for x is stored in aux6)
        TensorUtils.ludcmpc(aux66, 6, 6, indx);
        TensorUtils.lubksbc(aux66, 6, 6, indx, aux6);
        
        // Reconstruct full stress/strain tensors in twin coordinates
        for (int i = 1; i <= 6; i++) {
            etelChild[i] = ietTW[i] * etelParent[i] + istTW[i] * aux6[i];
            stChild[i] = istTW[i] * stParent[i] + ietTW[i] * aux6[i];
        }

        // --- 6. Rotate Child stress back to sample coordinates ---
        TensorUtils.voigt(stChild, aux33, aux66, aux3333, 1); // 6-vec -> 3x3 tensor
        
        // 2nd-order rotation: T_sample = R_tw2sa * T_twin * R_tw2sa^T
        for (int ij = 1; ij <= 6; ij++) {
            int i = CommonGlobals.ijv[ij][1];
            int j = CommonGlobals.ijv[ij][2];
            CommonGlobals.stcs[ij][iChild] = 0.0;
            for (int i1 = 1; i1 <= 3; i1++) {
                for (int j1 = 1; j1 <= 3; j1++) {
                    CommonGlobals.stcs[ij][iChild] += rTW2SA[i][i1] * rTW2SA[j][j1] * aux33[i1][j1];
                }
            }
        }
        
        // --- 7. If large strains, rotate Child elastic strain back to sample coords ---
        if (CommonGlobals.kSM == 1.0) {
            TensorUtils.voigt(etelChild, aux33, aux66, aux3333, 1);
            for (int ij = 1; ij <= 6; ij++) {
                int i = CommonGlobals.ijv[ij][1];
                int j = CommonGlobals.ijv[ij][2];
                CommonGlobals.etelcs[ij][iChild] = 0.0;
                for (int i1 = 1; i1 <= 3; i1++) {
                    for (int j1 = 1; j1 <= 3; j1++) {
                        CommonGlobals.etelcs[ij][iChild] += rTW2SA[i][i1] * rTW2SA[j][j1] * aux33[i1][j1];
                    }
                }
            }
            // Copy hydrostatic strain from parent
            for (int i = 1; i <= 6; i++) {
                CommonGlobals.etelhycs[i][iChild] = CommonGlobals.etelhycs[i][iParent];
            }
        }
    }

  /**
   * Solves the self-consistent equation for the sample elasto-plastic 'ass2'
   * and thermal 'alfass' moduli.
   * Translation of Fortran SUBROUTINE sc_new.
   *
   * @param iopt        (In) 0=Iterate to convergence, 1/2=Single iteration.
   * @param liter       (In) Current iteration number from calling program.
   * @param iflagcond   (Out) 0=Converged, 1=Not converged.
   * @param e2          (Out) 6x6 Eshelby tensor (1-based, [7][7]).
   * @param interaction (In) Interaction mode (e.g., 1 for Tangent).
   * @param istep       (In) Current step number.
   */
  public static void scNew(int iopt, int liter, IntHolder iflagcond,
                           double[][] e2, int interaction, int istep) {

    int iguess = 1;
    int niter;

    if (iopt == 0) {
      niter = CommonGlobals.itmax_mod;
    } else {
      niter = 1;
    }

    while (iguess <= niter) {

      // --- Calc. Eshelby tensor 'e2' in axis of the ellipsoid ---
      TensorUtils.voigt(ModScNew.aux6, ModScNew.aux33, CommonGlobals.ass2, ModScNew.ass4, 3);

      update_shape(); // STUB

      for (int j = 1; j <= 3; j++) {
        ModScNew.axb[j] = CommonGlobals.axisph[0][j];
        for (int i = 1; i <= 3; i++) {
          ModScNew.EIGB[i][j] = CommonGlobals.axisph[i][j];
        }
      }

      // STUB (Calls eshelbyb stub)
      TensorUtils.stiffness_rotation(ModScNew.ass4, ModScNew.EIGB, ModScNew.axb, ModScNew.esim4, CommonGlobals.ESCR4);

      TensorUtils.voigt(ModScNew.aux6, ModScNew.aux33, e2, ModScNew.esim4, 4);

      TensorUtils.invten(e2, ModScNew.e2inv);
      TensorUtils.voigt(ModScNew.aux6, ModScNew.aux33, ModScNew.e2inv, CommonGlobals.EINVSA, 3);

      if (CommonGlobals.ishape >= 2) {
        for (int igr = 1; igr <= CommonGlobals.ngrain; igr++) {
          for (int j = 1; j <= 3; j++) {
            ModScNew.axb[j] = CommonGlobals.axisgr[0][j][igr];
            for (int i = 1; i <= 3; i++) {
              ModScNew.EIGB[i][j] = CommonGlobals.axisgr[i][j][igr];
            }
          }

          // STUB (Calls eshelbyb stub)
          TensorUtils.stiffness_rotation(ModScNew.ass4, ModScNew.EIGB, ModScNew.axb, ModScNew.ESIM4TEMP, ModScNew.ESCR4TEMP);

          TensorUtils.voigt(ModScNew.aux6, ModScNew.aux33, ModScNew.E2GRTEMP, ModScNew.ESIM4TEMP, 4);
          TensorUtils.invten(ModScNew.E2GRTEMP, ModScNew.E2INVGRTEMP);
          TensorUtils.voigt(ModScNew.aux6, ModScNew.aux33, ModScNew.E2INVGRTEMP, ModScNew.EINVSATEMP, 3);

          for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
              for (int k = 1; k <= 3; k++) {
                for (int l = 1; l <= 3; l++) {
                  CommonGlobals.EINVSAGR[i][j][k][l][igr] = ModScNew.EINVSATEMP[i][j][k][l];
                  CommonGlobals.ESCR4GR[i][j][k][l][igr] = ModScNew.ESCR4TEMP[i][j][k][l];
                }
              }
            }
          }
        } // end loop igr

        // This logic seems to be inside the igr loop in Fortran,
        // which means it only calculates aefgr for the *last* grain.
        // This is likely a bug in the original Fortran, but we replicate it.
        // If it's *not* a bug, 'igr' must be passed as an argument.
        // Assuming it's a bug and should be outside the loop:
        // --> No, 'igr' is used in aefgr(i,j,igr). The loop is correct.
        // --> But wait, the *last* igr is used in invten.
        // --> Re-reading: The Fortran code has this invten *inside* the IGR loop.
        int igr = CommonGlobals.ngrain; // Use the last grain
        TensorUtils.invten(ModScNew.E2GRTEMP, ModScNew.E2IGRTEMP);
        for (int i = 1; i <= 6; i++) {
          for (int j = 1; j <= 6; j++) {
            CommonGlobals.aefgr[i][j][igr] = 0.0;
            for (int k = 1; k <= 6; k++) {
              CommonGlobals.aefgr[i][j][igr] += CommonGlobals.ass2[i][k]
                  * (ModScNew.E2IGRTEMP[k][j] - CommonGlobals.id2[k][j]) * CommonGlobals.profac[k];
            }
          }
        }
      } // end ishape >= 2

      // --- Calc. effective stiffness: aef = ass2 * ( e2**(-1) - I ) ---
      TensorUtils.invten(e2, ModScNew.e2i);
      for (int i = 1; i <= 6; i++) {
        for (int j = 1; j <= 6; j++) {
          CommonGlobals.aef[i][j] = 0.0;
          for (int k = 1; k <= 6; k++) {
            CommonGlobals.aef[i][j] += CommonGlobals.ass2[i][k]
                * (ModScNew.e2i[k][j] - CommonGlobals.id2[k][j]) * CommonGlobals.profac[k];
          }
        }
      }

      // --- Defines an effective compliance for each grain 'meffc(ng)' ---
      ModScNew.tmis = tmismatch(CommonGlobals.ass2, CommonGlobals.css2, 6, 6); // STUB
      ModScNew.xmmin = 1.0 - 0.5 * ModScNew.tmis;
      ModScNew.xmmin = 1.0; // WARNING --> SECANT IS HARD WIRED !!!!

      if (interaction == 1) {
        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
          CommonGlobals.meffc[ng] = ModScNew.xmmin;
        }
      } else {
        mEffective(1, CommonGlobals.ngrain, ModScNew.xmmin);
        // Write to unit 12 (System.out)
        System.out.printf(" xmmin%10.3f%n", ModScNew.xmmin);
        System.out.println(" meffc");
        for (int ng = 1; ng <= CommonGlobals.ngrain; ng += 50) {
          for(int i=0; i<10 && (ng+i) <= CommonGlobals.ngrain; i++) {
            System.out.printf("%7.2f", CommonGlobals.meffc[ng+i]);
          }
          System.out.println();
        }
      }

      // --- Calculates localization and elasto-plastic stiffness tensors ---
      // anew = < acs2 * aloca > * < aloca >**(-1)

      // Initialize accumulators
      for (int i = 1; i <= 6; i++) {
        ModScNew.aux11[i] = 0.0;
        for (int j = 1; j <= 6; j++) {
          ModScNew.aux22[i][j] = 0.0; // < acs2 * aloca >
          ModScNew.aux26[i][j] = 0.0; // < aloca >
          ModScNew.aux27[i][j] = 0.0; // inv(< aloca >)
          ModScNew.aux29[i][j] = 0.0; // < (ass2+aef) * (acs2+aef)^-1 >
        }
      }

      // --- STARTS LOOP OVER GRAINS ---
      for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
        if (CommonGlobals.wgt[ng] > 0) {
          // aux24 = (acs2+aef)
          // aux25 = (ass2+aef)
          for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
              if (CommonGlobals.ishape == 0 || CommonGlobals.ishape == 1) {
                ModScNew.aux24[i][j] = CommonGlobals.acs2[i][j][ng] + CommonGlobals.meffc[ng] * CommonGlobals.aef[i][j];
                ModScNew.aux25[i][j] = CommonGlobals.ass2[i][j] + CommonGlobals.meffc[ng] * CommonGlobals.aef[i][j];
              } else { // ishape >= 2
                ModScNew.aux24[i][j] = CommonGlobals.acs2[i][j][ng] + CommonGlobals.meffc[ng] * CommonGlobals.aefgr[i][j][ng];
                ModScNew.aux25[i][j] = CommonGlobals.ass2[i][j] + CommonGlobals.meffc[ng] * CommonGlobals.aefgr[i][j][ng];
              }
            }
          }

          // aux21 = (acs2+aef)^-1
          TensorUtils.invten(ModScNew.aux24, ModScNew.aux21);

          // aux23 = aloca = (acs2+aef)^-1 * (ass2+aef)
          for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
              ModScNew.aux23[i][j] = 0.0;
              for (int k = 1; k <= 6; k++) {
                ModScNew.aux23[i][j] += ModScNew.aux21[i][k] * ModScNew.aux25[k][j] * CommonGlobals.profac[k];
              }
            }
          }

          // Accumulate weighted averages
          for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
              ModScNew.aux26[i][j] += ModScNew.aux23[i][j] * CommonGlobals.wgt[ng]; // <aloca>
              for (int k = 1; k <= 6; k++) {
                // < acs2 * aloca >
                ModScNew.aux22[i][j] += CommonGlobals.acs2[i][k][ng] * ModScNew.aux23[k][j]
                    * CommonGlobals.profac[k] * CommonGlobals.wgt[ng];
              }
            }
          }

          // --- Aux tensors for thermal tensor ---
          if (iopt != 1) {
            // aux28 = (ass2+aef) * (acs2+aef)^-1
            for (int i = 1; i <= 6; i++) {
              for (int j = 1; j <= 6; j++) {
                ModScNew.aux28[i][j] = 0.0;
                for (int k = 1; k <= 6; k++) {
                  ModScNew.aux28[i][j] += ModScNew.aux25[i][k] * ModScNew.aux21[k][j] * CommonGlobals.profac[k];
                }
              }
            }

            // aux12 = aux28 * acs2 * alfacs
            for (int i = 1; i <= 6; i++) {
              ModScNew.aux12[i] = 0.0;
              for (int j = 1; j <= 6; j++) {
                for (int k = 1; k <= 6; k++) {
                  ModScNew.aux12[i] += ModScNew.aux28[i][j] * CommonGlobals.acs2[j][k][ng]
                      * CommonGlobals.alfacs[k][ng] * CommonGlobals.profac[j] * CommonGlobals.profac[k];
                }
              }
            }

            // Accumulate
            for (int i = 1; i <= 6; i++) {
              ModScNew.aux11[i] += ModScNew.aux12[i] * CommonGlobals.wgt[ng]; // < ... >
              for (int j = 1; j <= 6; j++) {
                ModScNew.aux29[i][j] += ModScNew.aux28[i][j] * CommonGlobals.wgt[ng]; // < aux28 >
              }
            }
          } // end if(iopt != 1)
        } // end if(wgt(ng) > 0)
      } // --- ENDS LOOP OVER GRAINS ---

      // aux27 = <aloca>^-1
      TensorUtils.invten(ModScNew.aux26, ModScNew.aux27);

      // anew = <acs2*aloca> * <aloca>^-1
      for (int i = 1; i <= 6; i++) {
        for (int j = 1; j <= 6; j++) {
          ModScNew.anew[i][j] = 0.0;
          for (int k = 1; k <= 6; k++) {
            ModScNew.anew[i][j] += ModScNew.aux22[i][k] * ModScNew.aux27[k][j] * CommonGlobals.profac[k];
          }
        }
      }

      // Enforce symmetry
      for (int i = 1; i <= 6; i++) {
        for (int j = i + 1; j <= 6; j++) {
          ModScNew.anew[i][j] = 0.5 * (ModScNew.anew[i][j] + ModScNew.anew[j][i]);
          ModScNew.anew[j][i] = ModScNew.anew[i][j];
        }
      }

      // --- Check convergence ---
      ModScNew.error = tmismatch(CommonGlobals.ass2, ModScNew.anew, 6, 6); // STUB

      // Update ass2 for next iteration
      for (int i = 1; i <= 6; i++) {
        for (int j = 1; j <= 6; j++) {
          CommonGlobals.ass2[i][j] = ModScNew.anew[i][j];
        }
      }

      // --- Print iteration status ---
      PrintWriter out11 = IOUtils.writer11;
      if (iopt == 0) {
        if (out11 != null) out11.printf("For iteration = %3d the error is %12.4e%n", iguess, ModScNew.error);
        System.out.printf("ITER: %3d   ERROR: %12.4e   NG: %5d%n", iguess, ModScNew.error, CommonGlobals.ngrain);
      } else {
        if (out11 != null) out11.printf("For iteration = %3d the error is %12.4e%n", liter, ModScNew.error);
        System.out.printf("ITER: %3d   ERROR: %12.4e   NG: %5d%n", liter, ModScNew.error, CommonGlobals.ngrain);
      }

      // --- Control loop (iguess) ---
      if (iopt == 0) {
        if (ModScNew.error <= CommonGlobals.error_mod) {
          iguess = CommonGlobals.itmax_mod + 1; // Converged, exit loop
          iflagcond.value = 0;
        } else {
          if (iguess == CommonGlobals.itmax_mod && iguess > 1) {
            String msg = "CONVERGENCE IN SUBROUTINE SC NOT ACHIEVED AFTER " + CommonGlobals.itmax_mod
                + " ITERATIONS\nABNORMAL PROGRAM STOP......................";
            if (out11 != null) { out11.println(); out11.println(msg); }
            throw new RuntimeException(msg);
          }
          iguess++;
          iflagcond.value = 1;
        }
      } else { // iopt != 0 (single iteration)
        if (ModScNew.error <= CommonGlobals.error_mod && liter > 1) {
          iguess = CommonGlobals.itmax_mod + 1; // Converged, exit loop
          iflagcond.value = 0;
        } else {
          if (iguess == CommonGlobals.itmax_mod) { // Should only be 1
            // This block seems unreachable if niter=1, but included for safety
            String msg = "CONVERGENCE IN SUBROUTINE SC NOT ACHIEVED AFTER " + CommonGlobals.itmax_mod
                + " ITERATIONS\nABNORMAL PROGRAM STOP......................";
            if (out11 != null) { out11.println(); out11.println(msg); }
            throw new RuntimeException(msg);
          }
          iguess++;
          iflagcond.value = 1;
        }
      }

    } // --- Closes DO WHILE (IGUESS.LE.NITER) ---

    // --- Evaluates the sc thermal expansion tensor ---
    if (iopt != 1) {
      TensorUtils.invten(ModScNew.aux29, ModScNew.aux24);
      TensorUtils.invten(ModScNew.anew, ModScNew.aux22); // anew is the converged ass2

      for (int i = 1; i <= 6; i++) {
        CommonGlobals.alfass[i] = 0.0;
        for (int j = 1; j <= 6; j++) {
          for (int k = 1; k <= 6; k++) {
            CommonGlobals.alfass[i] += ModScNew.aux22[i][j] * ModScNew.aux24[j][k] * ModScNew.aux11[k]
                * CommonGlobals.profac[j] * CommonGlobals.profac[k];
          }
        }
      }
    } // end if(iopt != 1)
  }
    /**
     * Calculates residual strain pole figures.
     * Translation of Fortran SUBROUTINE PoleFig.
     *
     * @param icrysym  The crystal symmetry ID.
     * @param nfile    The file number to append to the output filename.
     * @param iopt     0=Initialize symmetry, 1=Calculate pole figure.
     * @param filecrys The path to the crystal data file (used by iopt=0).
     */
    public static void poleFig(int icrysym, int nfile, int iopt, String filecrys) {
        
        // iopt 0: Initialize symmetry operators
        if (iopt == 0) {
            File inputFile = new File(filecrys);
            try (Scanner s = new Scanner(inputFile)) {
                s.useLocale(Locale.US);

                s.nextLine(); // Skip prosa
                s.nextLine(); // Skip i
                
                String[] line = s.nextLine().trim().split("\\s+");
                for (int i = 1; i <= 3; i++) {
                  ModPolefig.cdim[i] = Double.parseDouble(line[i - 1]);
                  ModPolefig.cang[i] = Double.parseDouble(line[i + 2]) * CommonConstants.DEG_TO_RAD;
                }

                // Calculate CVEC (Note: this is the "a on x" convention)
              ModPolefig.cvec[1][1] = 1.0;
              ModPolefig.cvec[2][1] = 0.0;
              ModPolefig.cvec[3][1] = 0.0;
              ModPolefig.cvec[1][2] = Math.cos(ModPolefig.cang[3]);
              ModPolefig.cvec[2][2] = Math.sin(ModPolefig.cang[3]);
              ModPolefig.cvec[3][2] = 0.0;
              ModPolefig.cvec[1][3] = Math.cos(ModPolefig.cang[2]);
              ModPolefig.cvec[2][3] = (Math.cos(ModPolefig.cang[1]) - Math.cos(ModPolefig.cang[2]) * Math.cos(ModPolefig.cang[3])) / Math.sin(ModPolefig.cang[3]);
              ModPolefig.cvec[3][3] = Math.sqrt(1.0 - ModPolefig.cvec[1][3] * ModPolefig.cvec[1][3] - ModPolefig.cvec[2][3] * ModPolefig.cvec[2][3]);

                for (int j = 1; j <= 3; j++) {
                    for (int i = 1; i <= 3; i++) {
                      ModPolefig.cvec[i][j] *= ModPolefig.cdim[j];
                    }
                }

                // --- Initialize and Generate Symmetry Operators (mirrors crystal_symmetry) ---
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                        for (int m = 1; m <= 6; m++) ModPolefig.hx[i][j][m] = 0.0;
                        for (int n = 1; n <= 24; n++) ModPolefig.hh[i][j][n] = 0;
                    }
                }
                for (int i = 1; i <= 3; i++) ModPolefig.hh[i][i][1] = 1;
              ModPolefig.nsymop = 1;
                int mn = 1;

                if (icrysym == 4 || icrysym == 5) { // Orthorhombic or Monoclinic
                  ModPolefig.hh[1][1][2] = -1;
                  ModPolefig.hh[2][2][2] = -1;
                  ModPolefig.hh[3][3][2] = 1;
                  ModPolefig.hh[1][2][2] = 0;
                  ModPolefig.hh[2][1][2] = 0;
                  ModPolefig.nsymop = 2;
                }
                if (icrysym == 4) { // Orthorhombic
                  ModPolefig.hh[1][1][3] = -1;
                  ModPolefig.hh[2][2][3] = 1;
                  ModPolefig.hh[3][3][3] = 1;
                  ModPolefig.hh[1][1][4] = 1;
                  ModPolefig.hh[2][2][4] = -1;
                  ModPolefig.hh[3][3][4] = 1;
                  ModPolefig.nsymop = 4;
                }
                if (icrysym == 1) { // Cubic
                    // ... (Full cubic symmetry generation) ...
                  ModPolefig.hx[1][3][1] = 1.0;
                  ModPolefig.hx[2][1][1] = 1.0;
                  ModPolefig.hx[3][2][1] = 1.0;
                  ModPolefig.hx[1][2][2] = 1.0;
                  ModPolefig.hx[2][3][2] = 1.0;
                  ModPolefig.hx[3][1][2] = 1.0;
                    int old_nsymop = ModPolefig.nsymop; // 1
                    for (int m = 1; m <= 2; m++) {
                        for (int n = 1; n <= old_nsymop; n++) {
                            mn = m * old_nsymop + n;
                            for (int i = 1; i <= 3; i++)
                              for (int j = 1; j <= 3; j++)
                                for (int k = 1; k <= 3; k++)
                                  ModPolefig.hh[i][j][mn] += (int)(ModPolefig.hx[i][k][m] * ModPolefig.hh[k][j][n]);
                        }
                    }
                  ModPolefig.nsymop = mn; // 3

                  ModPolefig.hx[1][2][3] = 1.0;
                  ModPolefig.hx[2][1][3] = 1.0;
                  ModPolefig.hx[3][3][3] = 1.0;
                    old_nsymop = ModPolefig.nsymop; // 3
                    for (int n = 1; n <= old_nsymop; n++) {
                        mn = old_nsymop + n;
                        for (int i = 1; i <= 3; i++)
                          for (int j = 1; j <= 3; j++)
                            for (int k = 1; k <= 3; k++)
                              ModPolefig.hh[i][j][mn] += (int)(ModPolefig.hx[i][k][3] * ModPolefig.hh[k][j][n]);
                    }
                  ModPolefig.nsymop = mn; // 6
                    
                    for (int m = 1; m <= 3; m++) {
                      ModPolefig.ang = CommonConstants.PI_2 * m;
                      ModPolefig.hx[1][1][m] = Math.cos(ModPolefig.ang);
                      ModPolefig.hx[2][2][m] = Math.cos(ModPolefig.ang);
                      ModPolefig.hx[3][3][m] = 1.0;
                      ModPolefig.hx[1][2][m] = -Math.sin(ModPolefig.ang);
                      ModPolefig.hx[2][1][m] = Math.sin(ModPolefig.ang);
                    }
                    old_nsymop = ModPolefig.nsymop; // 6
                    for (int m = 1; m <= 3; m++) {
                        for (int n = 1; n <= old_nsymop; n++) {
                            mn = m * old_nsymop + n;
                            for (int i = 1; i <= 3; i++)
                              for (int j = 1; j <= 3; j++)
                                for (int k = 1; k <= 3; k++)
                                  ModPolefig.hh[i][j][mn] += (int)(ModPolefig.hx[i][k][m] * ModPolefig.hh[k][j][n]);
                        }
                    }
                  ModPolefig.nsymop = mn; // 24
                }
                if (icrysym == 2 || icrysym == 3) { // Hexagonal or Trigonal
                    if (icrysym == 2) ModPolefig.nrot = 6;
                    if (icrysym == 3) ModPolefig.nrot = 3;
                  ModPolefig.ang = CommonConstants.PI / ModPolefig.nrot;
                  ModPolefig.hh[1][1][2] = (int)(Math.cos(ModPolefig.ang) * Math.cos(ModPolefig.ang) - Math.sin(ModPolefig.ang) * Math.sin(ModPolefig.ang));
                  ModPolefig.hh[2][2][2] = -ModPolefig.hh[1][1][2];
                  ModPolefig.hh[3][3][2] = 1;
                  ModPolefig.hh[1][2][2] = (int)(2.0 * Math.cos(ModPolefig.ang) * Math.sin(ModPolefig.ang));
                  ModPolefig.hh[2][1][2] = ModPolefig.hh[1][2][2];
                  ModPolefig.nsymop = 2;
                    for (int nr = 1; nr <= ModPolefig.nrot - 1; nr++) {
                      ModPolefig.ang = nr * CommonConstants.PI2 / ModPolefig.nrot;
                      ModPolefig.hx[1][1][nr] = Math.cos(ModPolefig.ang);
                      ModPolefig.hx[2][2][nr] = Math.cos(ModPolefig.ang);
                      ModPolefig.hx[3][3][nr] = 1.0;
                      ModPolefig.hx[1][2][nr] = -Math.sin(ModPolefig.ang);
                      ModPolefig.hx[2][1][nr] = Math.sin(ModPolefig.ang);
                    }
                    int old_nsymop = ModPolefig.nsymop; // 2
                    for (int m = 1; m <= ModPolefig.nrot - 1; m++) {
                        for (int n = 1; n <= old_nsymop; n++) {
                            mn = m * old_nsymop + n;
                            for (int i = 1; i <= 3; i++)
                              for (int j = 1; j <= 3; j++)
                                for (int k = 1; k <= 3; k++)
                              ModPolefig.hh[i][j][mn] += (int)(ModPolefig.hx[i][k][m] * ModPolefig.hh[k][j][n]);
                        }
                    }
                  ModPolefig.nsymop = mn;
                }
                
            } catch (FileNotFoundException e) {
                throw new RuntimeException("Crystal file not found: " + filecrys, e);
            }
            
        } else { // iopt == 1: Calculate Pole Figure
            
            String filename = String.format(Locale.US, "StrainPF%04d.out", nfile);
            
            try (Scanner s = new Scanner(new File("strainpf.in"));
                 PrintWriter out22 = new PrintWriter(new FileWriter(filename))) {
                 
                s.useLocale(Locale.US);

                String prosa = s.nextLine();
                if (IOUtils.writer19 != null)
                  IOUtils.writer19.println(prosa);
                prosa = s.nextLine();
                if (IOUtils.writer19 != null)
                  IOUtils.writer19.println(prosa);
                
                String[] line = s.nextLine().trim().split("\\s+");
              ModPolefig.ndiff2 = Integer.parseInt(line[0]);
              ModPolefig.spread2 = Double.parseDouble(line[1]);
              ModPolefig.ipolefig = Integer.parseInt(line[2]);
              ModPolefig.nPoles = Integer.parseInt(line[3]);
                
                if (ModPolefig.ipolefig < 0 || ModPolefig.ipolefig > 3) ModPolefig.ipolefig = 0;
                
                if (IOUtils.writer19 != null) IOUtils.writer19.printf(Locale.US, "%5d%6.2f%n", ModPolefig.ndiff2, ModPolefig.spread2);
              ModPolefig.toler2 = Math.cos(ModPolefig.spread2 * CommonConstants.DEG_TO_RAD);

                prosa = s.nextLine(); if (IOUtils.writer19 != null) IOUtils.writer19.println(prosa);
                prosa = s.nextLine(); if (IOUtils.writer19 != null) IOUtils.writer19.println(prosa);
                
                // --- Loop over all directions in strainpf.in ---
                for (int n = 1; n <= ModPolefig.ndiff2; n++) {
                  ModPolefig.nind = 3;
                    if (icrysym == 2 || icrysym == 3) ModPolefig.nind = 4;
                    
                    line = s.nextLine().trim().split("\\s+");
                    for (int i = 1; i <= ModPolefig.nind; i++)
                      ModPolefig.isn[i] = Integer.parseInt(line[i-1]);
                  ModPolefig.chi = Double.parseDouble(line[ModPolefig.nind]);
                  ModPolefig.eta = Double.parseDouble(line[ModPolefig.nind+1]);

                  ModPolefig.eta *= CommonConstants.DEG_TO_RAD;
                  ModPolefig.chi *= CommonConstants.DEG_TO_RAD;
                    
                    // sb = sample vector (direction)
                  ModPolefig.sb[1] = Math.cos(ModPolefig.eta) * Math.sin(ModPolefig.chi);
                  ModPolefig.sb[2] = Math.sin(ModPolefig.eta) * Math.sin(ModPolefig.chi);
                  ModPolefig.sb[3] = Math.cos(ModPolefig.chi);

                    for(int i=1; i<=ModPolefig.nind; i++) ModPolefig.ipole[i] = ModPolefig.isn[i];
                    if (ModPolefig.nind == 4) ModPolefig.isn[3] = ModPolefig.isn[4];

                    // Write header to output file
                    int j1 = ModPolefig.ndiff2 / ModPolefig.nPoles;
                    int i_test = (n / j1) * j1;
                    if (n == 1 || (i_test + 1 == n && n < ModPolefig.ndiff2)) {
                        out22.println("EPSC Strain Pole Figures");
                        out22.printf(Locale.US, "poles %d %d %d%n", ModPolefig.isn[1], ModPolefig.isn[2], ModPolefig.isn[3]);
                        out22.println("phi     theta       int");
                    }

                    // Calculate sn = crystal vector
                  ModPolefig.sn[1] = ModPolefig.isn[1] / ModPolefig.cvec[1][1];
                  ModPolefig.sn[2] = (ModPolefig.isn[2] - ModPolefig.cvec[1][2] * ModPolefig.sn[1]) / ModPolefig.cvec[2][2];
                  ModPolefig.sn[3] = (ModPolefig.isn[3] - ModPolefig.cvec[1][3] * ModPolefig.sn[1] - ModPolefig.cvec[2][3] * ModPolefig.sn[2]) / ModPolefig.cvec[3][3];
                  double snor = Math.sqrt(ModPolefig.sn[1] * ModPolefig.sn[1] + ModPolefig.sn[2] * ModPolefig.sn[2] + ModPolefig.sn[3] * ModPolefig.sn[3]);
                    for (int j_norm = 1; j_norm <= 3; j_norm++) {
                      ModPolefig.sn[j_norm] /= snor;
                        if (Math.abs(ModPolefig.sn[j_norm]) < 1e-3) ModPolefig.sn[j_norm] = 0.0;
                    }

                    // --- Generate equivalent plane normals 'sneq' ---
                    for (int ns = 1; ns <= ModPolefig.nsymop; ns++) {
                      ModPolefig.itag[ns] = 0;
                        for (int i = 1; i <= 3; i++) {
                          ModPolefig.sneq[i][ns] = 0.0;
                            for (int j_sym = 1; j_sym <= 3; j_sym++) {
                              ModPolefig.sneq[i][ns] += ModPolefig.hh[i][j_sym][ns] * ModPolefig.sn[j_sym];
                            }
                        }
                    }
                    
                    // Tag redundant poles
                    for (int m = 1; m < ModPolefig.nsymop; m++) {
                        if (ModPolefig.itag[m] == 0) {
                            for (int nn = m + 1; nn <= ModPolefig.nsymop; nn++) {
                              ModPolefig.snpro = ModPolefig.sneq[1][m] * ModPolefig.sneq[1][nn] + ModPolefig.sneq[2][m] * ModPolefig.sneq[2][nn] + ModPolefig.sneq[3][m] * ModPolefig.sneq[3][nn];
                                if (ModPolefig.snpro >= 0.999 && ModPolefig.snpro <= 1.001) ModPolefig.itag[nn] = 1; // Coincident
                                if (ModPolefig.snpro >= -1.001 && ModPolefig.snpro <= -0.999) ModPolefig.itag[nn] = 1; // Opposite
                            }
                        }
                    }

                    // Create compact list
                  ModPolefig.npol = 0;
                    for (int nn = 1; nn <= ModPolefig.nsymop; nn++) {
                        if (ModPolefig.itag[nn] == 0) {
                          ModPolefig.npol++;
                            int isign = (ModPolefig.sneq[3][nn] < 0.0) ? -1 : 1;
                          ModPolefig.sneq[1][ModPolefig.npol] = isign * ModPolefig.sneq[1][nn];
                          ModPolefig.sneq[2][ModPolefig.npol] = isign * ModPolefig.sneq[2][nn];
                          ModPolefig.sneq[3][ModPolefig.npol] = isign * ModPolefig.sneq[3][nn];
                        }
                    }
                    
                    // Rotate sample vector 'sb' by pole figure rotation 'temprot'
                    for (int i = 1; i <= 3; i++) {
                      ModPolefig.vs[i] = 0.0;
                        for (int j = 1; j <= 3; j++) {
                          ModPolefig.vs[i] += ModPolefig.temprot[j][i][ModPolefig.ipolefig] * ModPolefig.sb[j];
                        }
                    }

                    // --- Identify grains contributing to this pole ---
                  ModPolefig.sumwgt = 0.0;
                  ModPolefig.sumstr = 0.0;
                    for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                        for (int ipl = 1; ipl <= ModPolefig.npol; ipl++) {
                            
                            // ps = rotated crystal vector (sneq)
                            for (int i = 1; i <= 3; i++) {
                              ModPolefig.ps[i] = 0.0;
                                for (int j = 1; j <= 3; j++) {
                                  ModPolefig.ps[i] += CommonGlobals.r[j][i][ng] * ModPolefig.sneq[j][ipl];
                                }
                            }
                            
                            // prodesc = ps . vs
                          ModPolefig.prodesc = 0.0;
                            for (int i = 1; i <= 3; i++) {
                              ModPolefig.prodesc += ModPolefig.ps[i] * ModPolefig.vs[i];
                            }

                            if (Math.abs(ModPolefig.prodesc) >= ModPolefig.toler2) {
                                // This grain contributes
                                for (int i = 1; i <= 6; i++) {
                                  ModPolefig.etelcsx[i] = 0.0;
                                    for (int j = 1; j <= 6; j++) {
                                      ModPolefig.etelcsx[i] += CommonGlobals.scs2[i][j][ng] * CommonGlobals.stcs[j][ng] * CommonGlobals.profac[j];
                                    }
                                }
                              ModPolefig.eps = 0.0;
                                for (int ij = 1; ij <= 6; ij++) {
                                    int i = CommonGlobals.ijv[ij][1];
                                    int j = CommonGlobals.ijv[ij][2];
                                  ModPolefig.eps += ModPolefig.vs[i] * ModPolefig.vs[j] * ModPolefig.etelcsx[ij] * CommonGlobals.profac[ij];
                                }
                              ModPolefig.sumstr += ModPolefig.eps * CommonGlobals.wgt[ng];
                              ModPolefig.sumwgt += CommonGlobals.wgt[ng];
                            } // end if prodesc
                        } // end ipl
                    } // end ng
                    
                    // --- Write average strain for this pole ---
                    if (ModPolefig.sumwgt != 0.0) {
                      ModPolefig.sumstr /= ModPolefig.sumwgt;
                        out22.printf(Locale.US, "%12.5f%12.5f%12.1f%n",
                            ModPolefig.eta * CommonConstants.RAD_TO_DEG, ModPolefig.chi * CommonConstants.RAD_TO_DEG, ModPolefig.sumstr * 1.0e6);
                    } else {
                        out22.printf(Locale.US, "%12.5f%12.5f       NAN%n",
                            ModPolefig.eta * CommonConstants.RAD_TO_DEG, ModPolefig.chi * CommonConstants.RAD_TO_DEG);
                    }
                } // end loop n
                
            } catch (FileNotFoundException e) {
                throw new RuntimeException("strainpf.in file not found", e);
            } catch (IOException e) {
                System.err.println("Error writing pole figure file: " + filename);
                e.printStackTrace();
            }
        } // end iopt == 1
    }
    
    /**
     * Calculates eigenvalues, eigenvectors, and Euler angles for the
     * average phase ellipsoid (AXISPH, EULERPH) and, if iShape >= 2,
     * for each individual grain (AXISGR).
     *
     * Full translation of Fortran SUBROUTINE UPDATE_SHAPE.
     */
    public static void update_shape() {

        // --- Local 1-based work arrays ---
        double[] W = new double[4];      // Eigenvalues
        double[][] BX = new double[4][4]; // F * F^T
        double[][] B = new double[4][4];  // Eigenvectors
        double[][] BT = new double[4][4]; // Transposed eigenvectors
        
        // Holders for 'out' parameters from Jacobi
        IntHolder nrot = new IntHolder(0);
        IntHolder ier = new IntHolder(0);

        // --- 1. Calculate for the average phase ellipsoid (fijph) ---
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                BX[i][j] = 0.0;
                for (int k = 1; k <= 3; k++) {
                    BX[i][j] += CommonGlobals.fijph[i][k] * CommonGlobals.fijph[j][k];
                }
            }
        }

        // Find eigenvalues (W) and eigenvectors (B)
        NumericalRecipes.jacobi(BX, 3, 3, W, B, nrot, ier);
        NumericalRecipes.eigsrt(W, B, 3, 3);
        
        if (ier.value == 1) {
            throw new RuntimeException("ERROR IN UPDATE_SHAPE FOR PHASE ELLIPSOID");
        }

        // --- Re-order axes ---
        // Redefine axis 2 to be the largest.
        // If system is left-handed (det<0), make it right-handed.
        double sign = -1.0;
        if (TensorUtils.det(B) <= 0.0) {
            sign = 1.0;
        }

        for (int i = 1; i <= 3; i++) {
            double exchange = B[i][1];
            B[i][1] = B[i][2];
            B[i][2] = exchange * sign;
        }
        double exchange = W[1];
        W[1] = W[2];
        W[2] = exchange;
        
        // Store results in global arrays
        for (int i = 1; i <= 3; i++) {
            CommonGlobals.axisph[0][i] = Math.sqrt(W[i]); // Eigenvalues (axis lengths)
            for (int j = 1; j <= 3; j++) {
                CommonGlobals.axisph[i][j] = B[i][j]; // Eigenvectors (axis directions)
                BT[i][j] = B[j][i]; // Transpose for Euler angle calculation
            }
        }

        // Calculate and store Euler angles
        TensorUtils.EulerAngles angles = TensorUtils.matrixToEuler(BT);
        CommonGlobals.eulerph[1] = angles.phi;
        CommonGlobals.eulerph[2] = angles.theta;
        CommonGlobals.eulerph[3] = angles.omega;

        // --- 2. Calculate for individual grains (fijgr) if iShape >= 2 ---
        if (CommonGlobals.ishape >= 2) {
            for (int igr = 1; igr <= CommonGlobals.ngrain; igr++) {
                
                // BX = FIJGR * FIJGR^T
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                        BX[i][j] = 0.0;
                        for (int k = 1; k <= 3; k++) {
                            BX[i][j] += CommonGlobals.fijgr[i][k][igr] * CommonGlobals.fijgr[j][k][igr];
                        }
                    }
                }

                NumericalRecipes.jacobi(BX, 3, 3, W, B, nrot, ier);
                NumericalRecipes.eigsrt(W, B, 3, 3);
                
                if (ier.value == 1) {
                    throw new RuntimeException("Error in update_shape for grain " + igr);
                }
                
                // Re-order axes and ensure right-handed system
                sign = -1.0;
                if (TensorUtils.det(B) <= 0.0) {
                    sign = 1.0;
                }
                for (int i = 1; i <= 3; i++) {
                    exchange = B[i][1];
                    B[i][1] = B[i][2];
                    B[i][2] = exchange * sign;
                }
                exchange = W[1];
                W[1] = W[2];
                W[2] = exchange;

                // Store results for the grain
                for (int i = 1; i <= 3; i++) {
                    CommonGlobals.axisgr[0][i][igr] = Math.sqrt(W[i]);
                    for (int j = 1; j <= 3; j++) {
                        CommonGlobals.axisgr[i][j][igr] = B[i][j];
                    }
                }
            } // end loop over grains
        } // end if ishape
    }
    
    /**
     * Updates the deformation gradient tensors (fijph and fijgr)
     * using the current strain rates.
     * <p>
     * F_new = (L * step + I) * F_old
     * <p>
     * Translation of Fortran SUBROUTINE UPDATE_FIJ.
     *
     * @param step The time/strain step increment.
     */
    public static void updateFij(double step) {

        // --- Initialize 3x3 Identity Matrix ---
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                ModUpdateFij.XID3[i][j] = (i == j) ? 1 : 0;
            }
        }

        // --- 1. Update the average phase deformation gradient 'fijph' ---
        
        // Convert 6-vec macroscopic strain rate 'etrss' to 3x3 tensor 'etrss_33'
        TensorUtils.voigt(CommonGlobals.etrss, ModUpdateFij.etrss_33,
                          ModUpdateFij.C2, ModUpdateFij.C4, 1);

        // FNEW = (L_macro * step + I) * FIJPH
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                ModUpdateFij.FNEW[i][j] = 0.0;
                for (int k = 1; k <= 3; k++) {
                    ModUpdateFij.FNEW[i][j] += (ModUpdateFij.etrss_33[i][k] * step + ModUpdateFij.XID3[i][k])
                                            * CommonGlobals.fijph[k][j];
                }
            }
        }
        // Update FIJPH
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                CommonGlobals.fijph[i][j] = ModUpdateFij.FNEW[i][j];
            }
        }

        // --- 2. Update individual grain deformation gradients 'fijgr' ---
        if (CommonGlobals.ishape >= 2) {
            for (int igr = 1; igr <= CommonGlobals.ngrain; igr++) {
                
                // Get 6-vec grain strain rate
                for (int i = 1; i <= 6; i++) {
                    ModUpdateFij.etrcs6[i] = CommonGlobals.etrcs[i][igr];
                }
                
                // Convert 6-vec 'etrcs6' to 3x3 tensor 'etrcs_33'
                TensorUtils.voigt(ModUpdateFij.etrcs6, ModUpdateFij.etrcs_33,
                                  ModUpdateFij.C2, ModUpdateFij.C4, 1);

                // FNEW = (L_grain * step + I) * FIJGR
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                        ModUpdateFij.FNEW[i][j] = 0.0;
                        for (int k = 1; k <= 3; k++) {
                            ModUpdateFij.FNEW[i][j] += (ModUpdateFij.etrcs_33[i][k] * step + ModUpdateFij.XID3[i][k])
                                                    * CommonGlobals.fijgr[k][j][igr];
                        }
                    }
                }
                // Update FIJGR
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                        CommonGlobals.fijgr[i][j][igr] = ModUpdateFij.FNEW[i][j];
                    }
                }
            } // end loop over grains
        } // end if ishape
    }

    /**
     * Validates the boundary conditions (stress/strain flags) to prevent
     * over- or under-constraint. It also sets the global control variable 'icvx'.
     * Translation of Fortran SUBROUTINE load_conditions.
     */
    public static void loadConditions() {

        int[] tempa = new int[7]; // 1-based

        // --- Check ifulletbc (full strain tensor flags) ---
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                if (CommonGlobals.ifulletbc[i][j] != 0 && CommonGlobals.ifulletbc[i][j] != 1) {
                    throw new RuntimeException("ERROR! IFBC COMPONENT (" + i + "," + j + ") CAN ONLY BE 0 OR 1.");
                }
            }
        }
        
        // --- Check istbc (stress flags) ---
        for (int i = 1; i <= 6; i++) {
            if (CommonGlobals.istbc[i] != 0 && CommonGlobals.istbc[i] != 1) {
                throw new RuntimeException("ERROR! ISTBC COMPONENT " + i + " CAN ONLY BE 0 OR 1.");
            }
        }

        // --- Check for over- or under-constraint ---
        for (int i = 1; i <= 6; i++) {
            tempa[i] = CommonGlobals.ietbc[i] + CommonGlobals.istbc[i];
            if (tempa[i] == 2) {
                throw new RuntimeException("CANNOT CONSTRAIN STRAIN COMPONENT (" + i + ")\n"
                                         + "AND STRESS COMPONENT (" + i + ") - MUST RELAX ONE.");
            }
            if (tempa[i] == 0) {
                throw new RuntimeException("CANNOT RELAX STRAIN COMPONENT (" + i + ") AND\n"
                                         + "STRESS COMPONENT (" + i + ") - MUST CONSTRAIN ONE.");
            }
        }

        /* // --- Full strain tensor checks (Commented out in original) ---
        for (int i = 1; i <= 2; i++) {
            for (int j = i + 1; j <= 3; j++) {
                if (CommonGlobals.ifulletbc[i][j] + CommonGlobals.ifulletbc[j][i] == 0) {
                    throw new RuntimeException("CHECK OFF-DIAGONAL STRAIN BOUNDARY CONDITIONS\n"
                                             + "CANNOT RELAX BOTH OFF-DIAGONAL COMPONENTS");
                } else if (CommonGlobals.ifulletbc[i][j] != CommonGlobals.ifulletbc[j][i]) {
                    throw new RuntimeException("CHECK OFF-DIAGONAL STRIAN BOUNDARY CONDITIONS\n"
                                             + "YOUR LAB FRAME IS ROTATING");
                }
            }
        }
        */ // --- End of commented-out block ---

        // --- Determine control variable 'icvx' ---
        if (CommonGlobals.i_control_var == 0) {
            // Temperature Control
            CommonGlobals.icvx = 0;
        } else {
            if (CommonGlobals.ietbc[CommonGlobals.i_control_var] == 1) {
                // Incremental Strain Control
                CommonGlobals.icvx = CommonGlobals.i_control_var;
            } else if (CommonGlobals.istbc[CommonGlobals.i_control_var] == 1) {
                // Incremental Stress Control
                CommonGlobals.icvx = CommonGlobals.i_control_var + 6;
            } else {
                throw new RuntimeException("ERROR-CANNOT FIND CONTROLLING BOUNDARY CONDITION\n"
                                         + "FOR ICTRL =" + CommonGlobals.i_control_var);
            }
        }
    }
    
    /**
     * Updates the orientation matrix 'r' for every grain based on
     * plastic slip, local accommodation, and macroscopic spin.
     * Translation of Fortran SUBROUTINE UPDATE_ORIENTATION.
     *
     * @param step  The step increment.
     * @param istep The current step number.
     * @param iopt  Option flag (1=apply rotation, 0=calculate only).
     */
    public static void updateOrientation(double step, int istep, int iopt) {

        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
            
            // --- 1. Find local rotation component (ROTLOC) ---
            // AS = ESCR4 * EINVSA
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                    for (int k = 1; k <= 3; k++) {
                        for (int l = 1; l <= 3; l++) {
                            double DUMMY = 0.0;
                            for (int k1 = 1; k1 <= 3; k1++) {
                                for (int l1 = 1; l1 <= 3; l1++) {
                                    if (CommonGlobals.ishape == 0 || CommonGlobals.ishape == 1) {
                                        DUMMY += CommonGlobals.ESCR4[i][j][k1][l1] * CommonGlobals.EINVSA[k1][l1][k][l];
                                    } else { // ishape >= 2
                                        DUMMY += CommonGlobals.ESCR4GR[i][j][k1][l1][ng] * CommonGlobals.EINVSAGR[k1][l1][k][l][ng];
                                    }
                                }
                            }
                            CommonGlobals.as[i][j][k][l][ng] = DUMMY;
                        }
                    }
                }
            }

            // DEV = etrcs_grain - etrav_avg
            for (int i = 1; i <= 6; i++) {
                ModUpdateOrientation.DEV[i] = CommonGlobals.etrcs[i][ng] - CommonGlobals.etrav[i];
            }
            
            // Convert 6-vec DEV to 3x3 tensor DEV33
            TensorUtils.voigt(ModUpdateOrientation.DEV, ModUpdateOrientation.DEV33, ModUpdateOrientation.C2, ModUpdateOrientation.C4, 1);

            // ROTLOC = AS : DEV33
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                  ModUpdateOrientation.ROTLOC[i][j] = 0.0;
                    for (int k = 1; k <= 3; k++) {
                        for (int l = 1; l <= 3; l++) {
                          ModUpdateOrientation.ROTLOC[i][j] += CommonGlobals.as[i][j][k][l][ng] * ModUpdateOrientation.DEV33[k][l];
                        }
                    }
                }
            }

            // --- 2. Find plastic slip rotation component (ROTSLIP) ---
            // rg = r(ng)
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                  ModUpdateOrientation.rg[j][i] = CommonGlobals.r[j][i][ng];
                  ModUpdateOrientation.LIJGR0[i][j] = 0.0;
                }
            }
            
            // Calculate plastic velocity gradient LIJGR0 in sample frame
            for (int is = 1; is <= CommonGlobals.nsys; is++) {
                // Rotate slip normal (n) and burgers vector (b) to sample frame
                // dnsa = R^T * ncc
                // dbsa = R^T * bcc
                for (int i = 1; i <= 3; i++) {
                    CommonGlobals.dnsa[i][is] = 0.0;
                    CommonGlobals.dbsa[i][is] = 0.0;
                    for (int j = 1; j <= 3; j++) {
                        CommonGlobals.dnsa[i][is] += ModUpdateOrientation.rg[j][i] * CommonGlobals.ncc[j][is];
                        CommonGlobals.dbsa[i][is] += ModUpdateOrientation.rg[j][i] * CommonGlobals.bcc[j][is];
                    }
                }
                
                // LIJGR0 = sum( dbsa (tensor) dnsa * gamd )
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                      ModUpdateOrientation.LIJGR0[i][j] += CommonGlobals.dbsa[i][is] * CommonGlobals.dnsa[j][is] * CommonGlobals.gamd[is][ng];
                    }
                }
            }
            
            // ROTSLIP = anti-symmetric part of LIJGR0
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                  ModUpdateOrientation.rotslip[i][j] = (ModUpdateOrientation.LIJGR0[i][j] - ModUpdateOrientation.LIJGR0[j][i]) / 2.0;
                }
            }

            // --- 3. Sum rotation components and create rotation matrix ---
            // ROT = (macroscopic_spin + local_accomodation_spin - plastic_spin) * step
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                  ModUpdateOrientation.rot[i][j] = (CommonGlobals.omegabcr[i][j] + ModUpdateOrientation.ROTLOC[i][j] - ModUpdateOrientation.rotslip[i][j]) * step;
                  CommonGlobals.omegag[i][j][ng] = CommonGlobals.omegabcr[i][j] + ModUpdateOrientation.ROTLOC[i][j] - ModUpdateOrientation.rotslip[i][j];
                }
            }

            // Create finite rotation matrix 'AROTG' from spin tensor 'rot'
            reorientGrain(ModUpdateOrientation.AROTG, ModUpdateOrientation.rot);
            // rodrigues(rot, AROTG); // Alternative (commented out in original)

            // --- 4. Apply the rotation to the grain's orientation matrix 'r' ---
            if (iopt == 1) {
                // rg = AROTG * r(ng)
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                      ModUpdateOrientation.rg[j][i] = 0.0; // Use rg as a temp holder for the new r
                        for (int k = 1; k <= 3; k++) {
                          ModUpdateOrientation.rg[j][i] += ModUpdateOrientation.AROTG[i][k] * CommonGlobals.r[j][k][ng];
                        }
                    }
                }
                // Update r(ng)
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                        CommonGlobals.r[j][i][ng] = ModUpdateOrientation.rg[j][i];
                    }
                }
            }
        } // --- END OF DO LOOP OVER ALL GRAINS ---

        // Recalculate all grain properties in sample coordinates
        crToSa(1, CommonGlobals.ngrain, 0);
        crToSa(1, CommonGlobals.ngrain, 1);
    }

    /**
     * Builds an incremental rotation matrix 'arot' from a lattice spin tensor 'c'
     * using the Rodrigues/Cayley formula.
     * Translation of Fortran SUBROUTINE REORIENT_GRAIN.
     *
     * @param arot (Out) The 3x3 rotation matrix (1-based, [4][4]).
     * @param c    (In) The 3x3 antisymmetric spin tensor (1-based, [4][4]).
     */
    public static void reorientGrain(double[][] arot, double[][] c) {
        
        // Local 1-based arrays
        double[][] th = new double[4][4];
        double[][] th2 = new double[4][4];
        double[] v = new double[4];
        double[] vbar = new double[4];

        // Convert spin tensor C to axial vector v
        v[1] = c[3][2];
        v[2] = c[1][3];
        v[3] = c[2][1];
        
        double snorm = Math.sqrt(v[1] * v[1] + v[2] * v[2] + v[3] * v[3]); // rotation angle phi
        double snorm1 = Math.tan(snorm / 2.0);
        
        if (snorm < 1.0e-6) snorm = 1.0; // Avoid division by zero

        // vbar = Rodrigues vector = v_axis * tan(phi/2)
        for (int i = 1; i <= 3; i++) {
            vbar[i] = snorm1 * v[i] / snorm;
        }
        
        snorm = vbar[1] * vbar[1] + vbar[2] * vbar[2] + vbar[3] * vbar[3]; // |vbar|^2
        
        // th = antisymmetric tensor of vbar
        th[3][2] = vbar[1]; th[2][3] = -vbar[1];
        th[1][3] = vbar[2]; th[3][1] = -vbar[2];
        th[2][1] = vbar[3]; th[1][2] = -vbar[3];
        th[1][1] = 0.0; th[2][2] = 0.0; th[3][3] = 0.0;

        // th2 = th * th
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                th2[i][j] = 0.0;
                for (int k = 1; k <= 3; k++) {
                    th2[i][j] += th[i][k] * th[k][j];
                }
            }
        }
        
        // AROT = I + 2*(th + th^2) / (1 + |vbar|^2)
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                arot[i][j] = ((i == j) ? 1.0 : 0.0) + 2.0 * (th[i][j] + th2[i][j]) / (1.0 + snorm);
            }
        }
    }

    

    /**
     * Builds an incremental rotation matrix 'arot' from a lattice spin tensor 'c'
     * using the standard Rodrigues formula.
     * Translation of Fortran SUBROUTINE RODRIGUES.
     *
     * @param arot (Out) The 3x3 rotation matrix (1-based, [4][4]).
     * @param c    (In) The 3x3 antisymmetric spin tensor (1-based, [4][4]).
     */
    public static void rodrigues(double[][] c, double[][] arot) {
        
        // Local 1-based arrays
        double[][] c2 = new double[4][4];
        double[] v = new double[4];
        
        // v = axial vector of c
        v[1] = c[3][2];
        v[2] = c[1][3];
        v[3] = c[2][1];
        
        double vnorm = Math.sqrt(v[1] * v[1] + v[2] * v[2] + v[3] * v[3]); // phi
        
        if (vnorm < 1.0e-6) {
            // No rotation, return identity
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                    arot[i][j] = TensorUtils.XID33[i][j];
                }
            }
            return;
        }

        double coef1 = Math.sin(vnorm) / vnorm;
        double coef2 = (1.0 - Math.cos(vnorm)) / (vnorm * vnorm);

        // c2 = c * c
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                c2[i][j] = 0.0;
                for (int k = 1; k <= 3; k++) {
                    c2[i][j] += c[i][k] * c[k][j];
                }
            }
        }
        
        // AROT = I + coef1*C + coef2*C^2
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                arot[i][j] = TensorUtils.XID33[i][j] + coef1 * c[i][j] + coef2 * c2[i][j];
            }
        }
    }
    
    /**
     * Implements the Voce hardening law.
     * iopt=0: Reads parameters from unit 1 (assumed open).
     * iopt=1: Initializes tau for all grains.
     * iopt=2: Calculates hardening matrix 'hd' for grain 'ng'.
     * <p>
     * Full translation of Fortran SUBROUTINE CRSS_Voce.
     *
     * @param ioption Control flag.
     * @param ng      Grain index (for iopt=1 and 2).
     */
    public static void crssVoce(int ioption, int ng) {

        // iopt 0: Read parameters from unit 1
        if (ioption == 0) {
            
            // Allocate module arrays
            ModCrssVoce.hselfx = new double[CommonGlobals.NMOD + 1];
            ModCrssVoce.hlatex = new double[CommonGlobals.NMOD + 1][CommonGlobals.NMOD + 1];
            
            if (IOUtils.scanner1 == null) {
                throw new RuntimeException("CRSS_Voce(0) called but file unit 1 (IOUtils.scanner1) is not open.");
            }
            Scanner s = IOUtils.scanner1; // Use the static scanner

            int isys = 1;
            for (int im = 1; im <= CommonGlobals.nmodes; im++) {
                s.nextLine(); // Skip comment line
                
                ModCrssVoce.twvol = 0.0;
                ModCrssVoce.gamdthres = 0.0;
                if (CommonGlobals.itw[im] == 1) {
                    String[] line = s.nextLine().trim().split("\\s+");
                    ModCrssVoce.twvol = Double.parseDouble(line[0]);
                    ModCrssVoce.gamdthres = Double.parseDouble(line[1]);
                }
                
                String[] line = s.nextLine().trim().split("\\s+");
                ModCrssVoce.tau0x = Double.parseDouble(line[0]);
                ModCrssVoce.tau1x = Double.parseDouble(line[1]);
                ModCrssVoce.thet0x = Double.parseDouble(line[2]);
                ModCrssVoce.thet1x = Double.parseDouble(line[3]);

                ModCrssVoce.hselfx[im] = 1.0; // Default
                
                line = s.nextLine().trim().split("\\s+");
                for (int jm = 1; jm <= CommonGlobals.nmodes; jm++) {
                    ModCrssVoce.hlatex[im][jm] = Double.parseDouble(line[jm - 1]);
                }

                CommonGlobals.TwinFrac[im] = ModCrssVoce.twvol;
                CommonGlobals.TwinCRSS[im] = ModCrssVoce.gamdthres;

                // --- Sanitize hardening parameters ---
                if (ModCrssVoce.thet1x < 0.001 * ModCrssVoce.tau0x) {
                    ModCrssVoce.thet1x = 0.001 * ModCrssVoce.tau0x;
                }
                if (ModCrssVoce.tau1x < 0.001 * ModCrssVoce.tau0x) {
                    ModCrssVoce.tau1x = 0.0;
                    ModCrssVoce.thet0x = ModCrssVoce.thet1x;
                }

                // --- Assign parameters to each system in the mode ---
                for (int is = 1; is <= CommonGlobals.nsm[im]; is++) {
                    CommonGlobals.tau0[isys] = ModCrssVoce.tau0x;
                    CommonGlobals.tau1[isys] = ModCrssVoce.tau1x;
                    CommonGlobals.thet0[isys] = ModCrssVoce.thet0x;
                    CommonGlobals.thet1[isys] = ModCrssVoce.thet1x;

                    int jsys = 1;
                    for (int jm = 1; jm <= CommonGlobals.nmodes; jm++) {
                        for (int js = 1; js <= CommonGlobals.nsm[jm]; js++) {
                            CommonGlobals.h[isys][jsys] = ModCrssVoce.hlatex[im][jm];
                            jsys++;
                        }
                    }
                    CommonGlobals.h[isys][isys] = ModCrssVoce.hselfx[im];
                    isys++;
                }
            } // end loop over modes
            
            // NOTE: We do NOT close the scanner here.
            // The main program must close unit 1.
            
        } // end ioption == 0

        // ioption 1: Initialize tau for all grains
        if (ioption == 1) {
            for (int ng_loop = 1; ng_loop <= CommonGlobals.ngrain; ng_loop++) {
                CommonGlobals.gamtot[ng_loop] = 0.0;
                for (int is = 1; is <= CommonGlobals.nsys; is++) {
                    CommonGlobals.tau[is][ng_loop] = CommonGlobals.tau0[is];
                }
            }
        }

        // ioption 2: Calculate hardening matrix 'hd' for grain 'ng'
        if (ioption == 2) {
            for (int n1 = 1; n1 <= CommonGlobals.nsys; n1++) {
                ModCrssVoce.voce = CommonGlobals.thet1[n1];
                if (CommonGlobals.tau1[n1] > 0.001 * CommonGlobals.tau0[n1]) {
                    double thet0x = CommonGlobals.thet0[n1];
                    double thet1x = CommonGlobals.thet1[n1];
                    ModCrssVoce.fact = CommonGlobals.gamtot[ng] * thet0x / CommonGlobals.tau1[n1];
                    ModCrssVoce.voce += (thet0x - thet1x + thet1x * ModCrssVoce.fact) * Math.exp(-ModCrssVoce.fact);
                }
                for (int ns2 = 1; ns2 <= CommonGlobals.nact[ng]; ns2++) {
                    int n2 = CommonGlobals.iact[ns2][ng];
                    CommonGlobals.hd[n1][n2] = ModCrssVoce.voce * CommonGlobals.h[n1][n2];
                }
            }
        }
    }
    
    /**
     * Rotates a 4th-order stiffness tensor into the ellipsoid coordinate system,
     * calls Eshelby, and rotates the resulting Eshelby tensors back.
     * Full translation of Fortran SUBROUTINE stiffness_rotation.
     *
     * @param stiffness (In) The 4th-order stiffness tensor (sample frame).
     * @param rotb      (In) The 3x3 rotation matrix (ellipsoid -> sample).
     * @param elipaxis  (In) The 3 ellipsoid axis lengths.
     * @param es4       (Out) The symmetric Eshelby tensor (sample frame).
     * @param eas4      (Out) The antisymmetric Eshelby tensor (sample frame).
     */
    public static void stiffness_rotation(double[][][][] stiffness, double[][] rotb, double[] elipaxis,
                                        double[][][][] es4, double[][][][] eas4) {
        
        // --- 1. Rotate STIFFNESS from Sample to Ellipsoid axes ---
        // ASS4GA = R_transpose * R_transpose * STIFFNESS * R * R
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                for (int m = 1; m <= 3; m++) {
                    for (int n = 1; n <= 3; n++) {
                        double DUMMY = 0.0;
                        for (int i1 = 1; i1 <= 3; i1++) {
                            for (int j1 = 1; j1 <= 3; j1++) {
                                for (int m1 = 1; m1 <= 3; m1++) {
                                    for (int n1 = 1; n1 <= 3; n1++) {
                                        // ROTB(I1,I) is R_i1,i (transpose)
                                        DUMMY += rotb[i1][i] * rotb[j1][j] * rotb[m1][m] * rotb[n1][n]
                                               * stiffness[i1][j1][m1][n1];
                                    }
                                }
                            }
                        }
                        ModStiffnessRotation.ASS4GA[i][j][m][n] = DUMMY;
                    }
                }
            }
        }

        // --- 2. Calculate Eshelby tensors in Ellipsoid axes ---
        CalculationRoutines.eshelbyb(elipaxis, ModStiffnessRotation.ASS4GA, 0.0,
                                     ModStiffnessRotation.E4GA, ModStiffnessRotation.AUX3333, 1);

        // --- 3. Rotate Eshelby tensors (E4GA, AUX3333) from Ellipsoid to Sample axes ---
        // ES4 = R * R * E4GA * R_transpose * R_transpose
        for (int i = 1; i <= 3; i++) {
            for (int j = 1; j <= 3; j++) {
                for (int m = 1; m <= 3; m++) {
                    for (int n = 1; n <= 3; n++) {
                        double DUMMYE_S = 0.0;
                        double DUMMYE_A = 0.0;
                        for (int i1 = 1; i1 <= 3; i1++) {
                            for (int j1 = 1; j1 <= 3; j1++) {
                                for (int m1 = 1; m1 <= 3; m1++) {
                                    for (int n1 = 1; n1 <= 3; n1++) {
                                        double rotFactor = rotb[i][i1] * rotb[j][j1] * rotb[m][m1] * rotb[n][n1];
                                        DUMMYE_S += rotFactor * ModStiffnessRotation.E4GA[i1][j1][m1][n1];
                                        DUMMYE_A += rotFactor * ModStiffnessRotation.AUX3333[i1][j1][m1][n1];
                                    }
                                }
                            }
                        }
                        es4[i][j][m][n] = DUMMYE_S;
                        eas4[i][j][m][n] = DUMMYE_A;
                    }
                }
            }
        }
    }
    
    /**
     * Applies co-rotational stress updates (Jaumann derivative)
     * to both grain-level and sample-level stresses.
     * Translation of Fortran SUBROUTINE COROTATION.
     *
     * @param istep The current step number.
     * @param step  The step increment.
     */
    public static void corotation(int istep, double step) {

        if (istep != 0) {
            
            // --- 1. Apply to all grains (stcs) ---
            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                
                // Get 6-vec stress and convert to 3x3 tensor
                for (int i = 1; i <= 6; i++) {
                  ModCorotation.stcs6[i] = CommonGlobals.stcs[i][ng];
                }
                TensorUtils.voigt(ModCorotation.stcs6, ModCorotation.stcs33, ModCorotation.aux66, ModCorotation.aux3333, 1);

                // Calculate Jaumann rate: tmp33 = (omegag*stcs33 - stcs33*omegag^T) * step
                // Note: Fortran code is (W*S - S*W^T)
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                      ModCorotation.tmp33[i][j] = 0.0;
                        for (int k = 1; k <= 3; k++) {
                          ModCorotation.tmp33[i][j] += (CommonGlobals.omegag[i][k][ng] * ModCorotation.stcs33[k][j]
                                         - CommonGlobals.omegag[k][j][ng] * ModCorotation.stcs33[i][k]) * step;
                        }
                    }
                }
                
                // Convert 3x3 rate back to 6-vec
                TensorUtils.voigt(ModCorotation.tmp6, ModCorotation.tmp33, ModCorotation.aux66, ModCorotation.aux3333, 2);
                
                // Update grain stress
                for (int i = 1; i <= 6; i++) {
                    CommonGlobals.stcs[i][ng] += ModCorotation.tmp6[i];
                }
            } // end loop over grains

            // --- 2. Apply to macroscopic sample stress (stss) ---
            for (int i = 1; i <= 6; i++) {
              ModCorotation.stss6[i] = CommonGlobals.stss[i];
            }
            TensorUtils.voigt(ModCorotation.stss6, ModCorotation.stss33, ModCorotation.aux66, ModCorotation.aux3333, 1);

            // Calculate Jaumann rate: tmps33 = (omegabcr*stss33 - stss33*omegabcr^T) * step
            for (int i = 1; i <= 3; i++) {
                for (int j = 1; j <= 3; j++) {
                  ModCorotation.tmps33[i][j] = 0.0;
                    for (int k = 1; k <= 3; k++) {
                      ModCorotation.tmps33[i][j] += (CommonGlobals.omegabcr[i][k] * ModCorotation.stss33[k][j]
                                      - CommonGlobals.omegabcr[k][j] * ModCorotation.stss33[i][k]) * step;
                    }
                }
            }
            
            // Convert 3x3 rate back to 6-vec
            TensorUtils.voigt(ModCorotation.tmps6, ModCorotation.tmps33, ModCorotation.aux66, ModCorotation.aux3333, 2);
            
            // Update sample stress
            for (int i = 1; i <= 6; i++) {
                CommonGlobals.stss[i] += ModCorotation.tmps6[i];
            }
        }
    }
    
    
        
/**
     * Calculates stiffness moduli (ccc2) for Zirconium as a function of temperature.
     * Translation of Fortran SUBROUTINE zirconium.
     *
     * <p>Populates CommonGlobals.ccc2 and CommonGlobals.scc2.
     *
     * @param temp The temperature.
     */
    public static void zirconium(double temp) {
        
        final double C_REF = 1000.0; // Sets units of stiffness
        double tempf = temp + CommonGlobals.deltemp;
        
        // Coefficients for ZIRCONIUM from Fisher & Renken (1964)
        // NOTE: The Fortran code "1.447d0-2.d0*tempf**2" is translated literally as
        // (1.447 - (2.0 * Math.pow(tempf, 2))), as per Fortran operator precedence.
        
        CommonGlobals.ccc2[1][1] = (159430.0 - 58.133 * tempf + 1.447 - 2.0 * Math.pow(tempf, 2)
                                    - 4.099 - 6.0 * Math.pow(tempf, 3)) / C_REF;
        CommonGlobals.ccc2[2][2] = CommonGlobals.ccc2[1][1];
        
        CommonGlobals.ccc2[1][2] = (61357.0 + 49.009 * tempf - 4.1198 - 2.0 * Math.pow(tempf, 2)
                                    + 1.396 - 5.0 * Math.pow(tempf, 3)) / C_REF;
        CommonGlobals.ccc2[2][1] = CommonGlobals.ccc2[1][2];
        
        CommonGlobals.ccc2[1][3] = (64912.0 + 1.8131 - 2.0 * tempf + 4.4831 - 3.0 * Math.pow(tempf, 2)
                                    - 3.704 - 6.0 * Math.pow(tempf, 3)) / C_REF;
        CommonGlobals.ccc2[2][3] = CommonGlobals.ccc2[1][3];
        CommonGlobals.ccc2[3][1] = CommonGlobals.ccc2[1][3];
        CommonGlobals.ccc2[3][2] = CommonGlobals.ccc2[1][3];
        
        CommonGlobals.ccc2[3][3] = (174080.0 - 30.996 * tempf - 1.3754 - 3.0 * Math.pow(tempf, 2)
                                    - 6.997 - 9.0 * Math.pow(tempf, 3)) / C_REF;
        
        CommonGlobals.ccc2[4][4] = (37290.0 - 20.79 * tempf + 1.1433 - 2.0 * Math.pow(tempf, 2)
                                    - 5.0173 - 6.0 * Math.pow(tempf, 3)) / C_REF;
        CommonGlobals.ccc2[5][5] = CommonGlobals.ccc2[4][4];
        
        CommonGlobals.ccc2[6][6] = (CommonGlobals.ccc2[1][1] - CommonGlobals.ccc2[1][2]) * 0.5;
        
        // Calculate the compliance matrix (scc2) by inverting the stiffness matrix (ccc2)
        TensorUtils.invten(CommonGlobals.ccc2, CommonGlobals.scc2);
    }

    /**
     * Translation of SUBROUTINE CRSS_disloc_dens.
     * Manages hardening law based on dislocation density.
     *
     * @param ioption Control flag:
     * 0 = Read params, calc shear moduli
     * 1 = Initialize TAU & RHO for grain ng
     * 2 = Calculate dRHO/dGamma and dTAU/dGamma (hardening matrix)
     * 3 = Calculate dRHO/dGamma and update RHO
     * @param ng      The grain index (assumed 1-based)
     */
    public static void crssDislocDens(int ioption, int ng) {

        // ioption 0: Read parameters
        if (ioption == 0) {
            
            // Allocate module arrays (using 1-based indexing size)
            ModCrssDislocDens.Drho_for_Dgamma = new double[CommonGlobals.NSLS + 1];
            ModCrssDislocDens.Drho_deb_Dgamma = new double[CommonGlobals.NSLS + 1];

            // Local arrays for voigt stub
            double[] aux6 = new double[7];
            double[][] aux33 = new double[4][4];
            
            // Call voigt to convert ccc2 (6x6) to ccc4 (3x3x3x3)
            // Using ESCR4 as the 4D tensor, as it has the right dimensions
            TensorUtils.voigt(aux6, aux33, CommonGlobals.ccc2, CommonGlobals.ESCR4, 3);

            // Calculate elastic shear moduli
            for (int im = 1; im <= CommonGlobals.nmodes; im++) {
                CommonGlobals.shearmod[im] = 0.0;
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                        for (int k = 1; k <= 3; k++) {
                            for (int l = 1; l <= 3; l++) {
                                // Note: Fortran uses mc2(..., 1) for all modes, not mc2(..., is)
                                CommonGlobals.shearmod[im] += CommonGlobals.mc2[i][j][1]
                                                          * CommonGlobals.ESCR4[i][j][k][l]
                                                          * CommonGlobals.mc2[k][l][1];
                            }
                        }
                    }
                }

                if (IOUtils.writer11 != null) {
                    IOUtils.writer11.println();
                    IOUtils.writer11.printf("   mode%4d%n", im);
                    IOUtils.writer11.printf("   elastic shear modulus%12.5e%n", CommonGlobals.shearmod[im]);
                    IOUtils.writer11.println();
                }
            }

            // Initialize TLATENT
            for (int im = 1; im <= CommonGlobals.NMOD; im++) {
                for (int jm = 1; jm <= CommonGlobals.NMOD; jm++) {
                    CommonGlobals.TLATENT[im][jm] = 0.0;
                }
            }

            // --- Read parameters from unit 1 ---
            if (IOUtils.scanner1 == null) {
                System.err.println("Error in crssDislocDens: IOUtils.scanner1 is not initialized!");
                return;
            }
            
            Scanner s = IOUtils.scanner1;
            String[] line;

            s.nextLine(); // Skip comment line
            line = s.nextLine().trim().split("\\s+");
            CommonGlobals.chi_inter = Double.parseDouble(line[0]);
            CommonGlobals.q_rate = Double.parseDouble(line[1]);
            CommonGlobals.edot_macro = Double.parseDouble(line[2]);

            for (int is = 1; is <= CommonGlobals.nslmod; is++) {
                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.BURG[is] = Double.parseDouble(line[0]);
                CommonGlobals.ACTENER[is] = Double.parseDouble(line[1]);

//                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.aK1[is] = Double.parseDouble(line[0]);
                CommonGlobals.DRAG[is] = Double.parseDouble(line[1]);

//                s.nextLine(); // Skip comment line
                CommonGlobals.edot_zero[is] = Double.parseDouble(s.nextLine().trim().split("\\s+")[0]);

//                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.rho_ini_for[is] = Double.parseDouble(line[0]);
                CommonGlobals.rho_ini_deb[is] = Double.parseDouble(line[1]);

//                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.tau0_mode_a[is] = Double.parseDouble(line[0]);
                CommonGlobals.tau0_mode_b[is] = Double.parseDouble(line[1]);
                CommonGlobals.tau0_mode_c[is] = Double.parseDouble(line[2]);

//                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                for (int mo = 1; mo <= CommonGlobals.ntwmod; mo++) {
                    CommonGlobals.TLATENT[is][mo] = Double.parseDouble(line[mo - 1]);
                }

//                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.HPK0[is] = Double.parseDouble(line[0]);
                CommonGlobals.HPK1[is] = Double.parseDouble(line[1]);
                CommonGlobals.HPK2[is] = Double.parseDouble(line[2]);

//                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.a_deb_a[is] = Double.parseDouble(line[0]);
                CommonGlobals.a_deb_b[is] = Double.parseDouble(line[1]);
                CommonGlobals.a_deb_c[is] = Double.parseDouble(line[2]);
            }

            for (int it = 1; it <= CommonGlobals.ntwmod; it++) {
                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.tau_crit_a[it] = Double.parseDouble(line[0]);
                CommonGlobals.tau_crit_b[it] = Double.parseDouble(line[1]);
                CommonGlobals.tau_crit_c[it] = Double.parseDouble(line[2]);

//                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.tau_prop_a[it] = Double.parseDouble(line[0]);
                CommonGlobals.tau_prop_b[it] = Double.parseDouble(line[1]);
                CommonGlobals.tau_prop_c[it] = Double.parseDouble(line[2]);

//                s.nextLine(); // Skip comment line
                CommonGlobals.burg_tw[it] = Double.parseDouble(s.nextLine().trim().split("\\s+")[0]);

                int itt = it + CommonGlobals.nslmod;
//                s.nextLine(); // Skip comment line
                line = s.nextLine().trim().split("\\s+");
                CommonGlobals.TwinFrac[itt] = Double.parseDouble(line[0]);
                CommonGlobals.TwinCRSS[itt] = Double.parseDouble(line[1]);
            }

        } // end ioption == 0

        // ioption 1: Initialization of CRSS tau_0 for the first step
        if (ioption == 1) {
            CommonGlobals.gamtot[ng] = 0.0;
            int is = 0;
            // First, consider the slip modes
            for (int imo = 1; imo <= CommonGlobals.nslmod; imo++) {
                for (int isy = 1; isy <= CommonGlobals.nsm[imo]; isy++) {
                    is++;
                    // Defines the initial critical stress
                    CommonGlobals.tau0_mode[imo] = CommonGlobals.tau0_mode_a[imo]
                        + CommonGlobals.tau0_mode_b[imo] * Math.exp(-CommonGlobals.temp_s / CommonGlobals.tau0_mode_c[imo]);

                    // Defines the initial dislocation densities
                    CommonGlobals.rho_for[is][ng] = CommonGlobals.rho_ini_for[imo];
                    CommonGlobals.rho_deb[ng] = CommonGlobals.rho_ini_deb[imo];

                    // Defines the yield point for the slip mode
                    CommonGlobals.tau[is][ng] = CommonGlobals.tau0_mode[imo]
                        + CommonGlobals.chi_inter * CommonGlobals.BURG[imo] * CommonGlobals.shearmod[imo]
                        * Math.sqrt(CommonGlobals.rho_for[is][ng]);

                    // This is A in equation 3.15
                    CommonGlobals.a_deb[imo] = CommonGlobals.a_deb_a[imo]
                        + CommonGlobals.a_deb_b[imo] * Math.log(1.0 + CommonGlobals.temp_s / CommonGlobals.a_deb_c[imo]);
                } // isy
            } // imo (slip)

            // Consider the twin modes
            for (int imo = 1; imo <= CommonGlobals.ntwmod; imo++) {
                for (int isy = 1; isy <= CommonGlobals.nsm[imo + CommonGlobals.nslmod]; isy++) {
                    is++;
                    CommonGlobals.tau[is][ng] = 0.0;
                    int js = 0;
                    for (int jmo = 1; jmo <= CommonGlobals.nslmod; jmo++) {
                        for (int jsy = 1; jsy <= CommonGlobals.nsm[jmo]; jsy++) {
                            js++;
                            // Equation 3.28
                            CommonGlobals.tau[is][ng] += CommonGlobals.shearmod[imo] * CommonGlobals.burg_tw[imo]
                                * CommonGlobals.BURG[jmo] * CommonGlobals.TLATENT[jmo][imo]
                                * CommonGlobals.rho_for[js][ng];
                        }
                    }
                    // Equation 3.26
                    CommonGlobals.tau[is][ng] += CommonGlobals.tau_prop_a[imo]
                        + CommonGlobals.tau_prop_b[imo] * Math.exp(-CommonGlobals.temp_s / CommonGlobals.tau_prop_c[imo]);
                } // isy
            } // imo (twin)

        } // end ioption == 1

        // ioption 2 or 3: Calculates K1 and K2
        if (ioption == 2 || ioption == 3) {
            
            for (int imo = 1; imo <= CommonGlobals.nslmod; imo++) {
                ModCrssDislocDens.Drho_deb_Dgamma[imo] = 0.0;
                ModCrssDislocDens.Drho_for_Dgamma[imo] = 0.0;
            }

            int is = 0;
            for (int imo = 1; imo <= CommonGlobals.nslmod; imo++) {
                // Equation 3.12
                double aK2 = (CommonGlobals.aK1[imo] * CommonGlobals.chi_inter * CommonGlobals.BURG[imo] / CommonGlobals.ACTENER[imo])
                           * (1.0 - (CommonConstants.BOLTZ * CommonGlobals.temp_s
                                     / (CommonGlobals.DRAG[imo] * 1.0e6 * Math.pow(CommonGlobals.BURG[imo], 3)))
                                  * Math.log(CommonGlobals.edot_macro / CommonGlobals.edot_zero[imo]));

                for (int i = 1; i <= CommonGlobals.nsm[imo]; i++) {
                    is++;
                    // Equation 3.6
                    ModCrssDislocDens.Drho_for_Dgamma[is] = CommonGlobals.aK1[imo] * Math.sqrt(CommonGlobals.rho_for[is][ng])
                                                         - aK2 * CommonGlobals.rho_for[is][ng];
                    // Equation 3.14 and 3.15
                    ModCrssDislocDens.Drho_deb_Dgamma[is] = aK2 * CommonGlobals.rho_for[is][ng] * CommonGlobals.BURG[imo]
                                                         * Math.sqrt(CommonGlobals.rho_deb[ng]) * CommonGlobals.a_deb[imo]
                                                         * CommonGlobals.q_rate;

                    // Limit change to be non-negative
                    if (ModCrssDislocDens.Drho_for_Dgamma[is] < 0.0) {
                        ModCrssDislocDens.Drho_for_Dgamma[is] = 0.0;
                    }
                    if (ModCrssDislocDens.Drho_deb_Dgamma[is] < 0.0) {
                        ModCrssDislocDens.Drho_deb_Dgamma[is] = 0.0;
                    }
                } // i
            } // imo
            
        } // end ioption == 2 or 3

        // ioption 2: Define the hardening matrix
        if (ioption == 2) {
            
            for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                int n1 = CommonGlobals.iact[ns1][ng];
                int imode1 = CommonGlobals.iSysMode[n1];

                for (int ns2 = 1; ns2 <= CommonGlobals.nact[ng]; ns2++) {
                    int n2 = CommonGlobals.iact[ns2][ng];
                    int imode2 = CommonGlobals.iSysMode[n2];
                    CommonGlobals.hd[n1][n2] = 0.0;

                    if (CommonGlobals.iTwinSys[n1] != 1 && CommonGlobals.iTwinSys[n2] != 1) {
                        // SLIP:SLIP
                        // Derivative of 3.19
                        CommonGlobals.hd[n1][n2] += ModCrssDislocDens.Drho_for_Dgamma[n1] * CommonGlobals.chi_inter
                            * CommonGlobals.BURG[imode1] * CommonGlobals.shearmod[imode1]
                            / (2.0 * Math.sqrt(CommonGlobals.rho_for[n1][ng]));

                        // Derivative of 3.20
                        CommonGlobals.hd[n1][n2] -= 0.086 * CommonGlobals.BURG[imode1] * CommonGlobals.shearmod[imode1]
                            * (Math.log(CommonGlobals.BURG[imode1] * Math.sqrt(CommonGlobals.rho_deb[ng])) + 1.0)
                            * ModCrssDislocDens.Drho_deb_Dgamma[n2] / (2.0 * Math.sqrt(CommonGlobals.rho_deb[ng]));

                    } else if (CommonGlobals.iTwinSys[n1] == 1 && CommonGlobals.iTwinSys[n2] != 1) {
                        // TWIN:SLIP
                        // Derivative of 3.28
                        CommonGlobals.hd[n1][n2] = CommonGlobals.shearmod[imode1]
                            * CommonGlobals.burg_tw[imode1 - CommonGlobals.nslmod] * CommonGlobals.BURG[imode2]
                            * CommonGlobals.TLATENT[imode2][imode1 - CommonGlobals.nslmod]
                            * ModCrssDislocDens.Drho_for_Dgamma[n2];

                    } else if (CommonGlobals.iTwinSys[n1] != 1 && CommonGlobals.iTwinSys[n2] == 1) {
                        // SLIP:TWIN
                        // Derivative of 3.28
                        CommonGlobals.hd[n1][n2] = CommonGlobals.shearmod[imode1]
                            * CommonGlobals.burg_tw[imode2 - CommonGlobals.nslmod] * CommonGlobals.BURG[imode1]
                            * CommonGlobals.TLATENT[imode1][imode2 - CommonGlobals.nslmod]
                            * ModCrssDislocDens.Drho_for_Dgamma[n1];

                    } else if (CommonGlobals.iTwinSys[n1] == 1 && CommonGlobals.iTwinSys[n2] == 1) {
                        // TWIN:TWIN
                        // No self-hardening or TWIN:TWIN hardening
                    }
                } // ns2
            } // ns1

            // Ensure all components of the hardening matrix are >= 1.0
            for (int ns1 = 1; ns1 <= CommonGlobals.nact[ng]; ns1++) {
                int n1 = CommonGlobals.iact[ns1][ng];
                for (int ns2 = 1; ns2 <= CommonGlobals.nact[ng]; ns2++) {
                    int n2 = CommonGlobals.iact[ns2][ng];
                    if (CommonGlobals.hd[n1][n2] < 1.0) {
                        CommonGlobals.hd[n1][n2] = 1.0;
                    }
                }
            }
            
        } // end ioption == 2

        // ioption 3: Update the dislocation densities
        if (ioption == 3) {
            
            for (int imo = 1; imo <= CommonGlobals.nslmod; imo++) {
                for (int i = 1; i <= CommonGlobals.nsm[imo]; i++) {
                    int is = CommonGlobals.mode_slip[imo][i];
                    
                    CommonGlobals.rho_for[is][ng] += ModCrssDislocDens.Drho_for_Dgamma[is] * CommonGlobals.gamd[is][ng];
                    // Force rho_for to never get smaller than initial value
                    if (CommonGlobals.rho_for[is][ng] < CommonGlobals.rho_ini_for[imo]) {
                        CommonGlobals.rho_for[is][ng] = CommonGlobals.rho_ini_for[imo];
                    }

                    CommonGlobals.rho_deb[ng] += ModCrssDislocDens.Drho_deb_Dgamma[is] * CommonGlobals.gamd[is][ng];
                    // Force rho_deb to never get smaller than initial value
                    if (CommonGlobals.rho_deb[ng] < CommonGlobals.rho_ini_deb[imo]) {
                        CommonGlobals.rho_deb[ng] = CommonGlobals.rho_ini_deb[imo];
                    }
                } // i
            } // imo
            
        } // end ioption == 3

    } // end crssDislocDens

    /**
     * Translation of SUBROUTINE pressure_cij.
     * Calculates stiffness moduli in GPa as a function of pressure.
     *
     * @param pressure The pressure.
     */
    public static void pressureCij(double pressure) {
        
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                CommonGlobals.ccc2[i][j] = CommonGlobals.ccc2p0[i][j] + pressure * CommonGlobals.ccc2dp[i][j];
            }
        }
        
        // Invert the stiffness matrix to get the compliance matrix
        TensorUtils.invten(CommonGlobals.ccc2, CommonGlobals.scc2);
    }
    
}