package it.unitn.ing.rista.diffr.rsa.epsc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Locale;
import java.util.Scanner;

// Import all static classes
//import static CommonGlobals.*;
//import static your.package.ModScNew.*;
//import static your.package.CalculationRoutines.*;
//import static your.package.TensorUtils.*;
// ... (and all other module/helper classes) ...

/**
 * Main program translation for EPSC4.
 *
 * This class contains the main() method which controls the entire
 * simulation flow, reads the master input file, and calls all
 * calculation subroutines in order.
 */
public class EPSC4 {

    // DATA Number/'0','1','2','3','4','5','6','7','8','9'/
    private static final String[] NUMBER_CHARS = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"};

    public static void run(String path, String insName) {

        // --- Local variables from PROGRAM block ---
        String prosa;
        String filecrys = "", filesamp = "", fileprev = "", filediff = "";
        String[] fileproc = null; // Allocatable
      IOUtils.setPath(path);

        double[][] e2 = new double[7][7];
        double[] aux6 = new double[7];
        double[] etss_ini = new double[7];
        double[] stss_ini = new double[7];
        
        // Dummy arrays for Voigt calls
        double[][] C2_dummy = new double[7][7];
        double[][][][] C4_dummy = new double[4][4][4][4];

        double step = 1.0, temp = 0.0, time_acum = 0.0, xref = 0.0;
        int i_bc_mode = 0, i_prev_proc = 0, i_ref_et = 0, i_ref_st = 0;
        int i_strpf = 0, i_temp_cij = 0, icrysym = 0, interaction = 0;
        int istep = 0, it_grain = 0, iverify = 0, liter = 0;
        int nFile = 0, nout_old = 0, nskip = 0, nproc = 0;
        
        // iMaxTwinLevel = 2 (Set in CommonGlobals or here)
        CommonGlobals.iMaxTwinLevel = 2;
        // MaxTwins = 0 (Initialized in CommonGlobals)
      CommonGlobals.itwinning = 0; // set flag for absence/presence of twins
      CommonGlobals.iwrite9 = 0; // deactivate epsc9.out
        int idummy = 0;

        // --- 1. Initialize I/O and Global Constants ---
        IOUtils.initializeIO();
        CalculationRoutines.initializeGlobals();

        // --- 2. Call Eshelby Initialization ---
        CalculationRoutines.eshelbyb(CommonGlobals.axisph[0], ModScNew.ass4, 0.0, ModScNew.esim4, CommonGlobals.ESCR4, 0);

        // --- 3. Open and Read Master Input File (epsc4.in) ---
        Scanner s = null;
        try {
            File inputFile = new File(path + insName);
            s = new Scanner(inputFile);
            s.useLocale(Locale.US);
            // Assign to static field so data_crystal and crssVoce can use it
            IOUtils.scanner1 = s; 

            // Dump file to writer 11
            if (IOUtils.writer11 != null) {
                IOUtils.writer11.println();
                IOUtils.writer11.println(" ****** SIMULATION MASTER FILE *******");
                Scanner dumpScanner = new Scanner(inputFile);
                while (dumpScanner.hasNextLine()) {
                    IOUtils.writer11.println(dumpScanner.nextLine());
                }
                dumpScanner.close();
                IOUtils.writer11.println(" ****** END OF SIMULATION MASTER FILE ******");
                IOUtils.writer11.println();
            }

            prosa = s.nextLine().trim(); // simulation label
            
            // Write headers
            System.out.println("******************************************************************************");
            System.out.println("************** SELF-CONSISTENT THERMO-ELASTOPLASTIC CODE \"EPSC\" **************");
            System.out.println(prosa);
            System.out.println("******************************************************************************");

            for (PrintWriter writer : new PrintWriter[]{IOUtils.writer11, IOUtils.writer19}) {
                if (writer != null) {
                    writer.println("******************************************************************************");
                    writer.println(prosa);
                    writer.println("******************************************************************************");
                }
            }
            
            prosa = s.nextLine(); // Skip comment
            CommonGlobals.ishape = s.nextInt(); s.nextLine(); // line 3
            String[] line = s.nextLine().trim().split("\\s+"); // line 4
            for (int i = 1; i <= 3; i++) CommonGlobals.axis[i] = Double.parseDouble(line[i - 1]);
            line = s.nextLine().trim().split("\\s+"); // line 5
            for (int i = 1; i <= 3; i++) CommonGlobals.eulerph[i] = Double.parseDouble(line[i - 1]);
            
            prosa = s.nextLine(); // Skip comment
            filesamp = path + s.nextLine().trim(); // line 7
          CommonGlobals.irot = s.nextInt(); s.nextLine(); // line 8
            
            prosa = s.nextLine(); // Skip comment
            filecrys = path + s.nextLine().trim(); // line 10
            
            prosa = s.nextLine(); // Skip comment (Precision)
          CommonGlobals.itmax_mod = s.nextInt(); s.nextLine();
          CommonGlobals.error_mod = s.nextDouble(); s.nextLine();
          CommonGlobals.itmax_grain = s.nextInt(); s.nextLine();
            
            prosa = s.nextLine(); // Skip comment (Prev. proc)
            i_prev_proc = s.nextInt(); s.nextLine();
            fileprev = path + s.nextLine().trim();
          CommonGlobals.itexskip = s.nextInt(); s.nextLine();
            
            int i_diff_dir = s.nextInt(); s.nextLine();
            filediff = path + s.nextLine().trim();
            
            i_strpf = s.nextInt(); s.nextLine();
            
            prosa = s.nextLine(); // Skip comment (NProc)
            nproc = s.nextInt(); s.nextLine();
            
            CommonGlobals.NPROCX = nproc;
            fileproc = new String[nproc + 1]; // 1-based
            // (Dimension check is handled by Java array size)
            
            prosa = s.nextLine(); // Skip comment (Proc files)
            for (int n = 1; n <= nproc; n++) {
                fileproc[n] = path + s.nextLine().trim();
            }

            // --- 4. Write Headers to Output Files ---
            if (IOUtils.writer12 != null) IOUtils.writer12.println("FILE FOR GRAINS STATE AND PLASTIC ACTIVITY");
            if (IOUtils.writer13 != null) IOUtils.writer13.println("COMPONENTS 11 22 33 OF SAMPLE STRAIN, STRESS, ELASTIC STRAIN and AVACS");
            if (IOUtils.writer14 != null) IOUtils.writer14.println("FINAL SAMPLE AND GRAINS STATE");
            if (IOUtils.writer15 != null) {
                IOUtils.writer15.println("EVOLUTION OF SAMPLE STRAIN RATE, STRAIN (and devs)");
                IOUtils.writer15.println("ETRSS - ETRDEV - CommonGlobals.etss - ETDEV (6 components each):");
            }
            if (IOUtils.writer16 != null) {
                IOUtils.writer16.println("EVOLUTION OF SAMPLE STRESS RATE, STRESS (and devs)");
                IOUtils.writer16.println("STRSS - STRDEV - STSS - STDEV (6 components each):");
            }
            if (IOUtils.writer17 != null) {
                IOUtils.writer17.println("RELATIVE ACTIVITY IN EACH MODE AND AVACS vs STRAIN");
                IOUtils.writer17.println("SLIP AND TWINNING ACTIVITY STATISTIC:");
            }
            if (IOUtils.writer18 != null) {
                IOUtils.writer18.println("EQUIVALENT STATES");
                IOUtils.writer18.println("EQ ET - EQ PL ET - EQ ST - EQ ETR - EQ PL ETR - EQ STR - VOLUME - " +
                                         "PRESSURE - WTOT - WPLTOT - STET - STETAV");
            }
            if (IOUtils.writer19 != null) IOUtils.writer19.println("EVOLUTION OF INTERNAL STRAINS");

            for (PrintWriter writer : new PrintWriter[]{IOUtils.writer11, IOUtils.writer12, IOUtils.writer13,
                IOUtils.writer14, IOUtils.writer15, IOUtils.writer16, IOUtils.writer17,
                IOUtils.writer18, IOUtils.writer19}) {
                if (writer != null) writer.println("******************************************************************************");
            }

            if (IOUtils.writer13 != null) {
                IOUtils.writer13.printf(Locale.US, "%8s%8s%8s%8s%8s%8s%11s%8s%8s%8s%8s%8s%8s%9s%8s%6s%8s%6s%8s%6s%8s%8s%8s%8s%8s%n",
                    "et11", "et22", "et33", "et23", "et13", "et12", "st11", "st22", "st33", "st23", "st13", "st12",
                    "etel11", "etel22", "etel33", "etth11", "etth22", "etth33", "avacs", "rho_avg", "rho_for", "rho_deb");
            }
            
            // --- 5. Load Data Files ---
            // These calls use the still-open IOUtils.scanner1 (unit 1)
            IntHolder icrysymHolder = new IntHolder(0);
          CalculationRoutines.dataCrystal(filecrys, icrysymHolder);
            icrysym = icrysymHolder.value;

            s = IOUtils.scanner1;
            s.nextLine();
            CommonGlobals.kCL = s.nextInt();
            s.nextLine();
            if (CommonGlobals.kCL == 0) CalculationRoutines.crssVoce(0, idummy);
            if (CommonGlobals.kCL == 2) CalculationRoutines.crssDislocDens(0, idummy);

            // Now we are done with unit 1 (epsc4.in)
            if (IOUtils.scanner1 != null) {
                IOUtils.scanner1.close();
                IOUtils.scanner1 = null;
            }

            // This call opens 'filesamp' on its own (and closes it)
          CalculationRoutines.dataSample(filesamp, i_prev_proc, fileprev);

            // --- 6. Initialize Output File Headers ---
            if (CommonGlobals.iTotStep == 0 && IOUtils.writer17 != null) {
                IOUtils.writer17.printf(" %-15s%n", "Ctrl");
                for (int i = 1; i <= 3; i++) IOUtils.writer17.printf(" %-15s%n", "EP" + i);
                for (int i = 1; i <= 3; i++) IOUtils.writer17.printf(" %-15s%n", "SIG" + i);
                for (int i = 1; i <= CommonGlobals.nmodes; i++) IOUtils.writer17.printf(" %-15s%n", "MODE" + i);
                IOUtils.writer17.printf(" %-15s%n", "ActAv");
                for (int i = 1; i <= CommonGlobals.nmodes; i++) IOUtils.writer17.printf(" %-15s%n", "PMODE" + i);
                IOUtils.writer17.printf(" %-15s%n", "PActAv");
                for (int i = 1; i <= CommonGlobals.nmodes; i++) IOUtils.writer17.printf(" %-15s%n", "CMODE" + i);
                IOUtils.writer17.printf(" %-15s%n", "CActAv");
                IOUtils.writer17.printf(" %-15s%n", "PTWINVF");
                IOUtils.writer17.printf(" %-15s%n", "CTWINVF");
                IOUtils.writer17.printf(" %-15s%n", "nGrain");
                IOUtils.writer17.printf(" %-15s%n", "MaxTwins");
                IOUtils.writer17.println();
            }

          int ipileup = 0;
            if (ipileup == 1) CalculationRoutines.pileup(0);

          CalculationRoutines.crToSa(1, CommonGlobals.ngrain, 0);
          CalculationRoutines.crToSa(1, CommonGlobals.ngrain, 1);
            
            if (i_diff_dir == 1) {
              CalculationRoutines.difPlanes(icrysym, filediff, 0.0, 0, 0); // iopt=0
                
                if (CommonGlobals.iTotStep == 0 && IOUtils.writer20 != null) {
                    PrintWriter out20 = IOUtils.writer20;
                    out20.printf(" %-13s%n", "Ctrl");
                    for (int i = 1; i <= 3; i++) out20.printf(" %-13s%n", "EP" + i);
                    for (int i = 1; i <= 3; i++) out20.printf(" %-13s%n", "SIG" + i);
                    for (int i = 1; i <= 3; i++) out20.printf(" %-13s%n", "EPAV" + i);
                    for (int i = 1; i <= 3; i++) out20.printf(" %-13s%n", "SIGAV" + i);
                    for (int i = 1; i <= CommonGlobals.ndiff; i++) out20.printf(" %-13s%n", String.format("DIF%03d", i));
                    for (int i = 1; i <= CommonGlobals.ndiff; i++) out20.printf(" %-13s%n", String.format("STDEV%03d", i));
                    for (int i = 1; i <= CommonGlobals.ndiff; i++) out20.printf(" %-13s%n", String.format("DEV_DIF%03d", i));
                    for (int i = 1; i <= CommonGlobals.ndiff; i++) out20.printf(" %-13s%n", String.format("DEV_STDEV%03d", i));
                    for (int i = 1; i <= CommonGlobals.ndiff; i++) {
                        out20.printf(" %-13s%n", String.format("WGT%03d", i));
                      CommonGlobals.wgtsetini[i] = CommonGlobals.wgtset[i];
                    }
                    out20.println();
                }

              CalculationRoutines.difPlanes(icrysym, filediff, 0.0, 0, 1); // iopt=1
            }
            
            if (i_strpf == 1) {
              CalculationRoutines.poleFig(icrysym, nFile, 0, filecrys);
            }

            if (i_prev_proc == 0) {
              CalculationRoutines.avModulus();
                if (IOUtils.writer11 != null) IOUtils.writer11.println("PURE ELASTIC SELFCONSISTENT PROBLEM");
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    CommonGlobals.gamtot[ng] = 0.0;
                    for (int i = 1; i <= 6; i++) {
                        for (int j = 1; j <= 6; j++) {
                            CommonGlobals.acs2[i][j][ng] = CommonGlobals.ccs2[i][j][ng];
                        }
                    }
                }
                interaction = 1;
                IntHolder iverifyHolder = new IntHolder(0);
              CalculationRoutines.scNew(0, 0, iverifyHolder, e2, interaction, 0); // iopt=0
                TensorUtils.invten(CommonGlobals.ass2, CommonGlobals.sss2);
            }

            // --- 7. Initialize State Variables ---
            for (int i = 1; i <= 6; i++) {
                CommonGlobals.etssref[i] = 0.0;
            }
            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                for (int i = 1; i <= 6; i++) {
                  CommonGlobals.stcsref[i][ng] = 0.0;
                  CommonGlobals.etelcs[i][ng] = 0.0;
                  CommonGlobals.etelhycs[i][ng] = 0.0;
                }
            }
            step = 1.0;
            time_acum = 0.0;
            int iproc = 0;
            
            AllocationRoutines.allocateForPlasticity();

            // --- 8. START MAIN PROCESS LOOP ---
            while (iproc < nproc) {
                iproc++;
              CalculationRoutines.plasticity(iproc, 0, step, temp, 0); // iopt=0 (init)
                nout_old = 0;
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    if (iproc == 1) {
                      CommonGlobals.ng_update[ng] = 0;
                        for (int ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
                          CommonGlobals.tau_update[ns1][ng] = 1.0;
                        }
                    }
                }

                // --- 8a. Load Process Data ---
                IntHolder i_temp_cij_h = new IntHolder(0);
                IntHolder i_ref_et_h = new IntHolder(0);
                IntHolder i_ref_st_h = new IntHolder(0);
                IntHolder i_bc_mode_h = new IntHolder(0);
                CalculationRoutines.dataProcess(fileproc[iproc], i_temp_cij_h, i_ref_et_h, i_ref_st_h, i_bc_mode_h);
                i_temp_cij = i_temp_cij_h.value;
                i_ref_et = i_ref_et_h.value;
                i_ref_st = i_ref_st_h.value;
                i_bc_mode = i_bc_mode_h.value;

              CalculationRoutines.loadConditions();
                
                // Decompose full strain tensor
                for (int i = 1; i <= 3; i++) {
                    for (int j = 1; j <= 3; j++) {
                      CommonGlobals.etbc_sym[i][j] = 0.5 * (CommonGlobals.fulletbc[i][j] + CommonGlobals.fulletbc[j][i]);
                      CommonGlobals.omegabc[i][j] = 0.5 * (CommonGlobals.fulletbc[i][j] - CommonGlobals.fulletbc[j][i]);
                      CommonGlobals.omegabcr[i][j] = CommonGlobals.omegabc[i][j] / CommonGlobals.nsteps;
                        if (CommonGlobals.omegabcr[i][j] > 0) {
                            System.out.println("\nYOU HAVE IMPOSED A SPIN!!\n");
                        }
                    }
                }
                TensorUtils.voigt(CommonGlobals.etbc, CommonGlobals.etbc_sym, C2_dummy, C4_dummy, 2);

                if (i_ref_et == 1) {
                    for (int i = 1; i <= 6; i++) CommonGlobals.etssref[i] = CommonGlobals.etss[i];
                } else if (i_ref_et == -1) {
                    for (int i = 1; i <= 6; i++) CommonGlobals.etssref[i] = 0.0;
                }
                
                if (i_ref_st == 1) {
                    for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                        for (int i = 1; i <= 6; i++) CommonGlobals.stcsref[i][ng] = CommonGlobals.stcs[i][ng];
                    }
                } else if (i_ref_st == -1) {
                    for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                        for (int i = 1; i <= 6; i++) CommonGlobals.stcsref[i][ng] = 0.0;
                    }
                }

                for (int i = 1; i <= 6; i++) {
                    aux6[i] = 0.0;
                    for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                        aux6[i] += CommonGlobals.etthcs[i][ng] * CommonGlobals.wgt[ng];
                    }
                }
                
                if (IOUtils.writer13 != null) {
                    IOUtils.writer13.printf(Locale.US, "%12.4e%12.4e%12.4e%12.4e%12.4e%12.4e  %12.4e%12.4e%12.4e%12.4e%12.4e%12.4e  " +
                                                       "%12.4e%12.4e%12.4e  %12.4e%12.4e%12.4e  %10.4f  %12.4e%12.4e%12.4e%n",
                                          CommonGlobals.etss[1] - CommonGlobals.etssref[1], CommonGlobals.etss[2] - CommonGlobals.etssref[2], CommonGlobals.etss[3] - CommonGlobals.etssref[3],
                                          CommonGlobals.etss[4] - CommonGlobals.etssref[4], CommonGlobals.etss[5] - CommonGlobals.etssref[5], CommonGlobals.etss[6] - CommonGlobals.etssref[6],
                        CommonGlobals.stss[1], CommonGlobals.stss[2], CommonGlobals.stss[3], CommonGlobals.stss[4], CommonGlobals.stss[5], CommonGlobals.stss[6],
                        CommonGlobals.etelss[1], CommonGlobals.etelss[2], CommonGlobals.etelss[3],
                                          aux6[1], aux6[2], aux6[3],
                        CommonGlobals.actav, CommonGlobals.rho_avg_for + CommonGlobals.rho_avg_deb, CommonGlobals.rho_avg_for, CommonGlobals.rho_avg_deb);
                }
                
                if (i_diff_dir == 1) {
                  CalculationRoutines.difPlanes(icrysym, filediff, 0.0, istep, 1);
                }
                
                // Calculate increments
                for (int i = 1; i <= 6; i++) {
                  CommonGlobals.strbc[i] = 0.0;
                    if (CommonGlobals.istbc[i] == 1) CommonGlobals.strbc[i] = (CommonGlobals.stbc[i] - CommonGlobals.stss[i]) / CommonGlobals.nsteps;
                  CommonGlobals.etrbc[i] = 0.0;
                    if (CommonGlobals.ietbc[i] == 1) CommonGlobals.etrbc[i] = (CommonGlobals.etbc[i] - CommonGlobals.etss[i]) / CommonGlobals.nsteps;
                }
                
                // Check for zero increments
                if (CommonGlobals.icvx >= 1 && CommonGlobals.icvx <= 6) {
                    if (CommonGlobals.etrbc[CommonGlobals.icvx] == 0.0) {
                        throw new RuntimeException("WARNING - Strain increment for strain control component should not be zero.");
                    }
                }
                if (CommonGlobals.icvx >= 7) {
                    if (CommonGlobals.strbc[CommonGlobals.icvx - 6] == 0.0) {
                        throw new RuntimeException("WARNING - Stress increment for stress control component should not be zero.");
                    }
                }

                temp = CommonGlobals.temp_s;
                double temp_f = CommonGlobals.temp_s + CommonGlobals.deltemp * CommonGlobals.nsteps;
                
                for (PrintWriter writer : new PrintWriter[]{IOUtils.writer11, IOUtils.writer12}) {
                    if (writer != null) {
                        writer.println("******************************************************************************");
                        writer.println("******************************************************************************");
                        writer.println("******** PROCESS data - File: " + fileproc[iproc] + "********");
                    }
                }

                for (int i = 1; i <= 6; i++) {
                    etss_ini[i] = CommonGlobals.etss[i];
                    stss_ini[i] = CommonGlobals.stss[i];
                }
                istep = 0;
                
                // --- 8b. START MAIN STEP LOOP ---
                while (istep < CommonGlobals.nsteps) {
                    
                    if (CommonGlobals.itwinning == 1 || CommonGlobals.irot == 1) {
                        // Update BCs for non-proportional paths
                        for (int i = 1; i <= 6; i++) {
                          CommonGlobals.strbc[i] = 0.0;
                            if (CommonGlobals.istbc[i] == 1) {
                                if (i_bc_mode == 1) {
                                  CommonGlobals.strbc[i] = (CommonGlobals.stbc[i] - stss_ini[i]) * (istep + 1) / CommonGlobals.nsteps + stss_ini[i] - CommonGlobals.stss[i];
                                } else {
                                  CommonGlobals.strbc[i] = (CommonGlobals.stbc[i] - 0.0) * (istep + 1) / CommonGlobals.nsteps + stss_ini[i] - CommonGlobals.stss[i];
                                }
                            }
                          CommonGlobals.etrbc[i] = 0.0;
                            if (CommonGlobals.ietbc[i] == 1) {
                                if (i_bc_mode == 1) {
                                  CommonGlobals.etrbc[i] = (CommonGlobals.etbc[i] - etss_ini[i]) * (istep + 1) / CommonGlobals.nsteps + etss_ini[i] - CommonGlobals.etss[i];
                                } else {
                                  CommonGlobals.etrbc[i] = (CommonGlobals.etbc[i] - 0.0) * (istep + 1) / CommonGlobals.nsteps + etss_ini[i] - CommonGlobals.etss[i];
                                }
                            }
                        }
                    }
                    
                    CommonGlobals.iTotStep++;
                    if (CommonGlobals.icvx == 0) xref = temp;
                    if (CommonGlobals.icvx >= 1 && CommonGlobals.icvx <= 6) xref = CommonGlobals.etss[CommonGlobals.icvx] - CommonGlobals.etssref[CommonGlobals.icvx];
                    if (CommonGlobals.icvx >= 7) xref = CommonGlobals.stss[CommonGlobals.icvx - 6];
                    
                    istep++;
                    System.out.printf(Locale.US, "STEP:%4d   CONTROL VARIABLE:%13.5e%n", istep, xref);
                    System.out.println("PREVIOUS STRESS-STRAIN IN SAMPLE:");
                    System.out.printf(Locale.US, " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                        CommonGlobals.stss[1], CommonGlobals.stss[2], CommonGlobals.stss[3], CommonGlobals.stss[4], CommonGlobals.stss[5], CommonGlobals.stss[6]);
                    System.out.printf(Locale.US, " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                                      CommonGlobals.etss[1] - CommonGlobals.etssref[1], CommonGlobals.etss[2] - CommonGlobals.etssref[2], CommonGlobals.etss[3] - CommonGlobals.etssref[3],
                                      CommonGlobals.etss[4] - CommonGlobals.etssref[4], CommonGlobals.etss[5] - CommonGlobals.etssref[5], CommonGlobals.etss[6] - CommonGlobals.etssref[6]);
                    System.out.println();
                    
                    if (IOUtils.writer12 != null) {
                        IOUtils.writer12.printf(Locale.US, "STEP:%4d   CONTROL VARIABLE:%13.5e%n", istep, xref);
                    }
                    
                    for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                        for (int isys = 1; isys <= CommonGlobals.nsys; isys++) {
                          CommonGlobals.gamd[isys][ng] = 0.0;
                        }
                    }
                    
                    if (i_temp_cij == 1) {
                      CalculationRoutines.zirconium(temp);
                      CalculationRoutines.crToSa(1, CommonGlobals.ngrain, 1);
                    }
                    if (CommonGlobals.kSM == 1.0) {
                        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                            double pressure = -(CommonGlobals.stcs[1][ng] + CommonGlobals.stcs[2][ng] + CommonGlobals.stcs[3][ng]) / 3.0;
                          CalculationRoutines.pressureCij(pressure);
                          CalculationRoutines.crToSa(ng, ng, 1);
                        }
                    }

                    // --- 8c. START SOLVER (GRAIN ITERATION) LOOP ---
                    it_grain = 0;
                    iverify = 1;
                    IntHolder iverifyHolder = new IntHolder(iverify);
                    
                    while (iverify != 0) {
                        it_grain++;
                        if (it_grain == CommonGlobals.itmax_grain) {
                            throw new RuntimeException("ABNORMAL PROGRAM STOP\n" +
                                                       "DOES NOT CONVERGE AFTER " + it_grain + " ITERATIONS OVER GRAINS SYSTEMS");
                        }
                        
                        nout_old = CalculationRoutines.gActsys(nout_old);
                      CalculationRoutines.sState();
                      CalculationRoutines.gModulus();
                        interaction = 1;
                        int iopt_sc = 2; // iopt=2
                        if (ipileup == 1) iopt_sc = 2;

                      CalculationRoutines.scNew(iopt_sc, it_grain, iverifyHolder, e2, interaction, istep);
                        iverify = iverifyHolder.value;
                        
                    } // --- End solver loop (while iverify != 0)
                    
                    // --- 8d. Update State After Convergence ---
                    for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                        if (CommonGlobals.nact[ng] != 0) {
                            for (int ns1 = 1; ns1 <= CommonGlobals.nsys; ns1++) {
                              CommonGlobals.tau[ns1][ng] += CommonGlobals.taud[ns1][ng] * step;
                                CommonGlobals.gamtot[ng] += CommonGlobals.gamd[ns1][ng] * step;
                            }
                        }
                    }
                    
                    if (CommonGlobals.kCL == 2) {
                      CommonGlobals.rho_avg_for = 0.0;
                      CommonGlobals.rho_avg_deb = 0.0;
                        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                          CalculationRoutines.crssDislocDens(3, ng);
                            for (int is = 1; is <= CommonGlobals.nslsys; is++) {
                              CommonGlobals.rho_avg_for += CommonGlobals.rho_for[is][ng] * CommonGlobals.wgt[ng];
                            }
                          CommonGlobals.rho_avg_deb += CommonGlobals.rho_deb[ng] * CommonGlobals.wgt[ng];
                        }
                    }
                    
                    if (ipileup == 1) CalculationRoutines.pileup(1);
                    
                    if (CommonGlobals.itwinning == 1) {
                      CalculationRoutines.twinningBjcl(step);
                        // Update parent grain stresses after twinning
                        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                            for (int i = 1; i <= 6; i++) aux6[i] = 0.0;
                            double dumtemp = 0.0;
                            for (int ist = 1; ist <= CommonGlobals.nsys; ist++) {
                                int j = CommonGlobals.iChildGrain[ist][ng];
                                if (j < 0) { // New child this step
                                    j = -j;
                                  CommonGlobals.iChildGrain[ist][ng] = j; // Reset flag
                                    int imo = CommonGlobals.iParentMode[j];
                                    for (int i = 1; i <= 6; i++) {
                                        aux6[i] += CommonGlobals.stcs[i][j] * CommonGlobals.TwinFrac[imo];
                                    }
                                    dumtemp += CommonGlobals.TwinFrac[imo];
                                }
                            }
                            if (dumtemp != 0.0 && dumtemp != 1.0) {
                                for (int i = 1; i <= 6; i++) {
                                  CommonGlobals.stcs[i][ng] = (CommonGlobals.stcs[i][ng] + aux6[i]) / (1.0 - dumtemp);
                                }
                            }
                        }
                        
                        // Renormalize weights
                        double totwgt = 0.0;
                        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                            totwgt += (CommonGlobals.wgt[ng] + CommonGlobals.wgtd[ng]);
                        }
                        if (Math.abs(totwgt) > 1.0e-9) {
                            for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                              CommonGlobals.wgt[ng] = (CommonGlobals.wgt[ng] + CommonGlobals.wgtd[ng]) / totwgt;
                            }
                        }
                    } // end if twinning
                    
                    // --- Write Texture File ---
                    if (CommonGlobals.itexskip != 0) {
                        nskip = (istep / CommonGlobals.itexskip) * CommonGlobals.itexskip;
                        if (istep == 1 || istep == CommonGlobals.nsteps || istep == nskip) {
                            nFile++;
                          CalculationRoutines.writeTexFile(nFile, iproc, istep);
                        }
                    } else {
                        if (istep == CommonGlobals.nsteps) {
                            nFile++;
                          CalculationRoutines.writeTexFile(nFile, iproc, istep);
                        }
                    }
                    
                    if (CommonGlobals.ishape >= 1) {
                      CalculationRoutines.updateFij(step);
                    }
                    
                    if (istep < CommonGlobals.nsteps) {
                        if (CommonGlobals.irot == 1) {
                          CalculationRoutines.updateOrientation(step, istep, 1);
                          CalculationRoutines.corotation(istep, step);
                        }
                    }
                    
                    // --- Update global stress/strain state ---
                    for (int i = 1; i <= 6; i++) {
                      CommonGlobals.stss[i] += CommonGlobals.strss[i] * step;
                        CommonGlobals.etss[i] += CommonGlobals.etrss[i] * step;
                        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                          CommonGlobals.stcs[i][ng] += CommonGlobals.strcs[i][ng] * step;
                          CommonGlobals.etcs[i][ng] += CommonGlobals.etcs[i][ng] * step;
                          CommonGlobals.etthcs[i][ng] += CommonGlobals.alfacs[i][ng] * CommonGlobals.deltemp * step;
                        }
                    }
                    
                    if (ipileup == 0) temp += CommonGlobals.deltemp;

                  CalculationRoutines.gAverage();
                    
                    if (CommonGlobals.itwinning == 1) {
                        for (int i = 1; i <= 6; i++) {
                          CommonGlobals.stss[i] = CommonGlobals.stav[i];
                            CommonGlobals.etss[i] = CommonGlobals.etav[i];
                        }
                    }
                    
                    for (int i = 1; i <= 6; i++) {
                      CommonGlobals.etelss[i] = 0.0;
                        for (int j = 1; j <= 6; j++) {
                          CommonGlobals.etelss[i] += CommonGlobals.sss2[i][j] * CommonGlobals.stss[j] * CommonGlobals.profac[j];
                        }
                    }
                    
                    for (int i = 1; i <= 6; i++) {
                        aux6[i] = 0.0;
                        for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                            aux6[i] += CommonGlobals.etthcs[i][ng] * CommonGlobals.wgt[ng];
                        }
                    }
                    
                    if (IOUtils.writer13 != null) {
                        IOUtils.writer13.printf(Locale.US, "%12.4e%12.4e%12.4e%12.4e%12.4e%12.4e  %12.4e%12.4e%12.4e%12.4e%12.4e%12.4e  " +
                                                           "%12.4e%12.4e%12.4e  %12.4e%12.4e%12.4e  %10.4f  %12.4e%12.4e%12.4e%n",
                                              CommonGlobals.etss[1] - CommonGlobals.etssref[1], CommonGlobals.etss[2] - CommonGlobals.etssref[2], CommonGlobals.etss[3] - CommonGlobals.etssref[3],
                                              CommonGlobals.etss[4] - CommonGlobals.etssref[4], CommonGlobals.etss[5] - CommonGlobals.etssref[5], CommonGlobals.etss[6] - CommonGlobals.etssref[6],
                            CommonGlobals.stss[1], CommonGlobals.stss[2], CommonGlobals.stss[3], CommonGlobals.stss[4], CommonGlobals.stss[5], CommonGlobals.stss[6],
                            CommonGlobals.etelss[1], CommonGlobals.etelss[2], CommonGlobals.etelss[3],
                                              aux6[1], aux6[2], aux6[3],
                            CommonGlobals.actav, CommonGlobals.rho_avg_for + CommonGlobals.rho_avg_deb, CommonGlobals.rho_avg_for, CommonGlobals.rho_avg_deb);
                    }

                  CalculationRoutines.plasticity(iproc, istep, step, temp, 1); // iopt=1
                    
                    if (i_diff_dir == 1) {
                      CalculationRoutines.difPlanes(icrysym, filediff, temp, istep, 1);
                    }

                  CalculationRoutines.effectiveMagnitudes(iproc, istep, step);
                    
                    System.out.println("******************************************************************************");
                    if (IOUtils.writer12 != null) IOUtils.writer12.println("******************************************************************************");
                    
                } // --- End Step Loop

              CalculationRoutines.plasticity(iproc, istep, step, temp, 2); // iopt=2
              CalculationRoutines.gScysStat(iproc);
                
                if (i_strpf == 1) {
                  CalculationRoutines.poleFig(icrysym, iproc, 1, filecrys);
                }
                
            } // --- End Process Loop
            
            // --- 9. Write Final State to unit 14 ---
            PrintWriter out14 = IOUtils.writer14;
            if (out14 != null) {
                out14.println(" SELF-CONSISTENT ELASTIC STIFFNESS");
                print6x6Matrix(out14, CommonGlobals.css2);
                out14.println(" SELF-CONSISTENT THERMAL COEFFICIENTS");
                print6x1Vector(out14, CommonGlobals.alfass);
                out14.println(" SELF-CONSISTENT ELASTO-PLASTIC STIFFNESS");
                print6x6Matrix(out14, CommonGlobals.ass2);
                out14.println(" EFFECTIVE INTERACTION TENSOR");
                print6x6Matrix(out14, CommonGlobals.aef);
                out14.println(" STRESS - TOTAL STRAIN - ELASTIC STRAIN IN SAMPLE");
                print6x1Vector(out14, CommonGlobals.stss);
                print6x1Vector(out14, CommonGlobals.etss);
                print6x1Vector(out14, CommonGlobals.etelss);
                out14.println(" STRESS - TOTAL STRAIN IN GRAINS");
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    print6x1Vector(out14, CommonGlobals.stcs, ng);
                    print6x1Vector(out14, CommonGlobals.etcs, ng);
                }
                out14.println(" CRITICAL STRESS IN EACH SYSTEM OF EACH GRAIN");
                for (int ng = 1; ng <= CommonGlobals.ngrain; ng++) {
                    for (int n = 1; n <= CommonGlobals.nsys; n++) {
                        double dummy = -999.0;
                        if (Math.abs(CommonGlobals.tau0[n]) > 1.0e-9) {
                            dummy = CommonGlobals.tau[n][ng] / CommonGlobals.tau0[n];
                        }
                        out14.printf(Locale.US, "%7.4f", dummy);
                    }
                    out14.printf(Locale.US, "  %7.4f%n", CommonGlobals.gamtot[ng]);
                }
            }
            
            if (IOUtils.writer11 != null) {
                IOUtils.writer11.printf(Locale.US, " TOTAL TIME=%8.2f secs%n", time_acum);
                IOUtils.writer11.println("******************************************************************************");
            }

        } catch (FileNotFoundException e) {
            System.err.println("Main input file 'epsc4.in' not found.");
            e.printStackTrace();
        } catch (Exception e) {
            System.err.println("An error occurred during simulation.");
            e.printStackTrace();
        } finally {
            // Close all file writers
            IOUtils.closeIO();
        }
        
        System.out.println("****************************** EPSC PROGRAM END ******************************");
    }
    
    // --- Helper methods for writing output file 14 ---
    
    private static void print6x6Matrix(PrintWriter out, double[][] matrix) {
        for (int i = 1; i <= 6; i++) {
            out.printf(Locale.US, " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                       matrix[i][1], matrix[i][2], matrix[i][3],
                       matrix[i][4], matrix[i][5], matrix[i][6]);
        }
    }
    
    private static void print6x1Vector(PrintWriter out, double[] vector) {
        out.printf(Locale.US, " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                   vector[1], vector[2], vector[3],
                   vector[4], vector[5], vector[6]);
    }
    
    private static void print6x1Vector(PrintWriter out, double[][] matrix, int ng) {
        out.printf(Locale.US, " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                   matrix[1][ng], matrix[2][ng], matrix[3][ng],
                   matrix[4][ng], matrix[5][ng], matrix[6][ng]);
    }
}

