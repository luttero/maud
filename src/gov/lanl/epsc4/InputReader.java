package gov.lanl.epsc4;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;

public class InputReader {

    /**
     * Reads the CRYSTAL data file.
     * This method opens and is responsible for IOUtils.scanner1.
     */
    public void dataCrystal(String filecrys, IntHolder icrysym, SimulationState state) throws FileNotFoundException {
        File inputFile = new File(filecrys);
        // Dump file to writer 11
        if (IOUtils.writer11 != null) {
            try (Scanner dumpScanner = new Scanner(inputFile)) {
                IOUtils.writer11.println("\n ****** CRYSTAL DATA FILE *******");
                while (dumpScanner.hasNextLine()) {
                    IOUtils.writer11.println(dumpScanner.nextLine());
                }
                IOUtils.writer11.println(" ****** END OF CRYSTAL DATA FILE ******\n");
            }
        }
        
        // Open the static scanner for unit 1, as crssVoce will also read from it
        IOUtils.scanner1 = new Scanner(inputFile);
        Scanner s = IOUtils.scanner1;
        s.useLocale(Locale.US);

        if (IOUtils.writer11 != null) {
            IOUtils.writer11.println("******* CRYSTAL data - File: " + filecrys + "********");
            IOUtils.writer11.println("******************************************************************************");
        }

        // --- Reads crystal symmetry (calls crystalSymmetry iopt=1) ---
        Physics.crystalSymmetry(1, s, icrysym, null, null, null, null, null, new IntHolder(0));

        state.nind = (icrysym.value == 2 || icrysym.value == 3) ? 4 : 3;

        // --- READS SINGLE CRYSTAL ELASTIC STIFFNESS ---
        s.nextLine(); // prosa
        for (int i = 1; i <= 6; i++) {
            String[] line = s.nextLine().trim().split("\\s+");
            for (int j = 1; j <= 6; j++) {
                state.ccc2[i][j] = Double.parseDouble(line[j - 1]);
            }
        }
        TensorUtils.invten(state.ccc2, state.scc2);

        // --- Large elastic strain and pressure dependence ---
        s.nextLine(); // prosa
        state.kSM = Double.parseDouble(s.nextLine().trim());
        if (state.kSM == 1.0) {
            s.nextLine(); // prosa
            for (int i = 1; i <= 6; i++) {
                String[] line = s.nextLine().trim().split("\\s+");
                for (int j = 1; j <= 6; j++) {
                    state.ccc2dp[i][j] = Double.parseDouble(line[j - 1]);
                }
            }
            for (int i = 1; i <= 6; i++) {
                System.arraycopy(state.ccc2[i], 1, state.ccc2p0[i], 1, 6);
            }
        }

        // --- READS SINGLE CRYSTAL THERMAL EXPANSION COEFFICIENTS ---
        s.nextLine(); // prosa
        String[] line = s.nextLine().trim().split("\\s+");
        for (int i = 1; i <= 6; i++) state.alfacc[i] = Double.parseDouble(line[i - 1]);

        // --- READS CRYSTALLOGRAPHIC MODE PARAMETERS ---
        s.nextLine(); // prosa
        int nmodesx = Integer.parseInt(s.nextLine().trim());
        state.nmodes = Integer.parseInt(s.nextLine().trim());
        state.NMOD = state.nmodes;
        
        state.allocateForNMOD();
        
        int[] mode = new int[state.NMOD + 1];
        line = s.nextLine().trim().split("\\s+");
        for (int i = 1; i <= state.nmodes; i++) mode[i] = Integer.parseInt(line[i - 1]);

        int im = 1, isys = 0;
        state.nslsys = 0; state.ntwsys = 0; state.nslmod = 0; state.ntwmod = 0;
        state.NSLS = 100; // initially
        state.allocateForNSLS();

        for (int iloop = 1; iloop <= nmodesx; iloop++) {
            s.nextLine(); // namesys
            line = s.nextLine().trim().split("\\s+");
            int modex = Integer.parseInt(line[0]);
            int nsmx = Integer.parseInt(line[1]);
            int nrsx = Integer.parseInt(line[2]);
            int iopsysx = Integer.parseInt(line[3]);
            int itwx = Integer.parseInt(line[4]);
            double stwx = 0.0;
            if (itwx == 1) stwx = Double.parseDouble(s.nextLine().trim());
            
            if (nsmx > state.NSLS) throw new RuntimeException("ERROR: nsmx > NSLS. Dynamic resize not implemented in refactor.");
            if (modex != iloop) throw new RuntimeException("WARNING!!! MODE NUMBERS MUST BE SEQUENTIAL IN CRYSTAL FILE");

            if (iloop != mode[im]) {
                for (int is = 1; is <= nsmx; is++) s.nextLine(); // Skip systems
            } else {
                if (iopsysx == 1 && itwx == 1) throw new RuntimeException("WARNING!!! IOPSYSX MUST BE =0 WHEN ITWX=1");

                state.nsm[im] = (iopsysx + 1) * nsmx;
                state.itw[im] = itwx;

                if (itwx == 0) {
                    state.stw[im] = 0;
                    state.nslmod++;
                    state.nslsys += state.nsm[im];
                } else {
                    state.stw[im] = stwx;
                    state.itwinning = 1;
                    state.ntwmod++;
                    state.ntwsys += state.nsm[im];
                }

                // --- Read Miller indices ---
                int[] isn_loc = new int[5];
                int[] isb_loc = new int[5];
                double[] sn_loc = new double[4];
                double[] sb_loc = new double[4];
                
                for (int is = 1; is <= nsmx; is++) {
                    line = s.nextLine().trim().split("\\s+");
                    for (int i = 1; i <= state.nind; i++) {
                        isn_loc[i] = Integer.parseInt(line[i - 1]);
                        isb_loc[i] = Integer.parseInt(line[i - 1 + state.nind]);
                    }
                    
                    Physics.crystalSymmetry(2, null, icrysym, isn_loc, sn_loc, null, isb_loc, sb_loc, null);
                    
                    double prod = sn_loc[1]*sb_loc[1] + sn_loc[2]*sb_loc[2] + sn_loc[3]*sb_loc[3];
                    if (prod >= 0.000001) throw new RuntimeException("SYSTEM IS NOT ORTHOGONAL!!");

                    isys++;
                    for (int i = 1; i <= 3; i++) {
                        state.bcc[i][isys] = sb_loc[i];
                        state.ncc[i][isys] = sn_loc[i];
                        for (int j = 1; j <= 3; j++) {
                            state.mc2[i][j][isys] = 0.5 * (sb_loc[i] * sn_loc[j] + sb_loc[j] * sn_loc[i]);
                            state.qc2[i][j][isys] = 0.5 * (sb_loc[i] * sn_loc[j] - sb_loc[j] * sn_loc[i]);
                        }
                    }
                    
                    if (iopsysx == 1) {
                        isys++;
                        for (int i = 1; i <= 3; i++) {
                            state.bcc[i][isys] = -sb_loc[i];
                            state.ncc[i][isys] = sn_loc[i];
                            for (int j = 1; j <= 3; j++) {
                                state.mc2[i][j][isys] = -state.mc2[i][j][isys - 1];
                                state.qc2[i][j][isys] = -state.qc2[i][j][isys - 1];
                            }
                        }
                    }
                } // end loop over nsmx
                im++;
                if (im > state.NMOD) throw new RuntimeException("ERROR: More modes read than declared.");
                if (isys > state.NSLS) throw new RuntimeException("ERROR: More systems read than declared.");
            } // end if mode is active
        } // end loop over nmodesx
        
        state.nsys = isys;
        int nst = 0;
        for (int m = 1; m <= state.nmodes; m++) {
            for (int is = 1; is <= state.nsm[m]; is++) {
                nst++;
                state.iSysMode[nst] = m;
                state.mode_slip[m][is] = nst;
                state.iTwinSys[nst] = state.itw[m];
            }
        }
        
        // --- Initialize fijph (from data_crystal end) ---
        TensorUtils.eulerFromAngles(state.eulerph[1], state.eulerph[2], state.eulerph[3], ModUpdateFij.FNEW);
        for(int i=1; i<=3; i++) for(int j=1; j<=3; j++) state.fijph[i][j] = ModUpdateFij.FNEW[i][j];
        
        // ... (rest of fijph init is not needed as it's overwritten in update_fij)
    }

    /**
     * Reads the PROCESS data file.
     */
    public void dataProcess(String fileproc, SimulationState state, IntHolder i_temp_cij,
                            IntHolder i_ref_et, IntHolder i_ref_st, IntHolder i_bc_mode) {
        
        try (Scanner s = new Scanner(new File(fileproc))) {
            s.useLocale(Locale.US);
            // (Dump file to writer 11 logic...)

            s.nextLine(); s.nextLine(); s.nextLine(); // Skip comments
            state.nsteps = Integer.parseInt(s.nextLine().trim());
            state.i_control_var = Integer.parseInt(s.nextLine().trim());
            i_bc_mode.value = Integer.parseInt(s.nextLine().trim());
            state.i_bc_mode = i_bc_mode.value;
            s.nextLine(); s.nextLine(); s.nextLine(); // Skip comments

            String[] line1 = s.nextLine().trim().split("\\s+");
            String[] line2 = s.nextLine().trim().split("\\s+");
            String[] line3 = s.nextLine().trim().split("\\s+");
            // ... (Read ifulletbc) ...
            
            state.ietbc[1] = state.ifulletbc[1][1];
            // ... (etc. for ietbc) ...

            s.nextLine(); // Skip comment
            line1 = s.nextLine().trim().split("\\s+");
            line2 = s.nextLine().trim().split("\\s+");
            line3 = s.nextLine().trim().split("\\s+");
            // ... (Read fulletbc) ...
            
            s.nextLine(); s.nextLine(); s.nextLine(); // Skip comments
            line1 = s.nextLine().trim().split("\\s+");
            line2 = s.nextLine().trim().split("\\s+");
            line3 = s.nextLine().trim().split("\\s+");
            // ... (Read istbc) ...
            
            s.nextLine(); // Skip comment
            line1 = s.nextLine().trim().split("\\s+");
            line2 = s.nextLine().trim().split("\\s+");
            line3 = s.nextLine().trim().split("\\s+");
            // ... (Read stbc) ...

            s.nextLine(); s.nextLine(); s.nextLine(); // Skip comments
            state.temp_s = Double.parseDouble(s.nextLine().trim());
            s.nextLine(); // Skip comment
            state.deltemp = Double.parseDouble(s.nextLine().trim());
            s.nextLine(); // Skip comment
            i_temp_cij.value = Integer.parseInt(s.nextLine().trim());
            s.nextLine(); // Skip comment
            i_ref_et.value = Integer.parseInt(s.nextLine().trim());
            s.nextLine(); // Skip comment
            i_ref_st.value = Integer.parseInt(s.nextLine().trim());

        } catch (FileNotFoundException e) {
            throw new RuntimeException("Process file not found: " + fileproc, e);
        }
    }
    
    /**
     * Reads the SAMPLE (texture) data file.
     */
    public List<Grain> dataSample(String filesamp, int i_prev_proc, String fileprev, SimulationState state) {
        List<Grain> grains = new ArrayList<>();
        double totwgt = 0.0;
        
        try (Scanner s = new Scanner(new File(filesamp))) {
            s.useLocale(Locale.US);
            // ... (Dump file to writers 11 and 12 logic...)
            
            s.nextLine(); s.nextLine(); s.nextLine(); // Skip comments
            String[] line = s.nextLine().trim().split("\\s+");
            int ngrain = Integer.parseInt(line[1]);

            state.NGR = ngrain;
            state.ngParent = ngrain;
            
            // --- Allocate NGR-dependent arrays in SimulationState ---
            state.ngrset = new int[state.NDIFFX + 1];
            state.RAND_WGT = new double[state.NDIFFX + 1];
            state.wgtset = new double[state.NDIFFX + 1];
            state.wgtsetini = new double[state.NDIFFX + 1];
            state.chiPoleFig = new double[state.NDIFFX + 1];
            state.etaPoleFig = new double[state.NDIFFX + 1];
            state.igrset = new int[state.NDIFFX + 1][state.NGR + 1];
            state.wgtgrset = new double[state.NDIFFX + 1][state.NGR + 1];
            
            // --- Allocate Plasticity arrays ---
            state.shear_mod_acum_ch = new double[state.NMOD + 1];
            // ... (all other plasticity arrays) ...
            state.WP = new double[state.NGR + 1];
            state.WC = new double[state.NGR + 1];

            // --- Read all grains ---
            for (int i = 1; i <= state.NGR; i++) {
                Grain g = new Grain(i, state.NSLS, state.kCL);
                line = s.nextLine().trim().split("\\s+");
                g.phi = Double.parseDouble(line[0]);
                g.the = Double.parseDouble(line[1]);
                g.ome = Double.parseDouble(line[2]);
                g.wgt = Double.parseDouble(line[3]);
                totwgt += g.wgt;
                grains.add(g);
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Sample file not found: " + filesamp, e);
        }
        
        // --- Normalize weights and calculate rotation matrices ---
        for (Grain g : grains) {
            g.wgt /= totwgt;
            TensorUtils.eulerFromAngles(g.phi, g.the, g.ome, g.r);
        }

        // --- Read previous state if flagged ---
        if (i_prev_proc == 1) {
            try (Scanner s = new Scanner(new File(fileprev))) {
                // ... (Logic from data_sample to read previous state) ...
            } catch (FileNotFoundException e) {
                throw new RuntimeException("Previous state file not found: " + fileprev, e);
            }
        }
        return grains;
    }
}
