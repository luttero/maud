package gov.lanl.epsc4;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;

/**
 * Main program for the EPSC simulation.
 * This class orchestrates the simulation by:
 * 1. Initializing state
 * 2. Reading input files
 * 3. Calling the main process and step loops
 * 4. Writing final output
 */
public class EPSC4 {

    public static void main(String[] args) {

        // --- 1. Initialization ---
        IOUtils.initializeIO();
        SimulationState state = new SimulationState();
        state.initializeGlobals();
        
        EshelbyTensorCalculator eshelbyCalc = new EshelbyTensorCalculator();
        eshelbyCalc.initialize(); // Runs iopt=0

        // --- Create Service classes ---
        InputReader reader = new InputReader();
        Solver solver = new Solver();
        Physics physics = new Physics();
        OutputWriter output = new OutputWriter();

        // --- Local "holder" variables ---
        String filecrys = "", filesamp = "", fileprev = "", filediff = "";
        String[] fileproc = null;
        IntHolder i_temp_cij_h = new IntHolder(0);
        IntHolder i_ref_et_h = new IntHolder(0);
        IntHolder i_ref_st_h = new IntHolder(0);
        IntHolder i_bc_mode_h = new IntHolder(0);
        IntHolder icrysym_h = new IntHolder(0);
        
        List<Grain> grains = new ArrayList<>();
        HardeningModel hardeningModel = null;
        
        int nFile = 0, nout_old = 0;
        double step = 1.0, temp = 0.0, xref = 0.0;

        try {
            // --- 2. Read Master Input File (epsc4.in) ---
            File inputFile = new File("epsc4.in");
            Scanner s = new Scanner(inputFile);
            s.useLocale(Locale.US);
            IOUtils.scanner1 = s; // Set static scanner for data_crystal/crss_voce

            String prosa = s.nextLine().trim(); // simulation label
            output.writeMainHeaders(prosa); // Write headers to all files
            
            s.nextLine(); // Skip comment
            state.ishape = Integer.parseInt(s.nextLine().trim());
            String[] line = s.nextLine().trim().split("\\s+");
            for (int i = 1; i <= 3; i++) state.axis[i] = Double.parseDouble(line[i - 1]);
            line = s.nextLine().trim().split("\\s+");
            for (int i = 1; i <= 3; i++) state.eulerph[i] = Double.parseDouble(line[i - 1]);
            
            s.nextLine(); // Skip comment
            filesamp = s.nextLine().trim();
            state.irot = Integer.parseInt(s.nextLine().trim());
            
            s.nextLine(); // Skip comment
            filecrys = s.nextLine().trim();
            
            s.nextLine(); // Skip comment (Precision)
            state.itmax_mod = Integer.parseInt(s.nextLine().trim());
            state.error_mod = Double.parseDouble(s.nextLine().trim());
            state.itmax_grain = Integer.parseInt(s.nextLine().trim());
            
            s.nextLine(); // Skip comment (Prev. proc)
            int i_prev_proc = Integer.parseInt(s.nextLine().trim());
            fileprev = s.nextLine().trim();
            state.itexskip = Integer.parseInt(s.nextLine().trim());
            
            int i_diff_dir = Integer.parseInt(s.nextLine().trim());
            filediff = s.nextLine().trim();
            
            int i_strpf = Integer.parseInt(s.nextLine().trim());
            
            s.nextLine(); // Skip comment (NProc)
            int nproc = Integer.parseInt(s.nextLine().trim());
            state.NPROCX = nproc;
            fileproc = new String[nproc + 1]; // 1-based
            
            s.nextLine(); // Skip comment (Proc files)
            for (int n = 1; n <= nproc; n++) {
                fileproc[n] = s.nextLine().trim();
            }
            // Master input file 'epsc4.in' is now read.

            // --- 3. Load Data Files ---
            reader.dataCrystal(filecrys, icrysym_h, state);
            int icrysym = icrysym_h.value;
            
            state.kCL = Integer.parseInt(s.nextLine().trim()); // Read kCL
            if (state.kCL == 0)      hardeningModel = new VoceHardening();
            else if (state.kCL == 2) hardeningModel = new DislocationDensityHardening();
            else throw new RuntimeException("Unsupported kCL value: " + state.kCL);
            
            hardeningModel.readParameters(s, state);
            
            // We are done with unit 1
            if (IOUtils.scanner1 != null) {
                IOUtils.scanner1.close();
                IOUtils.scanner1 = null;
            }

            grains = reader.dataSample(filesamp, i_prev_proc, fileprev, state);
            
            for (Grain g : grains) {
                hardeningModel.initializeGrain(g, state);
            }
            output.writePlasticityHeaders(state);
            
            state.ipileup = 0;
            if (state.ipileup == 1) solver.pileup(0, state, grains);
            
            physics.crToSa(1, state.ngrain, 0, state, grains);
            physics.crToSa(1, state.ngrain, 1, state, grains);
            
            if (i_diff_dir == 1) {
                output.difPlanes(icrysym, filediff, 0.0, 0, 0, state, grains); // iopt=0
                output.writeDiffractionHeaders(state);
                output.difPlanes(icrysym, filediff, 0.0, 0, 1, state, grains); // iopt=1
            }
            
            if (i_strpf == 1) {
                physics.poleFig(icrysym, nFile, 0, filecrys, state, grains);
            }

            if (i_prev_proc == 0) {
                if (IOUtils.writer11 != null) IOUtils.writer11.println("PURE ELASTIC SELFCONSISTENT PROBLEM");
                for (Grain g : grains) {
                    g.gamtot = 0.0;
                    for (int i = 1; i <= 6; i++) {
                        for (int j = 1; j <= 6; j++) {
                            g.acs2[i][j] = g.ccs2[i][j];
                        }
                    }
                }
                solver.scNew(0, 0, new IntHolder(0), 1, 0, state, grains, eshelbyCalc); // iopt=0
                TensorUtils.invten(state.ass2, state.sss2, state.invfac);
            }

            // --- 7. Initialize State Variables ---
            for (int i = 1; i <= 6; i++) state.etssref[i] = 0.0;
            for (Grain g : grains) {
                for (int i = 1; i <= 6; i++) {
                    g.stcsref[i] = 0.0;
                    g.etelcs[i] = 0.0;
                    g.etelhycs[i] = 0.0;
                }
            }
            
            // --- 4. START MAIN PROCESS LOOP ---
            for (int iproc = 1; iproc <= nproc; iproc++) {
                output.plasticity(iproc, 0, 0, 0, 0, state, grains); // iopt=0
                nout_old = 0;
                for (Grain g : grains) {
                    if (iproc == 1) g.ng_update = 0;
                    for (int ns1 = 1; ns1 <= state.nsys; ns1++) g.tau_update[ns1] = 1.0;
                }

                reader.dataProcess(fileproc[iproc], state, i_temp_cij_h, i_ref_et_h, i_ref_st_h, i_bc_mode_h);
                solver.loadConditions(state);
                
                // Decompose full strain tensor
                for(int i=1; i<=3; i++) for(int j=1; j<=3; j++) {
                    state.etbc_sym[i][j] = 0.5 * (state.fulletbc[i][j] + state.fulletbc[j][i]);
                    state.omegabc[i][j] = 0.5 * (state.fulletbc[i][j] - state.fulletbc[j][i]);
                    state.omegabcr[i][j] = state.omegabc[i][j] / state.nsteps;
                }
                TensorUtils.voigt(state.etbc, state.etbc_sym, C2_dummy, C4_dummy, 2);

                // ... (Set etssref, stcsref based on flags) ...
                
                output.writeStrainStressLog(state, grains);
                if (i_diff_dir == 1) {
                    output.difPlanes(icrysym, filediff, 0.0, 0, 1, state, grains);
                }
                
                // ... (Calculate strbc, etrbc increments) ...
                // ... (Check for zero increments) ...
                
                temp = state.temp_s;
                output.writeProcessHeader(fileproc[iproc]);
                
                for (int i=1; i<=6; i++) {
                    etss_ini[i] = state.etss[i];
                    stss_ini[i] = state.stss[i];
                }

                // --- 5. START MAIN STEP LOOP ---
                for (istep = 1; istep <= state.nsteps; istep++) {
                    
                    state.iTotStep++;
                    if (state.icvx==0) xref=temp;
                    if (state.icvx>=1 && state.icvx<=6) xref=state.etss[state.icvx]-state.etssref[state.icvx];
                    if (state.icvx>=7) xref=state.stss[state.icvx-6];
                    
                    output.writeStepHeader(istep, xref, state);
                    
                    for (Grain g : grains) {
                        for (int isys = 1; isys <= state.nsys; isys++) g.gamd[isys] = 0.0;
                    }
                    
                    // ... (Update elastic constants if temp or pressure changes) ...

                    // --- 6. START SOLVER (GRAIN ITERATION) LOOP ---
                    int it_grain = 0;
                    IntHolder iverify = new IntHolder(1);
                    
                    while (iverify.value != 0) {
                        it_grain++;
                        if (it_grain == state.itmax_grain) {
                            throw new RuntimeException("DOES NOT CONVERGE AFTER " + it_grain + " ITERATIONS");
                        }
                        
                        nout_old = solver.gActsys(nout_old, grains, state);
                        solver.sState(state);
                        solver.gModulus(grains, state, hardeningModel);
                        
                        boolean converged = solver.scNew(2, it_grain, 1, istep, state, grains, eshelbyCalc);
                        iverify.value = converged ? 0 : 1;
                    }
                    
                    // --- 7. Update State After Convergence ---
                    for (Grain g : grains) {
                        if (g.nact != 0) {
                            for (int ns1 = 1; ns1 <= state.nsys; ns1++) {
                                g.tau[ns1] += g.taud[ns1] * step;
                                g.gamtot += g.gamd[ns1] * step;
                            }
                        }
                        if (state.kCL == 2) {
                            hardeningModel.updateState(g, state);
                        }
                    }
                    
                    if (state.itwinning == 1) {
                        physics.twinningBjcl(step, grains, state, hardeningModel);
                    }
                    
                    if (state.ishape >= 1) {
                        physics.updateFij(step, state, grains);
                    }
                    
                    if (istep < state.nsteps && state.irot == 1) {
                        physics.updateOrientation(step, istep, 1, grains, state);
                        physics.corotation(istep, step, grains, state);
                    }
                    
                    // --- Update global stress/strain state ---
                    for (int i = 1; i <= 6; i++) {
                        state.stss[i] += state.strss[i] * step;
                        state.etss[i] += state.etrss[i] * step;
                        for (Grain g : grains) {
                            g.stcs[i] += g.strcs[i] * step;
                            g.etcs[i] += g.etrcs[i] * step;
                            g.etthcs[i] += g.alfacs[i] * state.deltemp * step;
                        }
                    }
                    
                    if (state.ipileup == 0) temp += state.deltemp;
                    
                    output.gAverage(state, grains);
                    
                    // ... (Update stss/etss from stav/etav if twinning) ...
                    // ... (Calculate etelss from sss2) ...
                    
                    output.writeStrainStressLog(state, grains);
                    output.plasticity(iproc, istep, step, temp, 1, state, grains);
                    if (i_diff_dir == 1) {
                        output.difPlanes(icrysym, filediff, temp, istep, 1, state, grains);
                    }
                    output.effectiveMagnitudes(iproc, istep, step, state);
                    
                } // --- End Step Loop ---
                
                output.plasticity(iproc, istep, step, temp, 2, state, grains);
                output.gScysStat(iproc, state, grains);
                if (i_strpf == 1) {
                    physics.poleFig(icrysym, iproc, 1, filecrys, state, grains);
                }
                
            } // --- End Process Loop ---

            // --- 8. Write Final State ---
            output.writeFinalState(state, grains);
            
        } catch (Exception e) {
            System.err.println("An error occurred during simulation:");
            e.printStackTrace();
        } finally {
            // Close all file writers
            IOUtils.closeIO();
        }
    }
}
