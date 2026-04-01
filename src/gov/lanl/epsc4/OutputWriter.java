package gov.lanl.epsc4;

import java.io.PrintWriter;
import java.util.List;
import java.util.Locale;

/**
 * Handles all file output for the simulation.
 */
public class OutputWriter {
    
    public void writeMainHeaders(String prosa) {
        PrintWriter out12 = IOUtils.writer12; // System.out
        out12.println("******************************************************************************");
        out12.println("************** SELF-CONSISTENT THERMO-ELASTOPLASTIC CODE \"EPSC\" **************");
        out12.println(prosa);
        out12.println("******************************************************************************");
        
        for (int iunit = 11; iunit <= 19; iunit++) {
            PrintWriter out = getWriter(iunit);
            if (out != null) {
                out.println("******************************************************************************");
                out.println(prosa);
                out.println("******************************************************************************");
            }
        }
        
        if (IOUtils.writer13 != null) IOUtils.writer13.println("COMPONENTS 11 22 33 OF SAMPLE STRAIN, STRESS, ELASTIC STRAIN and AVACS");
        // ... (all other header writes from main program) ...
    }

    /**
     * Calculates and writes averages and deviations for stress, strain, etc.
     */
    public void gAverage(SimulationState state, List<Grain> grains) {
        // ... (Full implementation from g_average) ...
    }

    /**
     * Calculates and writes equivalent states and energies to unit #18.
     */
    public void effectiveMagnitudes(int iproc, int istep, double step, SimulationState state) {
        // ... (Full implementation from effective_magnitudes) ...
    }

    /**
     * Evaluates plastic activity, shear distribution, and twinning fractions.
     */
    public void plasticity(int iproc, int istep, double step, double temp, int iopt, SimulationState state, List<Grain> grains) {
        // ... (Full implementation from plasticity) ...
    }
    
    /**
     * Calculates statistics on Single Crystal Yield Surface (SCYS) updates.
     */
    public void gScysStat(int iproc, SimulationState state, List<Grain> grains) {
        // ... (Full implementation from g_scys_stat) ...
    }
    
    /**
     * Calculates and writes diffraction plane data.
     */
    public void difPlanes(int icrysym, String filediff, double temp, int istep, int iopt, SimulationState state, List<Grain> grains) {
        // ... (Full implementation from dif_planes) ...
        // Note: iopt=0 (init) and iopt=1 (calc) logic are both in here
    }

    /**
     * Writes the current texture (Euler angles and weights) to a new file.
     */
    public void writeTexFile(int nFile, int iProc, int iStep, SimulationState state, List<Grain> grains) {
        // ... (Full implementation from WriteTexFile) ...
    }
    
    /**
     * Writes the final simulation state to unit 14.
     */
    public void writeFinalState(SimulationState state, List<Grain> grains) {
        PrintWriter out14 = IOUtils.writer14;
        if (out14 == null) return;
        
        out14.println(" SELF-CONSISTENT ELASTIC STIFFNESS");
        IOUtils.printMatrix(out14, state.css2);
        // ... (Full implementation from PROGRAM EPSC4 end block) ...
    }
    
    // ... (other helper write methods) ...
}
