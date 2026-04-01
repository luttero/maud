package gov.lanl.epsc4;

/**
 * Represents a single Grain in the aggregate.
 * This class holds all data that was previously in [...][ng] arrays.
 */
public class Grain {
    
    // --- Identity ---
    public final int id; // 1-based index
    public double wgt;

    // --- Kinematics ---
    public double[] phi = new double[4]; // Euler angles
    public double[] the = new double[4];
    public double[] ome = new double[4];
    public double[][] r = new double[4][4]; // Orientation matrix

    // --- State Variables (1-based) ---
    public double[] stcs = new double[7]; // Stress
    public double[] etcs = new double[7]; // Strain
    public double[] strcs = new double[7]; // Stress rate
    public double[] etrcs = new double[7]; // Strain rate
    public double[] stcsref = new double[7];
    public double[] etelcs = new double[7]; // Elastic strain
    public double[] etelhycs = new double[7]; // Hydrostatic elastic strain
    public double[] etthcs = new double[7]; // Thermal strain
    public double[] alfacs = new double[7]; // Thermal expansion
    
    // --- Stiffness ---
    public double[][] ccs2 = new double[7][7]; // Elastic stiffness
    public double[][] scs2 = new double[7][7]; // Elastic compliance
    public double[][] acs2 = new double[7][7]; // Elasto-plastic stiffness
    public double[][] aefgr = new double[7][7]; // Interaction tensor
    
    // --- Plasticity ---
    public int nact;
    public int[] iact; // [NSLS+1]
    public double[] gamd; // [NSLS+1]
    public double[][] mcs; // [7][NSLS+1]
    public double[][] qcs; // [7][NSLS+1]
    public double[][] bcs; // [4][NSLS+1]
    public double[][] ncs; // [4][NSLS+1]
    public double[][] f; // [7][NSLS+1]
    public double[] taud; // [NSLS+1]
    public double[] tau; // [NSLS+1]
    public double[] tau_update; // [NSLS+1]
    public double gamtot;
    public int[] ng_update; // [NGR+1] -> This is messy. Should be a boolean.
    
    // --- Dislocation Density (kCL=2) ---
    public double[] rho_for; // [NSLS+1]
    public double rho_deb;
    
    // --- Twinning ---
    public int iTwinLevel;
    public int iParentGrain; // 0 if parent
    public int iParentSystem;
    public int iParentMode;
    public int[] iChildGrain; // [NSLS+1] (index of child, or 0)
    public double wgtd; // Weight change this step
    
    // --- Interaction ---
    public double meffc;
    
    // --- Shape (ishape >= 2) ---
    public double[][] fijgr = new double[4][4];
    public double[][] omegag = new double[4][4];
    public double[][][][] EINVSAGR = new double[4][4][4][4];
    public double[][][][] ESCR4GR = new double[4][4][4][4];
    public double[][][][] as = new double[4][4][4][4];
    public double[][] axisgr = new double[4][4]; // 0:3, 1:3

    
    /**
     * Constructor for a new Grain.
     * @param id The 1-based ID of the grain.
     * @param state The global simulation state (for array sizes).
     */
    public Grain(int id, SimulationState state) {
        this.id = id;
        
        // Allocate all per-grain arrays
        this.iact = new int[state.NSLS + 1];
        this.gamd = new double[state.NSLS + 1];
        this.mcs = new double[7][state.NSLS + 1];
        this.qcs = new double[7][state.NSLS + 1];
        this.bcs = new double[4][state.NSLS + 1];
        this.ncs = new double[4][state.NSLS + 1];
        this.f = new double[7][state.NSLS + 1];
        this.taud = new double[state.NSLS + 1];
        this.tau = new double[state.NSLS + 1];
        this.tau_update = new double[state.NSLS + 1];
        this.iChildGrain = new int[state.NSLS + 1];
        
        if (state.kCL == 2) {
            this.rho_for = new double[state.NSLS + 1];
        }
        
        // This array is still problematic.
        this.ng_update = new int[state.NGR + 1];
    }
}
