package gov.lanl.epsc4;

import java.util.Scanner;

/**
 * Interface for hardening models (e.g., Voce, Dislocation Density).
 * This replaces the "kCL" flag.
 */
public interface HardeningModel {

    /**
     * Reads hardening parameters from the crystal file.
     * @param s The scanner (unit 1) positioned at the start of the parameters.
     * @param state The global simulation state to populate.
     */
    void readParameters(Scanner s, SimulationState state);
    
    /**
     * Initializes the state of a single grain (e.g., tau0, rho_ini).
     * @param grain The grain to initialize.
     * @param state The global simulation state.
     */
    void initializeGrain(Grain grain, SimulationState state);
    
    /**
     * Calculates the hardening matrix 'hd' for a grain.
     * @param grain The grain to calculate for.
     * @param state The global simulation state.
     */
    void calculateHardening(Grain grain, SimulationState state);
    
    /**
     * Updates the internal state (e.g., densities) based on shear.
     * @param grain The grain to update.
     * @param state The global simulation state.
     */
    void updateState(Grain grain, SimulationState state);
}
