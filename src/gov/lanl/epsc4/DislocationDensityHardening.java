package gov.lanl.epsc4;

import java.util.Scanner;

/**
 * Dislocation Density hardening model implementation (kCL = 2).
 */
public class DislocationDensityHardening implements HardeningModel {

    // Work arrays
    private double[] Drho_for_Dgamma;
    private double[] Drho_deb_Dgamma;
    
    @Override
    public void readParameters(Scanner s, SimulationState state) {
        Drho_for_Dgamma = new double[state.NSLS + 1];
        Drho_deb_Dgamma = new double[state.NSLS + 1];
        
        // ... (This logic was in crssDislocDens iopt=0) ...
        // (Calculate shearmod(im) - requires 'voigt' and 'ccc2')
        // ...
        
        // Read hardening parameters
        s.nextLine(); // Skip comment
        String[] line = s.nextLine().trim().split("\\s+");
        state.chi_inter = Double.parseDouble(line[0]);
        state.Q_rate = Double.parseDouble(line[1]);
        state.edot_macro = Double.parseDouble(line[2]);
        
        for (int is = 1; is <= state.nslmod; is++) {
            // ... (Read all parameters for BURG, ACTENER, etc.) ...
        }
        for (int it = 1; it <= state.ntwmod; it++) {
            // ... (Read all parameters for tau_crit, tau_prop, etc.) ...
        }
    }

    @Override
    public void initializeGrain(Grain grain, SimulationState state) {
        grain.gamtot = 0.0;
        int is = 0;
        for (int imo = 1; imo <= state.nslmod; imo++) {
            for (int isy = 1; isy <= state.nsm[imo]; isy++) {
                is++;
                state.tau0_mode[imo] = state.tau0_mode_a[imo] + 
                    state.tau0_mode_b[imo] * Math.exp(-state.temp_s / state.tau0_mode_c[imo]);
                
                grain.rho_for[is] = state.rho_ini_for[imo];
                grain.rho_deb = state.rho_ini_deb[imo];
                
                grain.tau[is] = state.tau0_mode[imo] + state.chi_inter * state.BURG[imo] * state.shearmod[imo] * Math.sqrt(grain.rho_for[is]);
                
                state.a_deb[imo] = state.a_deb_a[imo] + 
                    state.a_deb_b[imo] * Math.log(1.0 + state.temp_s / state.a_deb_c[imo]);
            }
        }
        
        for (int imo = 1; imo <= state.ntwmod; imo++) {
            for (int isy = 1; isy <= state.nsm[imo + state.nslmod]; isy++) {
                is++;
                grain.tau[is] = 0.0;
                int js = 0;
                for (int jmo = 1; jmo <= state.nslmod; jmo++) {
                    for (int jsy = 1; jsy <= state.nsm[jmo]; jsy++) {
                        js++;
                        grain.tau[is] += state.shearmod[imo] * state.burg_tw[imo] * state.BURG[jmo] *
                                         state.TLATENT[jmo][imo] * grain.rho_for[js];
                    }
                }
                grain.tau[is] += state.tau_prop_a[imo] +
                    state.tau_prop_b[imo] * Math.exp(-state.temp_s / state.tau_prop_c[imo]);
            }
        }
    }
    
    private void calculateDerivatives(Grain grain, SimulationState state) {
        for (int imo = 1; imo <= state.nslmod; imo++) {
            Drho_deb_Dgamma[imo] = 0.0;
            Drho_for_Dgamma[imo] = 0.0;
        }
        
        int is = 0;
        for (int imo = 1; imo <= state.nslmod; imo++) {
            double aK2 = (state.aK1[imo] * state.chi_inter * state.BURG[imo] / state.ACTENER[imo]) *
                         (1.0 - (CommonConstants.BOLTZ * state.temp_s /
                         (state.DRAG[imo] * 1.0e6 * Math.pow(state.BURG[imo], 3))) *
                         Math.log(state.edot_macro / state.edot_zero[imo]));
            
            for (int i = 1; i <= state.nsm[imo]; i++) {
                is++;
                Drho_for_Dgamma[is] = state.aK1[imo] * Math.sqrt(grain.rho_for[is]) - aK2 * grain.rho_for[is];
                Drho_deb_Dgamma[is] = aK2 * grain.rho_for[is] * state.BURG[imo] *
                                      Math.sqrt(grain.rho_deb) * state.a_deb[imo] * state.Q_rate;
                
                if (Drho_for_Dgamma[is] < 0.0) Drho_for_Dgamma[is] = 0.0;
                if (Drho_deb_Dgamma[is] < 0.0) Drho_deb_Dgamma[is] = 0.0;
            }
        }
    }

    @Override
    public void calculateHardening(Grain grain, SimulationState state) {
        calculateDerivatives(grain, state);
        
        for (int ns1 = 1; ns1 <= grain.nact; ns1++) {
            int n1 = grain.iact[ns1];
            int imode1 = state.iSysMode[n1];
            
            for (int ns2 = 1; ns2 <= grain.nact; ns2++) {
                int n2 = grain.iact[ns2];
                int imode2 = state.iSysMode[n2];
                state.hd[n1][n2] = 0.0;
                
                if (state.iTwinSys[n1] != 1 && state.iTwinSys[n2] != 1) { // SLIP:SLIP
                    state.hd[n1][n2] += Drho_for_Dgamma[n1] * state.chi_inter * state.BURG[imode1] *
                                        state.shearmod[imode1] / (2.0 * Math.sqrt(grain.rho_for[n1]));
                    state.hd[n1][n2] -= 0.086 * state.BURG[imode1] * state.shearmod[imode1] *
                                        (Math.log(state.BURG[imode1] * Math.sqrt(grain.rho_deb)) + 1.0) *
                                        Drho_deb_Dgamma[n2] / (2.0 * Math.sqrt(grain.rho_deb));
                } else if (state.iTwinSys[n1] == 1 && state.iTwinSys[n2] != 1) { // TWIN:SLIP
                    state.hd[n1][n2] = state.shearmod[imode1] * state.burg_tw[imode1 - state.nslmod] * state.BURG[imode2] * state.TLATENT[imode2][imode1 - state.nslmod] *
                                       Drho_for_Dgamma[n2];
                } else if (state.iTwinSys[n1] != 1 && state.iTwinSys[n2] == 1) { // SLIP:TWIN
                    state.hd[n1][n2] = state.shearmod[imode1] * state.burg_tw[imode2 - state.nslmod] * state.BURG[imode1] * state.TLATENT[imode1][imode2 - state.nslmod] *
                                       Drho_for_Dgamma[n1];
                }
                
                if (state.hd[n1][n2] < 1.0) state.hd[n1][n2] = 1.0;
            }
        }
    }

    @Override
    public void updateState(Grain grain, SimulationState state) {
        calculateDerivatives(grain, state);
        
        for (int imo = 1; imo <= state.nslmod; imo++) {
            for (int i = 1; i <= state.nsm[imo]; i++) {
                int is = state.mode_slip[imo][i];
                grain.rho_for[is] += Drho_for_Dgamma[is] * grain.gamd[is];
                if (grain.rho_for[is] < state.rho_ini_for[imo]) {
                    grain.rho_for[is] = state.rho_ini_for[imo];
                }
                
                grain.rho_deb += Drho_deb_Dgamma[is] * grain.gamd[is];
                if (grain.rho_deb < state.rho_ini_deb[imo]) {
                    grain.rho_deb = state.rho_ini_deb[imo];
                }
            }
        }
    }
}
