package gov.lanl.epsc4;

import java.util.List;

/**
 * Contains the core iterative solver routines for the elasto-plastic problem.
 */
public class Solver {

    // --- Work arrays for sc_new ---
    private double[][] e2 = new double[7][7];
    private double[][] e2i = new double[7][7], anew = new double[7][7];
    private double[][] aux21_sc = new double[7][7], aux22_sc = new double[7][7];
    private double[][] aux23_sc = new double[7][7], aux24_sc = new double[7][7];
    private double[][] aux25_sc = new double[7][7], aux26_sc = new double[7][7];
    private double[][] aux27_sc = new double[7][7], aux28_sc = new double[7][7];
    private double[][] aux29_sc = new double[7][7];
    private double[] aux11_sc = new double[7], aux12_sc = new double[7];
    private double[][][][] ass4_sc = new double[4][4][4][4];
    private double[][][][] esim4_sc = new double[4][4][4][4];
    private double[] axb_sc = new double[4];
    private double[][] EIGB_sc = new double[4][4];
    
    // --- Work arrays for g_modulus ---
    private double[][] x_gmod, y_gmod;
    private int[] indx_gmod;
    
    // --- Work arrays for s_state ---
    private double[] aux11_sstate = new double[7];
    private double[][] aux21_sstate = new double[7][7];
    private int[] indx_sstate = new int[7];

    // --- Work arrays for g_actsys ---
    private double[] rss_act;

    public Solver(int nsls) {
        // Allocate solver-specific work arrays
        x_gmod = new double[nsls + 1][nsls + 1];
        y_gmod = new double[nsls + 1][nsls + 1];
        indx_gmod = new int[nsls + 1];
        rss_act = new double[nsls + 1];
    }

    /**
     * Solves the self-consistent equation.
     */
    public boolean scNew(int iopt, int liter, int interaction, int istep,
                         SimulationState state, List<Grain> grains, EshelbyTensorCalculator eshelby) {
        
        int iguess = 1;
        int niter = (iopt == 0) ? state.itmax_mod : 1;
        boolean converged = false;

        while (iguess <= niter) {
            TensorUtils.voigt(null, null, state.ass2, ass4_sc, 3);
            updateShape(state, grains); 
            
            for (int j = 1; j <= 3; j++) {
                axb_sc[j] = state.axisph[0][j];
                for (int i = 1; i <= 3; i++) EIGB_sc[i][j] = state.axisph[i][j];
            }
            
            TensorUtils.stiffnessRotation(ass4_sc, EIGB_sc, axb_sc, esim4_sc, state.ESCR4, eshelby);
            TensorUtils.voigt(null, null, e2, esim4_sc, 4);
            TensorUtils.invten(e2, ModScNew.e2inv, state.invfac);
            TensorUtils.voigt(null, null, ModScNew.e2inv, state.EINVSA, 3);
            
            if (state.ishape >= 2) {
                // ... (Logic for per-grain shape as in previous step) ...
            } 

            TensorUtils.invten(e2, e2i, state.invfac);
            for (int i = 1; i <= 6; i++) {
                for (int j = 1; j <= 6; j++) {
                    state.aef[i][j] = 0.0;
                    for (int k = 1; k <= 6; k++) {
                        state.aef[i][j] += state.ass2[i][k] * (e2i[k][j] - state.id2[k][j]) * state.profac[k];
                    }
                }
            }
            
            double tmis = TensorUtils.tmismatch(state.ass2, state.css2, 6, 6);
            double xmmin = 1.0; // HARD-WIRED SECANT
            
            mEffective(grains, state, xmmin);
            
            // --- Calculates localization and elasto-plastic stiffness tensors ---
            for (int i = 1; i <= 6; i++) {
                aux11_sc[i] = 0.0;
                for (int j = 1; j <= 6; j++) {
                    aux22_sc[i][j] = 0.0; aux26_sc[i][j] = 0.0;
                    aux29_sc[i][j] = 0.0;
                }
            }
            
            for (Grain g : grains) {
                if (g.wgt > 0) {
                    // ... (Full implementation from sc_new grain loop) ...
                    // ... (calculating aux24, aux25, aux21, aux23) ...
                    // ... (accumulating aux26, aux22, aux11, aux29) ...
                } 
            }
            
            TensorUtils.invten(aux26_sc, aux27_sc, state.invfac);
            
            for (int i = 1; i <= 6; i++) {
                for (int j = 1; j <= 6; j++) {
                    anew[i][j] = 0.0;
                    for (int k = 1; k <= 6; k++) {
                        anew[i][j] += aux22_sc[i][k] * aux27_sc[k][j] * state.profac[k];
                    }
                }
            }
            for (int i = 1; i <= 6; i++) {
                for (int j = i + 1; j <= 6; j++) {
                    anew[i][j] = 0.5 * (anew[i][j] + anew[j][i]);
                    anew[j][i] = anew[i][j];
                }
            }

            double error = TensorUtils.tmismatch(state.ass2, anew, 6, 6);
            for (int i = 1; i <= 6; i++) System.arraycopy(anew[i], 1, state.ass2[i], 1, 6);
            
            int iter_num = (iopt == 0) ? iguess : liter;
            if (IOUtils.writer11 != null) IOUtils.writer11.printf("For iteration = %3d the error is %12.4e%n", iter_num, error);
            System.out.printf("ITER: %3d   ERROR: %12.4e   NG: %5d%n", iter_num, error, state.NGR);
            
            converged = (error <= state.error_mod);
            if (iopt == 0) {
                if (converged) {
                    iguess = niter + 1;
                } else if (iguess == niter) {
                    throw new RuntimeException("CONVERGENCE IN SC NOT ACHIEVED AFTER " + niter + " ITERATIONS");
                }
                iguess++;
            } else {
                if (converged && liter > 1) iguess = niter + 1;
                else iguess++;
            }
        } 

        if (iopt != 1) {
            TensorUtils.invten(aux29_sc, aux24_sc, state.invfac);
            TensorUtils.invten(anew, aux22_sc, state.invfac); // anew is the converged ass2
            for (int i = 1; i <= 6; i++) {
                state.alfass[i] = 0.0;
                for (int j = 1; j <= 6; j++) {
                    for (int k = 1; k <= 6; k++) {
                        state.alfass[i] += aux22_sc[i][j] * aux24_sc[j][k] * aux11_sc[k] * state.profac[j] * state.profac[k];
                    }
                }
            }
        }
        return converged;
    }

    /**
     * Calculates the single-crystal elasto-plastic incremental stiffness 'acs2'.
     */
    public void gModulus(List<Grain> grains, SimulationState state, HardeningModel hardeningModel) {
        final double TOLER_DET = 1.0e-20;
        
        for (Grain g : grains) {
            int igverify = 0;
            if (g.nact == 0) {
                for (int i = 1; i <= 6; i++) {
                    for (int j = 1; j <= 6; j++) {
                        g.acs2[i][j] = g.ccs2[i][j] - g.stcs[i] * state.i6[j];
                    }
                }
                igverify = 1;
                gState(g, state);
            } else {
                while (igverify == 0 && g.nact > 0) {
                    hardeningModel.calculateHardening(g, state);
                    
                    for (int ns1 = 1; ns1 <= g.nact; ns1++) {
                        int n1 = g.iact[ns1];
                        for (int ns2 = 1; ns2 <= g.nact; ns2++) {
                            int n2 = g.iact[ns2];
                            x_gmod[ns1][ns2] = state.hd[n1][n2];
                            for (int i = 1; i <= 6; i++) {
                                for (int j = 1; j <= 6; j++) {
                                    x_gmod[ns1][ns2] += g.mcs[i][n1] * g.mcs[j][n2] * g.ccs2[i][j] 
                                                      * state.profac[i] * state.profac[j];
                                }
                            }
                        }
                    }

                    double d = TensorUtils.ludcmpc(x_gmod, g.nact, state.NSLS, indx_gmod);
                    for (int ns1 = 1; ns1 <= g.nact; ns1++) d *= x_gmod[ns1][ns1];
                    
                    if (Math.abs(d) < TOLER_DET) {
                        int idelsys = (int) (RandomUtils.ran2(state.jran) * g.nact) + 1;
                        if (idelsys < g.nact) {
                            for (int ns1 = idelsys; ns1 < g.nact; ns1++) {
                                g.iact[ns1] = g.iact[ns1 + 1];
                            }
                        }
                        g.nact--;
                        System.out.println(g.id + " DET=0 --> look for other combination");
                    } else {
                        // ... (Full implementation of matrix inversion and f, aux21, acs2 calculation) ...
                        
                        gState(g, state);
                        IntHolder igverifyHolder = new IntHolder(igverify);
                        gVerify(g, state, igverifyHolder, hardeningModel);
                        igverify = igverifyHolder.value;
                    }
                } // end while

                if (g.nact == 0) {
                    for (int i = 1; i <= 6; i++) {
                        for (int j = 1; j <= 6; j++) {
                            g.acs2[i][j] = g.ccs2[i][j] - g.stcs[i] * state.i6[j];
                        }
                    }
                    igverify = 1;
                    gState(g, state);
                }
            }
        }
    }

    /**
     * Identifies active slip systems in each grain.
     */
    public int gActsys(int nout_old, List<Grain> grains, SimulationState state) {
        final double slack = 0.98;
        int nout = 0;
        
        for (Grain g : grains) {
            double delt = 0.0;
            int nout_flag = 0;
            g.nact = 0;
            
            for (int ns1 = 1; ns1 <= state.nsys; ns1++) {
                rss_act[ns1] = 0.0;
                for (int i = 1; i <= 6; i++) {
                    rss_act[ns1] += g.mcs[i][ns1] * g.stcs[i] * state.profac[i];
                }
                rss_act[ns1] /= g.tau[ns1];
                
                if (rss_act[ns1] >= slack) {
                    g.nact++;
                    g.iact[g.nact] = ns1;
                    if (rss_act[ns1] > 1.0) {
                        nout_flag = 1;
                        g.tau[ns1] = rss_act[ns1] * g.tau[ns1];
                        delt += ((rss_act[ns1] - 1.0) * (rss_act[ns1] - 1.0));
                        g.ng_update = 1;
                    }
                }
            }
            delt = Math.sqrt(delt);
            g.tau_update[1] *= (delt + 1.0); 
            if (nout_flag == 1) nout++;
        } 
        
        if (nout > nout_old) {
            System.out.println("\nWARNING\nSTRESS IS OUT OF THE SCYS FOR " + nout + " GRAINS");
            nout_old = nout;
        }
        return nout_old;
    }

    /**
     * Verifies the active loading condition.
     */
    public void gVerify(Grain g, SimulationState state, IntHolder igverify, HardeningModel hardeningModel) {
        final double ERROR_LOAD = 0.001;
        igverify.value = 1;
        if (g.nact == 0) return;

        for (int ns1 = 1; ns1 <= state.nsys; ns1++) g.gamd[ns1] = 0.0;
        
        for (int ns1 = 1; ns1 <= g.nact; ns1++) {
            int n1 = g.iact[ns1];
            for (int i = 1; i <= 6; i++) {
                g.gamd[n1] += g.f[i][ns1] * (g.etrcs[i] - g.alfacs[i] * state.deltemp) * state.profac[i];
            }
        }

        int ns1 = 1;
        while (ns1 <= g.nact) {
            int n1 = g.iact[ns1];
            if (g.gamd[n1] < 0.0) {
                igverify.value = 0;
                g.nact--;
                for (int ns2 = ns1; ns2 <= g.nact; ns2++) g.iact[ns2] = g.iact[ns2 + 1];
                ns1 = g.nact; // End loop
            }
            ns1++;
        }

        if (igverify.value != 0) {
            int neload = 0;
            hardeningModel.calculateHardening(g, state);

            for (ns1 = 1; ns1 <= state.nsys; ns1++) {
                g.taud[ns1] = 0.0;
                for (int ns2 = 1; ns2 <= g.nact; ns2++) {
                    int n2 = g.iact[ns2];
                    g.taud[ns1] += state.hd[ns1][n2] * g.gamd[n2];
                }
            }
            
            for (ns1 = 1; ns1 <= g.nact; ns1++) {
                int n1 = g.iact[ns1];
                double rssd = 0.0;
                for (int i = 1; i <= 6; i++) {
                    rssd += g.mcs[i][n1] * g.strcs[i] * state.profac[i];
                }
                double control_load = Math.abs((rssd - g.taud[n1]) / g.taud[n1]);
                if (control_load > ERROR_LOAD) neload++;
            }
            
            if (neload != 0) {
                int idelsys = (int) (RandomUtils.ran2(state.jran) * g.nact) + 1;
                g.nact--;
                if (idelsys > 0 && idelsys <= g.nact) {
                    for (ns1 = idelsys; ns1 <= g.nact; ns1++) g.iact[ns1] = g.iact[ns1 + 1];
                }
                igverify.value = 0;
            }
        }
    }

    /**
     * Evaluates the macroscopic (sample) stress and strain rates.
     */
    public void sState(SimulationState state) {
        for (int i = 1; i <= 6; i++) {
            aux11_sstate[i] = -1.0 * state.istbc[i] * state.strbc[i];
            for (int j = 1; j <= 6; j++) {
                aux11_sstate[i] += state.ass2[i][j]
                          * (state.ietbc[j] * state.etrbc[j] - state.alfass[j] * state.deltemp)
                          * state.profac[j];
                aux21_sstate[i][j] = state.ietbc[j] * ((i == j) ? 1.0 : 0.0)
                            - state.istbc[j] * state.ass2[i][j] * state.profac[j];
            }
        }
        
        TensorUtils.ludcmpc(aux21_sstate, 6, 6, indx_sstate);
        TensorUtils.lubksbc(aux21_sstate, 6, 6, indx_sstate, aux11_sstate);
        
        for (int i = 1; i <= 6; i++) {
            state.etrss[i] = state.ietbc[i] * state.etrbc[i] + state.istbc[i] * aux11_sstate[i];
            state.strss[i] = state.istbc[i] * state.strbc[i] + state.ietbc[i] * aux11_sstate[i];
        }

        for (int i = 1; i <= 6; i++) {
            double aux11_term = 0.0;
            state.auxsample[i] = 0.0;
            for (int j = 1; j <= 6; j++) {
                aux11_term += (state.aef[i][j] + state.ass2[i][j]) * state.etrss[j] * state.profac[j];
                state.auxsample[i] += state.ass2[i][j] * state.alfass[j] * state.deltemp * state.profac[j];
            }
            state.auxsample[i] = aux11_term - state.auxsample[i];
        }
    }

    /**
     * Evaluates the stress rate and strain rate in a single grain.
     */
    public void gState(Grain g, SimulationState state) {
        // ... (Full implementation from g_state) ...
        // This will use g.acs2, g.alfacs, etc.
    }

    /**
     * Validates boundary conditions and sets 'icvx'.
     */
    public void loadConditions(SimulationState state) {
        // ... (Full implementation from load_conditions) ...
    }

    /**
     * Calculates the effective 'm' interaction parameter for each grain.
     */
    public void mEffective(List<Grain> grains, SimulationState state, double xmmin) {
        // ... (Full implementation from m_effective) ...
        // This will loop over 'grains' and set g.meffc
    }
    
    /**
     * Handles dislocation pileup and recoverable strain logic.
     */
    public void pileup(int iopt, SimulationState state, List<Grain> grains) {
        // ... (Full implementation from pileup) ...
        // This will operate on ModPileup static arrays
    }
}
