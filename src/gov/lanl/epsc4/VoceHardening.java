package gov.lanl.epsc4;

import java.util.Scanner;

/**
 * Voce hardening model implementation (kCL = 0).
 */
public class VoceHardening implements HardeningModel {
    
    // Voce-specific parameters
    private double[] hselfx;
    private double[][] hlatex;
    private double tau0x, tau1x, thet0x, thet1x, twvol, gamdthres, fact, voce;

    @Override
    public void readParameters(Scanner s, SimulationState state) {
        hselfx = new double[state.NMOD + 1];
        hlatex = new double[state.NMOD + 1][state.NMOD + 1];

        int isys = 1;
        for (int im = 1; im <= state.nmodes; im++) {
            s.nextLine(); // Skip comment
            twvol = 0.0;
            gamdthres = 0.0;
            if (state.itw[im] == 1) {
                String[] line = s.nextLine().trim().split("\\s+");
                twvol = Double.parseDouble(line[0]);
                gamdthres = Double.parseDouble(line[1]);
            }
            String[] line = s.nextLine().trim().split("\\s+");
            tau0x = Double.parseDouble(line[0]);
            tau1x = Double.parseDouble(line[1]);
            thet0x = Double.parseDouble(line[2]);
            thet1x = Double.parseDouble(line[3]);

            hselfx[im] = 1.0;
            line = s.nextLine().trim().split("\\s+");
            for (int jm = 1; jm <= state.nmodes; jm++) {
                hlatex[im][jm] = Double.parseDouble(line[jm - 1]);
            }

            state.TwinFrac[im] = twvol;
            state.TwinCRSS[im] = gamdthres;

            if (thet1x < 0.001 * tau0x) thet1x = 0.001 * tau0x;
            if (tau1x < 0.001 * tau0x) {
                tau1x = 0.0;
                thet0x = thet1x;
            }

            for (int is = 1; is <= state.nsm[im]; is++) {
                state.tau0[isys] = tau0x;
                state.tau1[isys] = tau1x;
                state.thet0[isys] = thet0x;
                state.thet1[isys] = thet1x;

                int jsys = 1;
                for (int jm = 1; jm <= state.nmodes; jm++) {
                    for (int js = 1; js <= state.nsm[jm]; js++) {
                        state.h[isys][jsys] = hlatex[im][jm];
                        jsys++;
                    }
                }
                state.h[isys][isys] = hselfx[im];
                isys++;
            }
        }
    }

    @Override
    public void initializeGrain(Grain grain, SimulationState state) {
        grain.gamtot = 0.0;
        for (int is = 1; is <= state.nsys; is++) {
            grain.tau[is] = state.tau0[is];
        }
    }

    @Override
    public void calculateHardening(Grain grain, SimulationState state) {
        for (int n1 = 1; n1 <= state.nsys; n1++) {
            voce = state.thet1[n1];
            if (state.tau1[n1] > 0.001 * state.tau0[n1]) {
                thet0x = state.thet0[n1];
                thet1x = state.thet1[n1];
                fact = grain.gamtot * thet0x / state.tau1[n1];
                voce += (thet0x - thet1x + thet1x * fact) * Math.exp(-fact);
            }
            for (int ns2 = 1; ns2 <= grain.nact; ns2++) {
                int n2 = grain.iact[ns2];
                state.hd[n1][n2] = voce * state.h[n1][n2];
            }
        }
    }

    @Override
    public void updateState(Grain grain, SimulationState state) {
        // Voce model has no separate state update step (it's all in tau)
    }
}
