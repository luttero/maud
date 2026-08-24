package org.diffax;

import java.util.Arrays;

/**
 * Nine-parameter Prasad-Lele stacking-fault model for dhcp beta-Ce,
 * extended with one symmetry-consistent lateral DIFFaX stacking-uncertainty
 * ("Fats-Waller") parameter Cperp.
 *
 * <p>Cperp is applied only to fault-associated transitions. Normal ABAC
 * transitions remain perfectly registered. For the hexagonal basal plane:</p>
 *
 * <pre>
 * C11 = C22 = Cperp
 * C12 = -Cperp/2
 * C33 = C13 = C23 = 0
 * </pre>
 *
 * <p>This makes the in-plane damping proportional to h^2 + h k + k^2 and
 * therefore preserves hexagonal symmetry. Cperp has units of Angstrom^2 in
 * the DIFFaX convention because it multiplies reciprocal-metric terms in
 * Angstrom^-2.</p>
 */
public final class BetaCePrasadLeleRefinement {
    public static final int STATE_COUNT = 14;

    /** Physical fault probabilities. Values are probabilities per eligible layer. */
    public static final class Parameters {
        public final double c, h, twoC, twoH, threeC, threeH, ch, fourC, cch;

        public Parameters(double c, double h, double twoC, double twoH,
                          double threeC, double threeH, double ch,
                          double fourC, double cch) {
            this.c = c; this.h = h; this.twoC = twoC; this.twoH = twoH;
            this.threeC = threeC; this.threeH = threeH; this.ch = ch;
            this.fourC = fourC; this.cch = cch;
        }

      public Parameters(double[] faults) {
        this.c = faults[0]; this.h = faults[1]; this.twoC = faults[2]; this.twoH = faults[3];
        this.threeC = faults[4]; this.threeH = faults[5]; this.ch = faults[6];
        this.fourC = faults[7]; this.cch = faults[8];
      }

      public static Parameters perfect() { return new Parameters(0,0,0,0,0,0,0,0,0); }

        public double cTypeFaultSum() { return h + twoH + threeH + ch + cch; }
        public double hTypeFaultSum() { return c + twoC + threeC + ch + fourC + cch; }

        public Parameters normalizedToPhysicalRegion() {
            validateFiniteNonNegative();
            double s = Math.max(cTypeFaultSum(), hTypeFaultSum());
            if (s <= 1.0) return this;
            double f = (1.0 - 1.0e-12) / s;
            return new Parameters(c*f,h*f,twoC*f,twoH*f,threeC*f,threeH*f,ch*f,fourC*f,cch*f);
        }

        public void validatePhysical() {
            validateFiniteNonNegative();
            if (cTypeFaultSum() > 1.0 + 1.0e-12)
                throw new IllegalArgumentException("c-type fault probabilities sum to " + cTypeFaultSum() + " > 1");
            if (hTypeFaultSum() > 1.0 + 1.0e-12)
                throw new IllegalArgumentException("h-type fault probabilities sum to " + hTypeFaultSum() + " > 1");
        }

        private void validateFiniteNonNegative() {
            double[] v = {c,h,twoC,twoH,threeC,threeH,ch,fourC,cch};
            for (double x : v)
                if (!Double.isFinite(x) || x < 0.0)
                    throw new IllegalArgumentException("Fault probabilities must be finite and >= 0");
        }

        @Override public String toString() {
            return String.format(java.util.Locale.ROOT,
                    "c=%.6g h=%.6g 2c=%.6g 2h=%.6g 3c=%.6g 3h=%.6g ch=%.6g 4c=%.6g cch=%.6g",
                    c,h,twoC,twoH,threeC,threeH,ch,fourC,cch);
        }
    }

    /** Complete refinement state: nine fault probabilities plus lateral Cperp. */
    public static final class RefinementParameters {
        public final Parameters faults;
        public final double[] cPerp;

        public RefinementParameters(Parameters faults, double[] cPerp) {
            if (faults == null) throw new NullPointerException("faults");
//            if (!Double.isFinite(cPerp) || cPerp < 0.0)
//                throw new IllegalArgumentException("Cperp must be finite and >= 0");
            this.faults = faults;
            this.cPerp = cPerp;
        }

        public static RefinementParameters perfect() {
            return new RefinementParameters(Parameters.perfect(),
                new double[]{0.0,0.0,0.0,0.0,0.0,0.0});
        }

        @Override public String toString() {
            return faults + String.format(java.util.Locale.ROOT, " Cperp=%.6g A^2", cPerp);
        }
    }

    private final DiffaxModel model;
    private Parameters parameters = Parameters.perfect();
    private double[] cPerp = new double[]{0.0,0.0,0.0,0.0,0.0,0.0};

    public BetaCePrasadLeleRefinement(DiffaxModel model) {
        if (model == null) throw new NullPointerException("model");
        if (model.nLayers != STATE_COUNT)
            throw new IllegalArgumentException("Expected a 14-state beta-Ce template, found " + model.nLayers + " states");
        this.model = model;
    }

    public Parameters parameters() { return parameters; }
    public double[] lateralCPerp() { return cPerp; }
    public RefinementParameters refinementParameters() { return new RefinementParameters(parameters, cPerp); }

    /** Backward-compatible strict update with no Fats-Waller uncertainty. */
    public synchronized void apply(Parameters p) { apply(new RefinementParameters(p, new double[]{0.0,0.0,0.0,0.0,0.0,0.0})); }

    /** Strict update of both fault probabilities and lateral Cperp. */
    public synchronized void apply(RefinementParameters rp) {
        rp.faults.validatePhysical();
        applyInternal(rp.faults, rp.cPerp);
    }

    /** Convenience strict update. */
    public synchronized void apply(Parameters p, double[] cPerp) {
        apply(new RefinementParameters(p, cPerp));
    }

    /** Backward-compatible probability normalization, with Cperp = 0. */
    public synchronized Parameters applyNormalized(Parameters trial) {
        Parameters p = trial.normalizedToPhysicalRegion();
        applyInternal(p, new double[]{0.0,0.0,0.0,0.0,0.0,0.0});
        return p;
    }

    /** Normalize only the probability simplex; Cperp remains unchanged. */
    public synchronized RefinementParameters applyNormalized(RefinementParameters trial) {
        Parameters p = trial.faults.normalizedToPhysicalRegion();
        applyInternal(p, trial.cPerp);
        return new RefinementParameters(p, trial.cPerp);
    }

    private void applyInternal(Parameters p, double[] lateralCPerp) {
        clearTransitionsAndUncertainty();

        final double normalC = 1.0 - p.cTypeFaultSum();
        final double normalH = 1.0 - p.hTypeFaultSum();

        // P1 states: current layer is c-type; perfect step is + to P2+ (state 3).
        setCTypeRow(1, normalC, p, lateralCPerp, 3, 6, 8, 2, 4, 11);
        setCTypeRow(2, normalC, p, lateralCPerp, 3, 6, 8, 2, 4, 11);

        // P2 states: current layer is h-type; perfect step is - to P3- (state 6).
        setHTypeRow(3, normalH, p, lateralCPerp, 6, 3, 1, 7, 5, 9, 12);
        setHTypeRow(4, normalH, p, lateralCPerp, 6, 3, 1, 7, 5, 9, 12);

        // P3 states: current layer is c-type; perfect step is - to P4- (state 8).
        setCTypeRowMinus(5, normalC, p, lateralCPerp, 8, 1, 3, 5, 7, 13);
        setCTypeRowMinus(6, normalC, p, lateralCPerp, 8, 1, 3, 5, 7, 13);

        // P4 states: current layer is h-type; perfect step is + to P1+ (state 1).
        setHTypeRowPlus(7, normalH, p, lateralCPerp, 1, 8, 6, 4, 2, 10, 14);
        setHTypeRowPlus(8, normalH, p, lateralCPerp, 1, 8, 6, 4, 2, 10, 14);

        // Deterministic exits of temporary extrinsic states are part of the fault event.
        add(9,  7, 1.0, +1, p.fourC > 0.0, lateralCPerp); // E4c(P2)
        add(10, 4, 1.0, -1, p.fourC > 0.0, lateralCPerp); // E4c(P4)
        add(11, 4, 1.0, -1, p.cch   > 0.0, lateralCPerp); // Ecch(P1)
        add(12, 5, 1.0, +1, p.cch   > 0.0, lateralCPerp); // Ecch(P2)
        add(13, 7, 1.0, +1, p.cch   > 0.0, lateralCPerp); // Ecch(P3)
        add(14, 2, 1.0, -1, p.cch   > 0.0, lateralCPerp); // Ecch(P4)

        refreshStackingState();
        refreshUncertaintyFlags();
        parameters = p;
        cPerp = lateralCPerp;
    }

    private void setCTypeRow(int from, double normal, Parameters p, double[] cp,
                             int normalTo, int hTo, int twoHTo, int threeHTo, int chTo, int cchTo) {
        add(from, normalTo, normal, +1, false, cp);
        add(from, hTo,      p.h,      -1, true, cp);
        add(from, twoHTo,   p.twoH,   -1, true, cp);
        add(from, threeHTo, p.threeH, -1, true, cp);
        add(from, chTo,     p.ch,     -1, true, cp);
        add(from, cchTo,    p.cch,    -1, true, cp);
    }

    private void setCTypeRowMinus(int from, double normal, Parameters p, double[] cp,
                                  int normalTo, int hTo, int twoHTo, int threeHTo, int chTo, int cchTo) {
        add(from, normalTo, normal, -1, false, cp);
        add(from, hTo,      p.h,      +1, true, cp);
        add(from, twoHTo,   p.twoH,   +1, true, cp);
        add(from, threeHTo, p.threeH, +1, true, cp);
        add(from, chTo,     p.ch,     +1, true, cp);
        add(from, cchTo,    p.cch,    +1, true, cp);
    }

    private void setHTypeRow(int from, double normal, Parameters p, double[] cp,
                             int normalTo, int cTo, int twoCTo, int threeCTo,
                             int chTo, int fourCTo, int cchTo) {
        add(from, normalTo, normal, -1, false, cp);
        add(from, cTo,      p.c,      +1, true, cp);
        add(from, twoCTo,   p.twoC,   +1, true, cp);
        add(from, threeCTo, p.threeC, +1, true, cp);
        add(from, chTo,     p.ch,     +1, true, cp);
        add(from, fourCTo,  p.fourC,  +1, true, cp);
        add(from, cchTo,    p.cch,    +1, true, cp);
    }

    private void setHTypeRowPlus(int from, double normal, Parameters p, double[] cp,
                                 int normalTo, int cTo, int twoCTo, int threeCTo,
                                 int chTo, int fourCTo, int cchTo) {
        add(from, normalTo, normal, +1, false, cp);
        add(from, cTo,      p.c,      -1, true, cp);
        add(from, twoCTo,   p.twoC,   -1, true, cp);
        add(from, threeCTo, p.threeC, -1, true, cp);
        add(from, chTo,     p.ch,     -1, true, cp);
        add(from, fourCTo,  p.fourC,  -1, true, cp);
        add(from, cchTo,    p.cch,    -1, true, cp);
    }

    private void clearTransitionsAndUncertainty() {
        for (int from=1; from<=STATE_COUNT; from++) {
            for (int to=1; to<=STATE_COUNT; to++) {
                model.lAlpha[to][from] = 0.0;
                model.lR[1][to][from] = 0.0;
                model.lR[2][to][from] = 0.0;
                model.lR[3][to][from] = 0.0;
                model.rB11[to][from] = 0.0;
                model.rB22[to][from] = 0.0;
                model.rB33[to][from] = 0.0;
                model.rB12[to][from] = 0.0;
                model.rB31[to][from] = 0.0;
                model.rB23[to][from] = 0.0;
                model.there[to][from] = false;
                model.bsZero[to][from] = true;
            }
        }
    }

    private void add(int from, int to, double probability, int sign,
                     boolean faultAssociated, double[] cp) {
        if (probability <= 0.0) return;
        model.lAlpha[to][from] += probability;
        model.lR[1][to][from] = sign > 0 ?  2.0/3.0 : -2.0/3.0;
        model.lR[2][to][from] = sign > 0 ?  1.0/3.0 : -1.0/3.0;
        model.lR[3][to][from] = 1.0;
        if (faultAssociated) {
            // Hexagonal-isotropic basal-plane stacking-vector covariance.
            model.rB11[to][from] = cp[0];
            model.rB22[to][from] = cp[1];
            model.rB12[to][from] = cp[3];
            // Purely lateral first test: no z variance and no x-z / y-z correlation.
            model.rB33[to][from] = cp[2];
            model.rB31[to][from] = cp[4];
            model.rB23[to][from] = cp[5];
        }
    }

    private void refreshStackingState() {
        for (int from=1; from<=STATE_COUNT; from++) {
            double sum = 0.0;
            for (int to=1; to<=STATE_COUNT; to++) {
                double a = model.lAlpha[to][from];
                sum += a;
                model.there[to][from] = a >= model.EPS7;
            }
            if (Math.abs(sum - 1.0) > 1.0e-10)
                throw new IllegalStateException("Transition row " + from + " sums to " + sum);
        }
        if (!model.getG())
            throw new IllegalStateException("DIFFaX GET_G failed for beta-Ce fault probabilities");
        model.lRz = 1.0;
        model.sameRz = true;
    }

    /** Reproduce the uncertainty-related part of DIFFaX OPTIMZ after an in-memory update. */
    private void refreshUncertaintyFlags() {
        model.allBsZero = true;
        for (int from=1; from<=STATE_COUNT; from++) {
            for (int to=1; to<=STATE_COUNT; to++) {
                boolean zero = model.rB11[to][from] == 0.0 && model.rB22[to][from] == 0.0 &&
                               model.rB33[to][from] == 0.0 && model.rB12[to][from] == 0.0 &&
                               model.rB23[to][from] == 0.0 && model.rB31[to][from] == 0.0;
                model.bsZero[to][from] = zero;
                if (model.there[to][from]) model.allBsZero &= zero;
            }
        }

        double[] av = new double[6];
        double[] err = new double[6];
        int n = 0;
        for (int from=1; from<=STATE_COUNT; from++) {
            for (int to=1; to<=STATE_COUNT; to++) if (model.there[to][from]) {
                av[0] += model.rB11[to][from]; av[1] += model.rB22[to][from];
                av[2] += model.rB33[to][from]; av[3] += model.rB12[to][from];
                av[4] += model.rB23[to][from]; av[5] += model.rB31[to][from];
                n++;
            }
        }
        if (n > 0) for (int q=0; q<6; q++) av[q] /= n;
        for (int from=1; from<=STATE_COUNT; from++) {
            for (int to=1; to<=STATE_COUNT; to++) if (model.there[to][from]) {
                err[0] += Math.abs(model.rB11[to][from] - av[0]);
                err[1] += Math.abs(model.rB22[to][from] - av[1]);
                err[2] += Math.abs(model.rB33[to][from] - av[2]);
                err[3] += Math.abs(model.rB12[to][from] - av[3]);
                err[4] += Math.abs(model.rB23[to][from] - av[4]);
                err[5] += Math.abs(model.rB31[to][from] - av[5]);
            }
        }
        model.sameBs = true;
        for (int q=0; q<6; q++) {
            double tol = Math.abs(model.EPS3 * av[q]);
            if (Math.abs(err[q]) > tol) model.sameBs = false;
        }
        model.aB11 = av[0]; model.aB22 = av[1]; model.aB33 = av[2];
        model.aB12 = av[3]; model.aB23 = av[4]; model.aB31 = av[5];
    }

    public synchronized DiffaxModel.PowderResult powder(double min2ThetaDeg, double max2ThetaDeg,
                                                        double stepDeg, boolean adaptive) {
        return model.computePowderPattern(min2ThetaDeg, max2ThetaDeg, stepDeg, adaptive);
    }

    public synchronized double pointIntensity(int h, int k, double l) {
        return model.pointIntensity(h, k, l);
    }

    public synchronized double[] statePopulations() {
        return Arrays.copyOfRange(model.lG, 1, STATE_COUNT + 1);
    }
}
