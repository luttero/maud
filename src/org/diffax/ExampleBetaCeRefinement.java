package org.diffax;

import java.nio.file.Path;

/** Minimal example of repeated in-memory beta-Ce fault updates. */
public final class ExampleBetaCeRefinement {
    public static void main(String[] args) throws Exception {
        DiffaxModel model = new DiffaxModel();
        model.loadInput(Path.of("beta_Ce_PL_combined_9fault_template.diff"));
        BetaCePrasadLeleRefinement ce = new BetaCePrasadLeleRefinement(model);

        // Example: start by refining only the visually dominant c fault.
        BetaCePrasadLeleRefinement.Parameters p =
            new BetaCePrasadLeleRefinement.Parameters(
                0.05, // c
                0.00, // h
                0.00, // 2c
                0.00, // 2h
                0.00, // 3c
                0.00, // 3h
                0.00, // ch
                0.00, // 4c
                0.00  // cch
            );
        ce.apply(p);

        DiffaxModel.PowderResult r = ce.powder(2.0, 3.1, 0.005, true);
        for (int i=0; i<r.twoThetaDeg.length; i++)
            System.out.printf(java.util.Locale.ROOT, "%.6f %.12g%n",
                              r.twoThetaDeg[i], r.broadenedIntensity[i]);
    }
}
