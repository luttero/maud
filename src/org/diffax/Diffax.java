package org.diffax;

import java.io.IOException;
import java.nio.file.Path;

/**
 * Backward-compatible CLI/static facade for the instance-based DIFFaX port.
 *
 * <p>New integrations should prefer {@link DiffaxModel}: create one instance per
 * structural model.  This facade intentionally owns one default model so code
 * written against earlier versions of the port continues to work.</p>
 */
public final class Diffax {
    private Diffax() {}

    private static final DiffaxModel DEFAULT = new DiffaxModel();

    public static void main(String[] args) throws Exception {
        DiffaxModel model = new DiffaxModel();
        model.salute();
        if (args.length == 0) {
            System.err.println("Usage: java -jar diffax-java-complete.jar file.dif [h k l | powder ... | sadp ...]");
            System.exit(2);
        }
        if (!model.runFile(args[0], args)) System.exit(2);
    }

    /** Compatibility default-model loader. Prefer new DiffaxModel().loadInput(...). */
    public static synchronized void loadInput(Path input) throws IOException { DEFAULT.loadInput(input); }
    public static synchronized void loadInput(String input) throws IOException { DEFAULT.loadInput(input); }
    public static synchronized double pointIntensity(int h, int k, double l) { return DEFAULT.pointIntensity(h, k, l); }
    public static synchronized DiffaxModel.PowderResult computePowderPattern(double minDeg, double maxDeg, double stepDeg, boolean adaptive) {
        return DEFAULT.computePowderPattern(minDeg, maxDeg, stepDeg, adaptive);
    }
    public static synchronized DiffaxModel.SadpResult computeSadp(int view, double lMax, boolean adaptive, int bits, boolean linear, double brightness) {
        return DEFAULT.computeSadp(view, lMax, adaptive, bits, linear, brightness);
    }
    public static synchronized DiffaxModel.SadpResult computeSadp(int view, double lMax, boolean adaptive, int bits, boolean linear, double brightness, int size) {
        return DEFAULT.computeSadp(view, lMax, adaptive, bits, linear, brightness, size);
    }
    public static Path writeSadpRaw(Path output, DiffaxModel.SadpResult result) throws IOException { return DEFAULT.writeSadpRaw(output, result); }
    public static Path writeSadpPng(Path output, DiffaxModel.SadpResult result) throws IOException { return DEFAULT.writeSadpPng(output, result); }
}
