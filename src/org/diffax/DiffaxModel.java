package org.diffax;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.instbroad.InstrumentBroadeningPVCaglioti;
import it.unitn.ing.rista.util.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;

/**
 * Java port scaffold for DIFFaX 1.812/1.813-era FORTRAN source.
 *
 * <p>The original code is FORTRAN 77 fixed-form and relies heavily on COMMON
 * blocks, 1-based arrays, pass-by-reference scalar arguments and labelled
 * GOTOs.  This class centralizes COMMON-block state and intentionally retains
 * 1-based indexing by allocating one extra array element.</p>
 *
 * <p>This source is a compile-tested migration foundation.  The program driver
 * and several language/utility routines are translated; numerical and file-I/O
 * routines that still require direct porting are explicitly marked with
 * UnsupportedOperationException rather than silently changing behavior.</p>
 */

public final class DiffaxModel {

  static double intensityCorrection = 0.414619964316147;

  private int stepFactor = MaudPreferences.getInteger("diffax.dividePatternStepBy", 4);;

  public void setStepFactor(int stepFactor) {
    this.stepFactor = stepFactor;
  }

  static final double[] x = {0, .095012509837637440185, .281603550779258913230,
      .458016777657227386342, .617876244402643748447,
      .755404408355003033895, .865631202387831743880,
      .944575023073232576078, .989400934991649932596};
  static final double[] w = {0, .189450610455068496285, .182603415044923588867,
      .169156519395002538189, .149595988816576732081,
      .124628971255533872052, .095158511682492784810,
      .062253523938647892863, .027152459411754094852};

    /**
     * Preferred-orientation weight supplied by the host/Rietveld program.
     * The l coordinate is continuous because faulted hk reciprocal-lattice rods
     * are integrated between nominal Bragg positions.
     */
    @FunctionalInterface
    public interface TextureFactor {
        double weight(int h, int k, double l);
    }

    private static final TextureFactor UNITY_TEXTURE = (h, k, l) -> 1.0;
    private TextureFactor textureFactor = UNITY_TEXTURE;

    /**
     * Install a texture callback. Passing null restores the untextured calculation.
     * The callback is evaluated at every quadrature point before powder-bin
     * accumulation, so broad/diffuse fault intensity is weighted consistently.
     */
    public synchronized void setTextureFactor(TextureFactor factor) {
        textureFactor = factor == null ? UNITY_TEXTURE : factor;
    }

    /** Restore the original random-powder behavior (texture factor = 1). */
    public synchronized void clearTextureFactor() {
        textureFactor = UNITY_TEXTURE;
    }

    private double textureWeight(int h, int k, double l) {
      double w = 1.0;
//      System.out.println(hasTexture);
      if (phase != null && hasTexture) {
        l *= 4;
//        double[] phicosphi = Angles.getPhicosPhi(phase, h, k, l);
        double[] phicosphi = phase.tfhkl(h, k, l);

        double beta = phicosphi[1];
        double phi = phicosphi[0];
        double dpi = 1.0 / Math.sqrt(soVector[0] * h * h + soVector[1] * k * k + soVector[2] * l * l + 2.
            * soVector[5] * h * k + 2. * soVector[3] * k * l + 2. * soVector[4] * h * l);
        double twothetaang = dataFiles[activeDatafileNumber].computeposition(dpi, lambda);
//        double[] alphabeta = dataFiles[activeDatafileNumber].getTextureAngles(dpi, lambda);
        double[] alphabeta = instrumentGeometry.getTextureAngles(dataFiles[activeDatafileNumber], tiltingAngles[activeDatafileNumber],
            sample, twothetaang, 0);
        //getGeometry().getTextureAngles(datafile, tilting_angles, sample, twotheta, ppp);
        //     double twothetaang = computeposition(dspace, wavelength);
        //    return getDataFileSet().getTextureAngles(this, getTiltingAngle(), twothetaang, 0);
        w = textureModel.computeTextureFactorQuick(phi, beta,
            alphabeta[0] * Constants.DEGTOPI, alphabeta[1] * Constants.DEGTOPI);
//        if (Math.abs(l - ((int) l)) < 1.0E-3) {
//          System.out.println("Diff: " + h + " " + k + " " + l + " " + phi * Constants.PITODEG + " " + beta * Constants.PITODEG + " " + alphabeta[0] + " " + alphabeta[1] + " " + w);
//        }
      }
/*        double w = textureFactor.weight(h, k, l);
        if (!Double.isFinite(w) || w < 0.0)
            throw new IllegalArgumentException(
                    "Texture factor must be finite and >= 0 at h=" + h +
                    " k=" + k + " l=" + l + "; got " + w);*/
        return w;
    }

    /**
     * Host-supplied powder-geometry correction. This replaces DIFFaX's built-in
     * Lorentz-polarization factor and may include absorption or any other
     * geometry-dependent multiplicative correction.
     *
     * <p>twoThetaDegrees is the actual scattering angle for the active radiation
     * component. The l coordinate is the continuous midpoint of the integrated
     * faulted hk rod interval. wavelength is the active component wavelength in
     * the same units used by the DIFFaX input (normally Angstrom).</p>
     */
    @FunctionalInterface
    public interface GeometryCorrection {
        double weight(double twoThetaDegrees, int h, int k, double l, double wavelength);
    }

    private GeometryCorrection geometryCorrection = null;

    /**
     * Replace the built-in DIFFaX powder Lorentz-polarization correction with a
     * host/Rietveld geometry correction. The callback may include LP, absorption,
     * and other multiplicative geometry terms. Passing null restores DIFFaX's
     * original correction.
     */
    public synchronized void setGeometryCorrection(GeometryCorrection correction) {
        geometryCorrection = correction;
    }

    /** Restore DIFFaX's original radiation-dependent powder LP correction. */
    public synchronized void clearGeometryCorrection() {
        geometryCorrection = null;
    }

    /** True when the host program is overriding DIFFaX's built-in powder LP term. */
    public synchronized boolean hasGeometryCorrection() {
        return geometryCorrection != null;
    }

    /** One incoherent radiation line in a multi-wavelength powder calculation. */
    public static final class RadiationComponent {
        public final double wavelength;
        public final double relativeWeight;

        public RadiationComponent(double wavelength, double relativeWeight) {
            if (!Double.isFinite(wavelength) || wavelength <= 0.0)
                throw new IllegalArgumentException("Radiation wavelength must be finite and > 0");
            if (!Double.isFinite(relativeWeight) || relativeWeight < 0.0)
                throw new IllegalArgumentException("Radiation relative weight must be finite and >= 0");
            this.wavelength = wavelength;
            this.relativeWeight = relativeWeight;
        }
    }

    private RadiationComponent[] radiationComponents = new RadiationComponent[0];

    /**
     * Set the incoherent wavelength mixture used by powder calculations. Relative
     * weights are normalized internally to sum to one, so e.g. {2,1} and
     * {0.6666667,0.3333333} are equivalent. Overall intensity remains controlled
     * by the normal DIFFaX/Rietveld scale factor.
     */
    public synchronized void setRadiationComponents(double[] wavelengths, double[] relativeWeights) {
        Objects.requireNonNull(wavelengths, "wavelengths");
        Objects.requireNonNull(relativeWeights, "relativeWeights");
        if (wavelengths.length == 0 || wavelengths.length != relativeWeights.length)
            throw new IllegalArgumentException("Wavelength and weight arrays must have the same non-zero length");
        RadiationComponent[] c = new RadiationComponent[wavelengths.length];
        double sum = 0.0;
        for (int i = 0; i < c.length; i++) {
            c[i] = new RadiationComponent(wavelengths[i], relativeWeights[i]);
            sum += relativeWeights[i];
        }
        if (!(sum > 0.0) || !Double.isFinite(sum))
            throw new IllegalArgumentException("At least one radiation relative weight must be > 0");
        radiationComponents = new RadiationComponent[c.length];
        for (int i = 0; i < c.length; i++)
            radiationComponents[i] = new RadiationComponent(c[i].wavelength, c[i].relativeWeight / sum);
        lambda = radiationComponents[0].wavelength;
    }

    /** Convenience overload accepting component objects; weights are normalized. */
    public synchronized void setRadiationComponents(RadiationComponent... components) {
        Objects.requireNonNull(components, "components");
        double[] wl = new double[components.length], wt = new double[components.length];
        for (int i = 0; i < components.length; i++) {
            if (components[i] == null) throw new NullPointerException("components[" + i + "]");
            wl[i] = components[i].wavelength;
            wt[i] = components[i].relativeWeight;
        }
        setRadiationComponents(wl, wt);
    }

    /** Return a defensive copy of the normalized powder-radiation mixture. */
    public synchronized RadiationComponent[] radiationComponents() {
        return radiationComponents.clone();
    }

    /** Restore the wavelength read from the DIFFaX input as a single 100% component. */
    public synchronized void setSingleWavelength(double wavelength) {
        setRadiationComponents(new double[]{wavelength}, new double[]{1.0});
    }
    /** Runtime capacities. Defaults reproduce the historical DIFFaX dimensions. */
    public static final class Config {
        public int maxLayers = 20;
        public int maxAtomsPerLayer = 200;
        public int maxAtomTypes = 20;
        public int maxSpectrumPoints = 20001;
        public int maxExplicitSequence = 5000;
        public int defaultSadpSize = 256;

        public Config configMax(int maxLayers, int maxAtomsPerLayer, int maxAtomTypes, int maxSpectrumPoints,
                      int maxExplicitSequence, int defaultSadpSize) {
          this.maxLayers = maxLayers;
          this.maxAtomsPerLayer = maxAtomsPerLayer;
          this.maxAtomTypes = maxAtomTypes;
          this.maxSpectrumPoints = maxSpectrumPoints;
          this.maxExplicitSequence = maxExplicitSequence;
          this.defaultSadpSize = defaultSadpSize;
          return this;
        }

        public Config maxLayers(int v){ this.maxLayers=v; return this; }
        public Config maxAtomsPerLayer(int v){ this.maxAtomsPerLayer=v; return this; }
        public Config maxAtomTypes(int v){ this.maxAtomTypes=v; return this; }
        public Config maxSpectrumPoints(int v){ this.maxSpectrumPoints=v; return this; }
        public Config maxExplicitSequence(int v){ this.maxExplicitSequence=v; return this; }
        public Config defaultSadpSize(int v){ this.defaultSadpSize=v; return this; }
    }

    public DiffaxModel() { this(new Config()); }

  private Texture textureModel = null;
  private Phase  phase = null;
  private double[] soVector = null;
  public synchronized void setPhase(Phase phase) {
    this.phase = phase;
    soVector = phase.getSoVector();
    textureModel = phase.getActiveTexture();
    textureModel.initQuickComputation();
    hasTexture = !textureModel.isRandomTexture();
  }

  private DiffrDataFile[] dataFiles = null;
  public synchronized void setDataFiles(DiffrDataFile[] dataFiles) {
    this.dataFiles = dataFiles;
  }
  public int activeDatafileNumber = 0;

  private DataFileSet dataset = null;
  private InstrumentBroadening instBroad = null;
  private Geometry instrumentGeometry = null;
  private Sample sample = null;
  private double[][] tiltingAngles = null;

  public synchronized void setDataSet(DataFileSet dataset) {
    // INSTRUMENTAL
    this.dataset = dataset;
    sample = dataset.getSample();
    Instrument inst = dataset.getInstrument();
    DiffrDataFile[] datafiles = dataset.getActiveDataFiles();
    setDataFiles(datafiles);
    tiltingAngles = new double[datafiles.length][DiffrDataFile.maxAngleNumber];
    for (int i = 0; i < datafiles.length; i++) {
      DiffrDataFile datafile = datafiles[i];
      double[] angles = datafile.getTiltingAngle();
      for (int j = 0; j < angles.length; j++) {
        tiltingAngles[i][j] = angles[j];
      }
    }
    instBroad = inst.getInstrumentBroadening();
    instrumentGeometry = inst.getGeometry();
    RadiationType radiationType = inst.getRadiationType();
    radType = DiffaxModel.X_RAY;
    if (radiationType.isNeutron())
      radType = DiffaxModel.NEUTRN;
    if (radiationType.isElectron())
      radType = DiffaxModel.ELECTN;

    int nrad = radiationType.getLinesCount();
    double[] wavelengths = new double[nrad];
    double[] relativeWeights = new double[nrad];
    for (int i = 0; i < nrad; i++) {
      wavelengths[i] = radiationType.getRadiationWavelength(i);
      relativeWeights[i] = radiationType.getRadiationWeigth(i);
    }
    setRadiationComponents(wavelengths, relativeWeights);
/*

    double singleLambda = radiationType.getMeanRadiationWavelength();
    lambda = singleLambda;
    if (!(lambda > 0.0))
      throw new IllegalArgumentException("wavelength must be positive");
*/
  }

  public boolean hasTexture = false;

  /**
   * @param aphase
   * @param adataset
   * @param maxLayers
   * @param maxAtomsPerLayer
   * @param maxAtomTypes
   * @param maxSpectrumPoints
   * @param maxExplicitSequence
   * @param defaultSadpSize
   */
  public DiffaxModel(Phase aphase, DataFileSet adataset, int maxLayers, int maxAtomsPerLayer, int maxAtomTypes, int maxSpectrumPoints,
                     int maxExplicitSequence, int defaultSadpSize) {
      this((new Config()).configMax(maxLayers, maxAtomsPerLayer, maxAtomTypes, maxSpectrumPoints,
          maxExplicitSequence, defaultSadpSize));

      setPhase(aphase);
      if (adataset != null)
        setDataSet(adataset);
  }

    public DiffaxModel(Config cfg) {
        Objects.requireNonNull(cfg, "cfg");
        if(cfg.maxLayers < 1 || cfg.maxAtomsPerLayer < 1 || cfg.maxAtomTypes < 1 ||
           cfg.maxSpectrumPoints < 32 || cfg.maxExplicitSequence < 1)
            throw new IllegalArgumentException("Invalid DIFFaX model capacities");
        validateSadpSize(cfg.defaultSadpSize);
        MAX_L=cfg.maxLayers;
        MAX_A=cfg.maxAtomsPerLayer;
        MAX_TA=cfg.maxAtomTypes;
        MAX_SP=cfg.maxSpectrumPoints;
        XP_MAX=cfg.maxExplicitSequence;
        DEFAULT_SADP_SIZE=cfg.defaultSadpSize;

        aName=new String[MAX_A+1][MAX_L+1]; atomL=new String[MAX_TA+1];
        oneB=new boolean[MAX_L+1]; oneOccup=new boolean[MAX_L+1];
        bsZero=new boolean[MAX_L+1][MAX_L+1]; there=new boolean[MAX_L+1][MAX_L+1];
        lSeq=new int[XP_MAX+1]; aType=new int[MAX_A+1][MAX_L+1];
        lNAtoms=new int[MAX_L+1]; lSymmetry=new int[MAX_L+1]; lActual=new int[MAX_L+1];
        aNumber=new int[MAX_A+1][MAX_L+1]; eSf=new int[MAX_TA+1];
        lAlpha=new double[MAX_L+1][MAX_L+1]; lR=new double[4][MAX_L+1][MAX_L+1];
        lG=new double[MAX_L+1]; aPos=new double[4][MAX_A+1][MAX_L+1];
        aB=new double[MAX_A+1][MAX_L+1]; aOccup=new double[MAX_A+1][MAX_L+1];
        highAtom=new double[MAX_L+1]; lowAtom=new double[MAX_L+1];
        rB11=new double[MAX_L+1][MAX_L+1]; rB22=new double[MAX_L+1][MAX_L+1];
        rB33=new double[MAX_L+1][MAX_L+1]; rB12=new double[MAX_L+1][MAX_L+1];
        rB23=new double[MAX_L+1][MAX_L+1]; rB31=new double[MAX_L+1][MAX_L+1];
        hxKy=new double[MAX_A+1][MAX_L+1]; spec=new double[MAX_SP+1]; brdSpc=new double[MAX_SP+1];
        detune=new double[MAX_L+1][MAX_L+1]; xSf=new double[10][MAX_TA+1];
        mat=complexMatrix(MAX_L,MAX_L); mat1=complexMatrix(MAX_L,MAX_L); lPhi=complexMatrix(MAX_L,MAX_L);
        luReal=new double[MAX_L*MAX_L]; luImag=new double[MAX_L*MAX_L];
        rhsReal=new double[MAX_L]; rhsImag=new double[MAX_L]; pivot=new int[MAX_L];
        initializeConstants();
    }

    private static void validateSadpSize(int n){
        if(n < 16 || (n & 1) != 0) throw new IllegalArgumentException("SADP size must be an even integer >= 16");
    }

    // Runtime replacements for historical DIFFaX.par dimensions
    int MAX_L, MAX_A, MAX_TA, MAX_SP, XP_MAX, DEFAULT_SADP_SIZE;
    final int RCSV_MAX = 1022, MAX_NAM = 31, MAX_BIN = 10;
    final int FFACT_SIZE = 201, N_SIGMAS = 7;
    final double INF_WIDTH = 1.0e4;
    final int CLIP = 14, UNKNOWN = -1;

    final static double ZERO = 0.0, QUARTER = 0.25, HALF = 0.5, ONE = 1.0, TWO = 2.0,
            THREE = 3.0, FOUR = 4.0, FIVE = 5.0, SIX = 6.0, EIGHT = 8.0, TEN = 10.0,
            TWENTY = 20.0;
    final static double PI = Math.PI;
    final static double PI2 = 2.0 * PI;
    final static double TWO_OVER_PI = 2.0 / PI;
    final static double DEG2RAD = PI / 180.0;
    final static double RAD2DEG = 180.0 / PI;

    final static double c00 = FOUR * Math.log(TWO);
    final static double sqrtC0PI = Math.sqrt(c00 / PI);
    final static double EPS1 = 1e-1, EPS2 = 1e-2, EPS3 = 1e-3, EPS4 = 1e-4,
            EPS5 = 1e-5, EPS6 = 1e-6, EPS7 = 1e-7, EPS8 = 1e-8, EPS9 = 1e-9,
            EPS10 = 1e-10, EPS14 = 1e-14;
    final static double EIGHTBITS = 256.0, FIFTEENBITS = 32768.0, SIXTEENBITS = 65536.0;

    // FORTRAN block data /consts/
    final static int NONE = 0, CENTRO = 1;
    final static int GAUSS = 1, LORENZ = 2, PS_VGT = 3, PV_GSS = 4, PV_LRN = 5;
    public final static int X_RAY = 0, NEUTRN = 1, ELECTN = 2;

    // COMMON /chars*/. String arrays are 1-based just like the source.
    final String[][] aName;
    final String[] atomL;
    String pntGrp = "", sfname = "", cfname = "control.dif";
    Path currentInputDir = Path.of(".").toAbsolutePath().normalize();

    // COMMON /logic*/
    final boolean[] oneB;
    final boolean[] oneOccup;
    final boolean[][] bsZero;
    final boolean[][] there;
    boolean onlyReal, sameBs, allBsZero, rotOnly, cFile, doDatDump, doSymDump,
            intpF, trimOrigin, recrsv, xplcit, rndm, infThick, hasLMirror, autoSymmetry,
            hMirror, kMirror, hkMirror, checkSym, sameRz, anySharp, sameLayer, finiteWidth;

    // COMMON /integ*/
    final int[] lSeq;
    final int[] pow = new int[MAX_BIN + 1];
    final int[][] aType;
    final int[] lNAtoms, lSymmetry, lActual;
    final int[][] aNumber;
    final int[] eSf;
    int symGrpNo, noTrials, hBnd, kBnd, maxPow, lCnt, fullBrd, fullShrp,
            sadblock, loglin, bitdepth, radType, nLayers, nActual, blurring, nAtoms, maxsad;

    // COMMON /reals*/
    public double[][] lAlpha;
    public double[][][] lR;
    final double[] lG;
    final double[][][] aPos;
    final double[][] aB;
    final double[][] aOccup;
    final double[] highAtom, lowAtom;
    final double[][] rB11, rB22, rB33, rB12, rB23, rB31;
    final double[][] hxKy;
    final double[] spec, brdSpc;
    final double[][] detune;
    final double[][] xSf;
    // n_sf is EQUIVALENCE'd to x_sf in Fortran. Access through neutronSf().

    double aB11, aB22, aB33, aB12, aB23, aB31;
    double tolerance, maxVar, maxAngle, lBnd, lRz,
            scaleint, brightness, lambda, th2Min, th2Max, dTheta, hStart, kStart, hEnd, kEnd,
            cellA, cellB, cellC, cellGamma, pvU, pvV, pvW, pvGamma, pvGamma2, fwhm, mltplcty,
            bndsWt, theta1, theta2, a0, b0, c0, d0, ab0, bc0, ca0, tinyInty, fatsWallaHk;
    final double[] formfactor = new double[FFACT_SIZE + 1];
    double ffactScale, wa, wb, ffhkcnst, ffwdth;

    // COMMON /cmplx1/
    final Complex[][] mat;
    final Complex[][] mat1;
    final Complex[][] lPhi;
    Complex wavefn = Complex.ZERO;

    public static String scat_database = "data.sfc";

    // Allocation-free workspace for the hot recursive complex solve.  The old
    // object-based solver remains available for numerical regression/debugging.
    final double[] luReal, luImag, rhsReal, rhsImag;
    final int[] pivot;
    private boolean primitiveComplexSolver = true;
    private boolean optimizedGetMat = true;
    private boolean optimizedApprF = true;

    /** Use the allocation-free primitive complex LU solver (default: true). */
    public synchronized void setPrimitiveComplexSolver(boolean enabled) {
        primitiveComplexSolver = enabled;
    }

    public synchronized boolean isPrimitiveComplexSolverEnabled() {
        return primitiveComplexSolver;
    }

    /** Use the reduced-allocation GET_MAT implementation (default: true). */
    public synchronized void setOptimizedGetMat(boolean enabled) { optimizedGetMat = enabled; }
    public synchronized boolean isOptimizedGetMatEnabled() { return optimizedGetMat; }

    /** Use direct primitive Lagrange interpolation in APPR_F (default: true). */
    public synchronized void setOptimizedApprF(boolean enabled) { optimizedApprF = enabled; }
    public synchronized boolean isOptimizedApprFEnabled() { return optimizedApprF; }

    private Complex[][] complexMatrix(int n, int m) {
        Complex[][] a = new Complex[n + 1][m + 1];
        for (int i = 0; i <= n; i++) Arrays.fill(a[i], Complex.ZERO);
        return a;
    }

    double neutronSf(int i) { return xSf[1][i]; }
    void neutronSf(int i, double value) { xSf[1][i] = value; }

    private void initializeConstants() {
      scat_database = MaudPreferences.getPref("diffax.scattering_database", Constants.documentsDirectory + "data.sfc");
    }

    /** Translation of the top-level FORTRAN PROGRAM DIFFaX control flow. */
    void runDriver(BufferedReader keyboard, PrintStream out) throws Exception {
        boolean ending = false;
        boolean ok = fndctl();

        while (ok && !ending) {
            String infile = getfil(keyboard, out);
            if (infile == null || infile.equalsIgnoreCase("END")) break;

            doDatDump = readInt(keyboard, out, "Enter 1 for DUMP") == 1;
            ok = rdfile(infile);
            if (ok) ok = sfc();
            if (ok) ok = getG();
            if (ok && rndm) ok = getlay();
            if (ok) sphcst();
            if (ok && doDatDump) dump(infile);
            if (ok) detun();
            if (!ok) break;

            doSymDump = false;
            if (symGrpNo != 11) {
                doSymDump = readInt(keyboard, out,
                        "Enter 1 for a dump of the symmetry evaluations") == 1;
            }

            optimz();
            if (!ok) break;

            boolean functionMenu = true;
            while (functionMenu && ok) {
                int choice;
                do {
                    choice = readInt(keyboard, out,
                            "Enter function number:\n 0 POINT, 1 STREAK, 2 INTEGRATE, 3 POWDER PATTERN, 4 SADP");
                } while (choice < 0 || choice > 4);

                switch (choice) {
                    case 0 -> point();
                    case 1 -> gostrk(infile);
                    case 2 -> gointr();
                    case 3 -> gospec(infile);
                    case 4 -> gosadp(infile);
                    default -> throw new AssertionError();
                }

                if (ok && choice != 3) {
                    functionMenu = readInt(keyboard, out,
                            "Enter 1 to return to function menu. 0 to exit.") == 1;
                } else {
                    functionMenu = false;
                }
            }

            if (!cFile) ending = true;
        }

        out.println(ok ? "DIFFaX ended normally." : "DIFFaX was terminated abnormally.");
    }

    private int readInt(BufferedReader in, PrintStream out, String prompt) throws IOException {
        while (true) {
            out.println(prompt);
            String line = in.readLine();
            if (line == null) throw new EOFException("End of input");
            try { return Integer.parseInt(line.trim().split("\\s+")[0]); }
            catch (NumberFormatException ignored) { }
        }
    }

    // ---------------- translated utility routines ----------------

    /** FORTRAN BINPOW: true iff n is an exact positive power of two. */
    boolean binpow(int n) {
        if (n < 1) return false;
        return (n & (n - 1)) == 0;
    }

    /** FORTRAN BOUNDS: fold a fractional coordinate into [0,1). */
    double bounds(double x) {
        x -= Math.floor(x);
        if (x < ZERO) x += ONE;
        if (x >= ONE) x -= ONE;
        return x;
    }

    /** FORTRAN LENGTH: position of last non-blank character. */
    int length(String s) {
        int i = s.length();
        while (i > 0 && s.charAt(i - 1) == ' ') i--;
        return i;
    }

    /** FORTRAN TOUPPR. */
    String touppr(String s) { return s.toUpperCase(Locale.ROOT); }

    /** Count whitespace/comma-delimited arguments, respecting slash fractions. */
    int cntarg(String line) {
        String t = line.trim();
        if (t.isEmpty()) return 0;
        return t.split("[\\s,]+").length;
    }

    /** FORTRAN RDNMBR: accepts ordinary decimal numbers and a/b fractions. */
    double rd_nmbr(String numberString) {
        String s = numberString.trim();
        int slash = s.indexOf('/');
        if (slash < 0) return Double.parseDouble(s.replace('D', 'E').replace('d', 'e'));
        double num = Double.parseDouble(s.substring(0, slash).trim().replace('D', 'E').replace('d', 'e'));
        double den = Double.parseDouble(s.substring(slash + 1).trim().replace('D', 'E').replace('d', 'e'));
        if (den == 0.0) throw new ArithmeticException("zero denominator in " + numberString);
        return num / den;
    }

    /** FORTRAN RAN3 replacement with deterministic state. */
    final class Ran3 {
        private final Random random;
        Ran3(long seed) { random = new Random(seed); }
        double next() { return random.nextDouble(); }
    }

    /** Matrix multiply preserving 1-based indices. */
    void matmul(Complex[][] a, Complex[][] b, int n) {
        Complex[][] tmp = complexMatrix(n, n);
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= n; j++) {
                Complex sum = Complex.ZERO;
                for (int k = 1; k <= n; k++) sum = sum.add(a[i][k].mul(b[k][j]));
                tmp[i][j] = sum;
            }
        }
        for (int i = 1; i <= n; i++)
            System.arraycopy(tmp[i], 1, a[i], 1, n);
    }

    void matsqr(Complex[][] a, Complex[][] b, int n) {
        for (int i = 1; i <= n; i++)
            for (int j = 1; j <= n; j++) b[i][j] = a[i][j];
        matmul(a, b, n);
    }

    // ---------------- diffraction kernel ----------------

    /** Mutable boolean used where the FORTRAN routines returned an OK flag by reference. */
    public final class BoolRef {
        public boolean value;
        public BoolRef() { this(true); }
        public BoolRef(boolean value) { this.value = value; }
    }

    /** Q^2 = 1/d^2 at reciprocal coordinate (h,k,l). */
    double q2(int h, int k, double l) {
        return h * (double) h * a0 + k * (double) k * b0 + l * l * c0 + h * (double) k * d0;
    }

    /** Translation of FORTRAN GET_F: layer structure factors at h,k,l. */
    void getF(Complex[] f, double s2, double l) {
        final double electronFactor = 0.023934;
        double q2 = QUARTER * s2;
        double[] fact = new double[MAX_TA + 1];

        if (radType == X_RAY || radType == ELECTN) {
            for (int i = 1; i <= nAtoms; i++) {
                fact[i] = xSf[1][i] * Math.exp(-xSf[2][i] * q2)
                        + xSf[3][i] * Math.exp(-xSf[4][i] * q2)
                        + xSf[5][i] * Math.exp(-xSf[6][i] * q2)
                        + xSf[7][i] * Math.exp(-xSf[8][i] * q2)
                        + xSf[9][i];
            }
        } else if (radType == NEUTRN) {
            for (int i = 1; i <= nAtoms; i++) fact[i] = neutronSf(i);
        }

        if (radType == ELECTN) {
            for (int i = 1; i <= nAtoms; i++)
                fact[i] = electronFactor * (eSf[i] - fact[i]) / q2;
        }

        Complex[] fUniq = new Complex[MAX_L + 1];
        Arrays.fill(fUniq, Complex.ZERO);
        for (int m = 1; m <= nActual; m++) {
            double[] tmp = new double[MAX_TA + 1];
            Complex[] ctmp = new Complex[MAX_TA + 1];
            Arrays.fill(ctmp, Complex.ZERO);

            if (lSymmetry[m] == CENTRO && oneB[m]) {
                for (int j = 1; j <= lNAtoms[m]; j++) {
                    int type = aType[j][m];
                    double dot = hxKy[j][m] + l * aPos[3][j][m];
                    tmp[type] += aOccup[j][m] * Math.cos(dot);
                }
                double sum = 0.0;
                for (int j = 1; j <= nAtoms; j++) sum += tmp[j] * fact[j];
                fUniq[m] = new Complex(TWO * Math.exp(-aB[1][m] * q2) * sum, 0.0);
            } else if (lSymmetry[m] == CENTRO) {
                for (int j = 1; j <= lNAtoms[m]; j++) {
                    int type = aType[j][m];
                    double dot = hxKy[j][m] + l * aPos[3][j][m];
                    tmp[type] += aOccup[j][m] * Math.exp(-aB[j][m] * q2) * Math.cos(dot);
                }
                double sum = 0.0;
                for (int j = 1; j <= nAtoms; j++) sum += tmp[j] * fact[j];
                fUniq[m] = new Complex(TWO * sum, 0.0);
            } else if (oneB[m]) {
                for (int j = 1; j <= lNAtoms[m]; j++) {
                    int type = aType[j][m];
                    double dot = hxKy[j][m] + l * aPos[3][j][m];
                    ctmp[type] = ctmp[type].add(Complex.expi(dot).mul(aOccup[j][m]));
                }
                Complex sum = Complex.ZERO;
                for (int j = 1; j <= nAtoms; j++) sum = sum.add(ctmp[j].mul(fact[j]));
                fUniq[m] = sum.mul(Math.exp(-aB[1][m] * q2));
            } else {
                for (int j = 1; j <= lNAtoms[m]; j++) {
                    int type = aType[j][m];
                    double dot = hxKy[j][m] + l * aPos[3][j][m];
                    double scale = aOccup[j][m] * Math.exp(-aB[j][m] * q2);
                    ctmp[type] = ctmp[type].add(Complex.expi(dot).mul(scale));
                }
                Complex sum = Complex.ZERO;
                for (int j = 1; j <= nAtoms; j++) sum = sum.add(ctmp[j].mul(fact[j]));
                fUniq[m] = sum;
            }
        }

        for (int i = 1; i <= nLayers; i++) f[i] = fUniq[lActual[i]];
    }

    /** Translation of FORTRAN GET_G. */
    boolean getGKernel() {
        double[][] g = new double[nLayers][nLayers];
        double[] rhs = new double[nLayers];
        for (int i = 1; i <= nLayers - 1; i++) {
            double sum = 0.0;
            for (int j = 1; j <= nLayers; j++) sum += lAlpha[j][i];
            if (sum == 0.0) return false;
            double inv = ONE / sum;
            for (int j = 1; j <= nLayers; j++) g[i - 1][j - 1] = inv * lAlpha[i][j];
            g[i - 1][i - 1] -= ONE;
        }
        for (int i = 0; i < nLayers; i++) g[nLayers - 1][i] = ONE;
        rhs[nLayers - 1] = ONE;

        int cnt = 0;
        for (int i = 1; i <= nLayers; i++) if (lAlpha[i][i] == ONE) cnt++;
        if (cnt != 0) {
            for (int i = 1; i <= nLayers; i++) lG[i] = lAlpha[i][i] == ONE ? ONE / cnt : ZERO;
            return true;
        }

        double[] sol = solveReal(g, rhs);
        if (sol == null) return false;
        for (int i = 1; i <= nLayers; i++) lG[i] = sol[i - 1];
        return true;
    }

    /** Translation of PRE_MAT: cache the h-k part of the stacking matrix. */
    void preMat(int h, int k) {
        for (int i = 1; i <= nLayers; i++) {
            for (int j = 1; j <= nLayers; j++) {
                if (there[j][i]) {
                    double dot = PI2 * (h * lR[1][j][i] + k * lR[2][j][i]);
                    lPhi[j][i] = Complex.expi(dot);
                    double scale = detune[j][i] * lAlpha[j][i];
                    Complex z = lPhi[j][i].mul(scale);
                    if (!(sameBs || bsZero[j][i])) {
                        double dw = Math.exp(-QUARTER * (rB11[j][i] * a0 * h * h
                                + rB22[j][i] * b0 * k * k)
                                + HALF * rB12[j][i] * ab0 * h * k);
                        z = z.mul(dw);
                    }
                    mat1[i][j] = z;
                } else mat1[i][j] = Complex.ZERO;
            }
        }
        if (sameBs) {
            fatsWallaHk = allBsZero ? ONE : Math.exp(-(QUARTER * (aB11 * a0 * h * h
                    + aB22 * b0 * k * k) + HALF * aB12 * ab0 * h * k));
        }
    }

    /** Translation of GET_MAT: add the l-dependent part to PRE_MAT's matrix. */
    void getMat(int h, int k, double l) {
        if (!optimizedGetMat) { getMatLegacy(h, k, l); return; }
        final double twopiL = PI2 * l;
        final boolean commonPhase = sameRz;
        final double commonAngle = commonPhase ? twopiL * lRz : ZERO;
        final double commonCos = commonPhase ? Math.cos(commonAngle) : ZERO;
        final double commonSin = commonPhase ? Math.sin(commonAngle) : ZERO;

        if (sameBs) {
            final double dw = allBsZero ? ONE : fatsWallaHk * Math.exp(-(l *
                    (QUARTER * aB33 * c0 * l + HALF * (aB23 * bc0 * k + aB31 * ca0 * h))));
            for (int i = 1; i <= nLayers; i++) {
                for (int j = 1; j <= nLayers; j++) {
                    if (!there[j][i]) { mat[i][j] = Complex.ZERO; continue; }
                    Complex base = mat1[i][j];
                    double c, sn;
                    if (commonPhase) { c = commonCos; sn = commonSin; }
                    else { double a = twopiL * lR[3][j][i]; c = Math.cos(a); sn = Math.sin(a); }
                    double br = base.re() * dw, bi = base.im() * dw;
                    mat[i][j] = new Complex(br * c - bi * sn, br * sn + bi * c);
                }
            }
        } else {
            for (int i = 1; i <= nLayers; i++) {
                for (int j = 1; j <= nLayers; j++) {
                    if (!there[j][i]) { mat[i][j] = Complex.ZERO; continue; }
                    Complex base = mat1[i][j];
                    double c, sn;
                    if (commonPhase) { c = commonCos; sn = commonSin; }
                    else { double a = twopiL * lR[3][j][i]; c = Math.cos(a); sn = Math.sin(a); }
                    double dw = ONE;
                    if (!bsZero[j][i]) {
                        dw = Math.exp(-(l * (QUARTER * rB33[j][i] * c0 * l
                                + HALF * (rB23[j][i] * bc0 * k + rB31[j][i] * ca0 * h))));
                    }
                    double br = base.re() * dw, bi = base.im() * dw;
                    mat[i][j] = new Complex(br * c - bi * sn, br * sn + bi * c);
                }
            }
        }
    }

    /** Original object-heavy GET_MAT retained for numerical regression/debugging. */
    private void getMatLegacy(int h, int k, double l) {
        double twopiL = PI2 * l;
        if (sameBs) {
            if (allBsZero) {
                for (int i = 1; i <= nLayers; i++) for (int j = 1; j <= nLayers; j++)
                    mat[i][j] = there[j][i] ? mat1[i][j].mul(Complex.expi(twopiL * lR[3][j][i])) : Complex.ZERO;
            } else {
                double dw = fatsWallaHk * Math.exp(-(l * (QUARTER * aB33 * c0 * l
                        + HALF * (aB23 * bc0 * k + aB31 * ca0 * h))));
                for (int i = 1; i <= nLayers; i++) for (int j = 1; j <= nLayers; j++)
                    mat[i][j] = there[j][i] ? mat1[i][j].mul(dw).mul(Complex.expi(twopiL * lR[3][j][i])) : Complex.ZERO;
            }
        } else {
            for (int i = 1; i <= nLayers; i++) for (int j = 1; j <= nLayers; j++) {
                if (!there[j][i]) { mat[i][j] = Complex.ZERO; continue; }
                Complex z = mat1[i][j].mul(Complex.expi(twopiL * lR[3][j][i]));
                if (!bsZero[j][i]) {
                    double dw = Math.exp(-(l * (QUARTER * rB33[j][i] * c0 * l
                            + HALF * (rB23[j][i] * bc0 * k + rB31[j][i] * ca0 * h))));
                    z = z.mul(dw);
                }
                mat[i][j] = z;
            }
        }
    }

    /** Translation of GET_S for infinitely thick recursive crystals. */
    boolean getS(Complex[] f, Complex[] s, int h, int k, double l) {
        if (!primitiveComplexSolver) return getSLegacy(f, s);

        final int n = nLayers;
        if (n == 1) {
            Complex z = mat[1][1];
            double ar = z.re() - ONE, ai = z.im();
            double den = ar * ar + ai * ai;
            if (den == 0.0) return false;
            Complex b = s[1];
            double br = b.re(), bi = b.im();
            s[1] = new Complex((br * ar + bi * ai) / den,
                               (bi * ar - br * ai) / den);
            return true;
        }

        // Build A = mat - I directly into the reusable flat primitive workspace.
        // This avoids the matrix allocation/copy performed by the legacy path.
        for (int i = 0; i < n; i++) {
            int row = i * n;
            for (int j = 0; j < n; j++) {
                Complex z = mat[i + 1][j + 1];
                int q = row + j;
                luReal[q] = z.re() - (i == j ? ONE : ZERO);
                luImag[q] = z.im();
            }
            Complex b = s[i + 1];
            rhsReal[i] = b.re();
            rhsImag[i] = b.im();
        }

        if (!solveComplexPrimitiveDirect(n)) return false;
        for (int i = 0; i < n; i++) s[i + 1] = new Complex(rhsReal[i], rhsImag[i]);
        return true;
    }

    /** Original object-based GET_S path retained for regression/debugging. */
    private boolean getSLegacy(Complex[] f, Complex[] s) {
        Complex[][] a = new Complex[nLayers][nLayers];
        for (int i = 1; i <= nLayers; i++) for (int j = 1; j <= nLayers; j++) {
            Complex z = mat[i][j];
            if (i == j) z = z.sub(Complex.ONE);
            a[i - 1][j - 1] = z;
        }

        if (nLayers == 1) {
            if (a[0][0].abs() == 0.0) return false;
            s[1] = f[1].mul(-1.0).div(a[0][0]);
            return true;
        }

        Complex[] rhs = new Complex[nLayers];
        for (int i = 1; i <= nLayers; i++) rhs[i - 1] = s[i];
        Complex[] sol = solveComplexLegacy(a, rhs);
        if (sol == null) return false;
        for (int i = 1; i <= nLayers; i++) s[i] = sol[i - 1];
        return true;
    }

    /**
     * Primitive complex Gaussian elimination matching the legacy solveComplex()
     * operation order closely.  Matrix and RHS are updated together at each
     * pivot/elimination step; this is important for DIFFaX matrices that can be
     * very ill-conditioned near sharp Bragg conditions.
     */
    private boolean solveComplexPrimitiveDirect(int n) {
        for (int k = 0; k < n; k++) {
            int p = k;
            int q0 = k * n + k;
            double best = Math.hypot(luReal[q0], luImag[q0]);
            for (int i = k + 1; i < n; i++) {
                int q = i * n + k;
                double mag = Math.hypot(luReal[q], luImag[q]);
                if (mag > best) { best = mag; p = i; }
            }
            if (best == 0.0) return false;

            if (p != k) {
                int rp = p * n, rk = k * n;
                for (int j = 0; j < n; j++) {
                    int a = rk + j, b = rp + j;
                    double t = luReal[a]; luReal[a] = luReal[b]; luReal[b] = t;
                    t = luImag[a]; luImag[a] = luImag[b]; luImag[b] = t;
                }
                double t = rhsReal[k]; rhsReal[k] = rhsReal[p]; rhsReal[p] = t;
                t = rhsImag[k]; rhsImag[k] = rhsImag[p]; rhsImag[p] = t;
            }

            int kk = k * n + k;
            double pr = luReal[kk], pi = luImag[kk];
            double pden = pr * pr + pi * pi;
            for (int i = k + 1; i < n; i++) {
                int ik = i * n + k;
                double xr = luReal[ik], xi = luImag[ik];
                double mr = (xr * pr + xi * pi) / pden;
                double mi = (xi * pr - xr * pi) / pden;

                // Legacy solveComplex explicitly zeros the eliminated entry.
                luReal[ik] = 0.0;
                luImag[ik] = 0.0;

                int row = i * n, prow = k * n;
                for (int j = k + 1; j < n; j++) {
                    int ij = row + j, kj = prow + j;
                    double br = luReal[kj], bi = luImag[kj];
                    double tr = mr * br - mi * bi;
                    double ti = mr * bi + mi * br;
                    luReal[ij] -= tr;
                    luImag[ij] -= ti;
                }

                double br = rhsReal[k], bi = rhsImag[k];
                double tr = mr * br - mi * bi;
                double ti = mr * bi + mi * br;
                rhsReal[i] -= tr;
                rhsImag[i] -= ti;
            }
        }

        // Back substitution, again in the same arithmetic order as the legacy path.
        for (int i = n - 1; i >= 0; i--) {
            double sr = rhsReal[i], si = rhsImag[i];
            int row = i * n;
            for (int j = i + 1; j < n; j++) {
                int ij = row + j;
                double ar = luReal[ij], ai = luImag[ij];
                double xr = rhsReal[j], xi = rhsImag[j];
                double tr = ar * xr - ai * xi;
                double ti = ar * xi + ai * xr;
                sr -= tr;
                si -= ti;
            }
            int ii = row + i;
            double ar = luReal[ii], ai = luImag[ii];
            double den = ar * ar + ai * ai;
            rhsReal[i] = (sr * ar + si * ai) / den;
            rhsImag[i] = (si * ar - sr * ai) / den;
        }
        return true;
    }

    /** Translation of GET_S2 for finite recursive crystals. */
    boolean getS2(Complex[] f, Complex[] s, int h, int k, double l) {
        Complex[][] original = copyMatrix(mat, nLayers);
        Complex[][] matN = matrixPower(original, lCnt + 1, nLayers);
        for (int i = 1; i <= nLayers; i++) matN[i][i] = matN[i][i].sub(Complex.ONE);

        for (int i = 1; i <= nLayers; i++) {
            Complex sum = Complex.ZERO;
            for (int j = 1; j <= nLayers; j++) sum = sum.add(matN[i][j].mul(f[j]));
            s[i] = sum;
        }
        restoreMatrix(mat, original, nLayers);
        if (!getS(f, s, h, k, l)) return false;

        for (int i = 1; i <= nLayers; i++) s[i] = s[i].sub(f[i].mul(lCnt + 1.0)).mul(ONE / lCnt);
        restoreMatrix(mat, original, nLayers);
        return getS(f, s, h, k, l);
    }

    /** Translation of INTENS. PRE_MAT(h,k) must have been called for this streak. */
    double intens(Complex[] f, int h, int k, double l, BoolRef ok) {
        getMat(h, k, l);
        Complex[] s = new Complex[MAX_L + 1];
        Arrays.fill(s, Complex.ZERO);
        boolean good;
        if (infThick) {
            for (int i = 1; i <= nLayers; i++) s[i] = f[i].mul(-1.0);
            good = getS(f, s, h, k, l);
        } else good = getS2(f, s, h, k, l);
        ok.value = good;
        if (!good) return ZERO;

        double sum = ZERO;
        if (onlyReal) {
            for (int i = 1; i <= nLayers; i++) sum += lG[i] * f[i].re() * s[i].re();
            sum *= TWO;
            for (int i = 1; i <= nLayers; i++) sum -= lG[i] * f[i].re() * f[i].re();
        } else {
            for (int i = 1; i <= nLayers; i++) sum += lG[i] * f[i].conj().mul(s[i]).re();
            sum *= TWO;
            for (int i = 1; i <= nLayers; i++) { double x = f[i].abs(); sum -= lG[i] * x * x; }
        }
        return sum;
    }

    /** Translation of INTEN2 for an explicit layer sequence. */
    double inten2(Complex[] f, int h, int k, double l, BoolRef ok) {
        ok.value = true;
        if (lCnt == 1) wavefn = f[lSeq[1]];
        else if (sameLayer) {
            int i = lSeq[1];
            double dot = PI2 * (h * lR[1][i][i] + k * lR[2][i][i] + l * lR[3][i][i]);
            Complex z = Complex.expi(dot);
            double tmp = dot / PI2;
            if (Math.abs(tmp - Math.rint(tmp)) <= EPS5) wavefn = f[i].mul(lCnt);
            else wavefn = f[i].mul(Complex.ONE.sub(Complex.expi(dot * lCnt))).div(Complex.ONE.sub(z));
        } else {
            Complex[][] phi = complexMatrix(MAX_L, MAX_L);
            double twopiL = PI2 * l;
            for (int i = 1; i <= nLayers; i++) for (int j = 1; j <= nLayers; j++)
                phi[j][i] = lPhi[j][i].mul(Complex.expi(twopiL * lR[3][j][i]));
            wavefn = f[lSeq[lCnt]];
            for (int m = lCnt - 1; m >= 1; m--) {
                int i = lSeq[m], j = lSeq[m + 1];
                wavefn = f[i].add(wavefn.mul(phi[j][i]));
            }
        }
        double amp = wavefn.abs();
        return amp * amp / lCnt;
    }

    /**
     * APPR_F structure-factor interpolation.  The production path exploits the
     * fact that interpolation is linear in the complex structure factor: for a
     * fixed target l, the same real Lagrange coefficients apply to every layer
     * and to both real and imaginary components.  This removes the repeated
     * Complex-object POLINT work from the powder quadrature.
     */
    boolean apprF(Complex[][] f, int h, int k, double[] ll, double[] agL, int n, int[] list) {
        if (!optimizedApprF) return apprFLegacy(f, h, k, ll, agL, n, list);

        // Sample the exact layer structure factors only at the n support points.
        Complex[][] ff = new Complex[n + 1][];
        for (int i = 1; i <= n; i++) {
            Complex[] sample = new Complex[MAX_L + 1];
            Arrays.fill(sample, Complex.ZERO);
            getF(sample, q2(h, k, ll[i]), ll[i]);
            ff[i] = sample;
        }

        // Known Gauss nodes are copied directly.  All other nodes use one set
        // of interpolation coefficients shared by every layer.
        double[] coeff = new double[n + 1];
        for (int m = 1; m <= 16; m++) {
            int known = 0;
            for (int i = 1; i <= n; i++) {
                if (m == list[i]) { known = i; break; }
            }
            if (known != 0) {
                Complex[] src = ff[known];
                for (int layer = 1; layer <= nLayers; layer++) f[layer][m] = src[layer];
                continue;
            }

            final double x = agL[m];
            for (int j = 1; j <= n; j++) {
                double cj = 1.0;
                final double xj = ll[j];
                for (int q = 1; q <= n; q++) {
                    if (q == j) continue;
                    final double den = xj - ll[q];
                    if (den == 0.0) return false;
                    cj *= (x - ll[q]) / den;
                }
                coeff[j] = cj;
            }

            for (int layer = 1; layer <= nLayers; layer++) {
                double re = 0.0, im = 0.0;
                for (int j = 1; j <= n; j++) {
                    Complex z = ff[j][layer];
                    double c = coeff[j];
                    re += c * z.re();
                    im += c * z.im();
                }
                f[layer][m] = new Complex(re, im);
            }
        }
        return true;
    }

    /** Original object-heavy APPR_F/POLINT path retained for regression. */
    private boolean apprFLegacy(Complex[][] f, int h, int k, double[] ll, double[] agL, int n, int[] list) {
        Complex[][] ff = new Complex[MAX_L + 1][n + 1];
        for (int i = 1; i <= n; i++) {
            Complex[] sample = new Complex[MAX_L + 1];
            Arrays.fill(sample, Complex.ZERO);
            getF(sample, q2(h, k, ll[i]), ll[i]);
            for (int layer = 1; layer <= nLayers; layer++) ff[layer][i] = sample[layer];
        }
        for (int m = 1; m <= 16; m++) {
            int known = 0;
            for (int i = 1; i <= n; i++) if (m == list[i]) known = i;
            if (known != 0) {
                for (int layer = 1; layer <= nLayers; layer++) f[layer][m] = ff[layer][known];
            } else {
                for (int layer = 1; layer <= nLayers; layer++) {
                    Complex[] ya = new Complex[n + 1];
                    for (int j = 1; j <= n; j++) ya[j] = ff[layer][j];
                    Complex ans = polintLegacy(ll, ya, n, agL[m]);
                    if (ans == null) return false;
                    f[layer][m] = ans;
                }
            }
        }
        return true;
    }

    Complex polint(double[] xa, Complex[] ya, int n, double x) {
        if (!optimizedApprF) return polintLegacy(xa, ya, n, x);
        double re = 0.0, im = 0.0;
        for (int j = 1; j <= n; j++) {
            double c = 1.0;
            for (int q = 1; q <= n; q++) {
                if (q == j) continue;
                double den = xa[j] - xa[q];
                if (den == 0.0) return null;
                c *= (x - xa[q]) / den;
            }
            re += c * ya[j].re();
            im += c * ya[j].im();
        }
        return new Complex(re, im);
    }

    private Complex polintLegacy(double[] xa, Complex[] ya, int n, double x) {
        Complex[] c = new Complex[n + 1], d = new Complex[n + 1];
        int ns = 1;
        double dif = Math.abs(x - xa[1]);
        for (int i = 1; i <= n; i++) {
            double dift = Math.abs(x - xa[i]);
            if (dift < dif) { ns = i; dif = dift; }
            c[i] = ya[i]; d[i] = ya[i];
        }
        Complex y = ya[ns--];
        for (int m = 1; m <= n - 1; m++) {
            for (int i = 1; i <= n - m; i++) {
                double ho = xa[i] - x, hp = xa[i + m] - x;
                double den0 = ho - hp;
                if (den0 == 0.0) return null;
                Complex den = c[i + 1].sub(d[i]).mul(ONE / den0);
                d[i] = den.mul(hp);
                c[i] = den.mul(ho);
            }
            Complex dy;
            if (2 * ns < n - m) dy = c[ns + 1];
            else { dy = d[ns]; ns--; }
            y = y.add(dy);
        }
        return y;
    }

    /** Translation of 16-point Gauss-Legendre streak integration GLQ16. */
    double glq16(int h, int k, double a, double b, BoolRef ok) {
        if (b < a) { ok.value = false; return ZERO; }
        if (b == a) { ok.value = true; return ZERO; }
        intpF = true;
        double c1 = HALF * (b - a), c2 = c1 + a;
        double[] ag = new double[17];
        for (int i = 1; i <= 8; i++) ag[i] = -c1 * x[9 - i] + c2;
        for (int i = 9; i <= 16; i++) ag[i] = c1 * x[i - 8] + c2;

        Complex[][] f = new Complex[MAX_L + 1][17];
        for (int layer = 0; layer <= MAX_L; layer++) Arrays.fill(f[layer], Complex.ZERO);
        if (intpF) {
            int[] list = {0, 1, 8, 16};
            double[] samp = {0, ag[1], ag[8], ag[16]};
            boolean close = samp[1] == samp[3] || samp[1] == samp[2] || samp[2] == samp[3];
            if (!close) {
                if (!apprF(f, h, k, samp, ag, 3, list)) { ok.value = false; return ZERO; }
            } else {
                Complex[] one = new Complex[MAX_L + 1]; Arrays.fill(one, Complex.ZERO);
                getF(one, q2(h, k, ag[1]), ag[1]);
                for (int layer = 1; layer <= nLayers; layer++) for (int i = 1; i <= 16; i++) f[layer][i] = one[layer];
            }
        }

        double sum = 0.0;
        BoolRef o = new BoolRef(true);
        for (int pair = 1; pair <= 8; pair++) {
            int lo = pair, hi = 17 - pair, wi = 9 - pair;
            Complex[] flo = column(f, lo), fhi = column(f, hi);
            double yl = recrsv ? intens(flo, h, k, ag[lo], o) : inten2(flo, h, k, ag[lo], o);
            if (!o.value) { ok.value = false; return ZERO; }
            double yh = recrsv ? intens(fhi, h, k, ag[hi], o) : inten2(fhi, h, k, ag[hi], o);
            if (!o.value) { ok.value = false; return ZERO; }
            // Preferred orientation belongs inside the rod quadrature, not as a
            // post-hoc peak multiplier.  This is essential for fault-broadened
            // rods, overlapping features, and diffuse intensity.
            double twl = textureWeight(h, k, ag[lo]);
            double twh = textureWeight(h, k, ag[hi]);
            sum += w[wi] * (yl * twl + yh * twh);
        }
        ok.value = true;
        return c1 * sum;
    }

    private Complex[] column(Complex[][] a, int col) {
        Complex[] v = new Complex[MAX_L + 1]; Arrays.fill(v, Complex.ZERO);
        for (int i = 1; i <= nLayers; i++) v[i] = a[i][col];
        return v;
    }

    private double[] solveReal(double[][] a0, double[] b0) {
        int n = b0.length; double[][] a = new double[n][n]; double[] b = b0.clone();
        for (int i = 0; i < n; i++) a[i] = a0[i].clone();
        for (int k = 0; k < n; k++) {
            int p = k; for (int i = k + 1; i < n; i++) if (Math.abs(a[i][k]) > Math.abs(a[p][k])) p = i;
            if (a[p][k] == 0.0) return null;
            double[] tr = a[k]; a[k] = a[p]; a[p] = tr; double tb = b[k]; b[k] = b[p]; b[p] = tb;
            for (int i = k + 1; i < n; i++) { double m = a[i][k] / a[k][k]; a[i][k] = 0.0; for (int j = k + 1; j < n; j++) a[i][j] -= m * a[k][j]; b[i] -= m * b[k]; }
        }
        double[] x = new double[n]; for (int i = n - 1; i >= 0; i--) { double s = b[i]; for (int j = i + 1; j < n; j++) s -= a[i][j] * x[j]; x[i] = s / a[i][i]; }
        return x;
    }

    private Complex[] solveComplexLegacy(Complex[][] a0, Complex[] b0) {
        int n = b0.length; Complex[][] a = new Complex[n][n]; Complex[] b = b0.clone();
        for (int i = 0; i < n; i++) a[i] = a0[i].clone();
        for (int k = 0; k < n; k++) {
            int p = k; for (int i = k + 1; i < n; i++) if (a[i][k].abs() > a[p][k].abs()) p = i;
            if (a[p][k].abs() == 0.0) return null;
            Complex[] tr = a[k]; a[k] = a[p]; a[p] = tr; Complex tb = b[k]; b[k] = b[p]; b[p] = tb;
            for (int i = k + 1; i < n; i++) { Complex m = a[i][k].div(a[k][k]); a[i][k] = Complex.ZERO; for (int j = k + 1; j < n; j++) a[i][j] = a[i][j].sub(m.mul(a[k][j])); b[i] = b[i].sub(m.mul(b[k])); }
        }
        Complex[] x = new Complex[n]; Arrays.fill(x, Complex.ZERO);
        for (int i = n - 1; i >= 0; i--) { Complex s = b[i]; for (int j = i + 1; j < n; j++) s = s.sub(a[i][j].mul(x[j])); x[i] = s.div(a[i][i]); }
        return x;
    }

    private Complex[][] copyMatrix(Complex[][] src, int n) {
        Complex[][] dst = complexMatrix(n, n); for (int i = 1; i <= n; i++) for (int j = 1; j <= n; j++) dst[i][j] = src[i][j]; return dst;
    }
    private void restoreMatrix(Complex[][] dst, Complex[][] src, int n) {
        for (int i = 1; i <= n; i++) for (int j = 1; j <= n; j++) dst[i][j] = src[i][j];
    }
    private Complex[][] matrixPower(Complex[][] base0, int exponent, int n) {
        Complex[][] result = complexMatrix(n, n); for (int i = 1; i <= n; i++) result[i][i] = Complex.ONE;
        Complex[][] base = copyMatrix(base0, n); int e = exponent;
        while (e > 0) { if ((e & 1) != 0) result = multiply(result, base, n); e >>= 1; if (e != 0) base = multiply(base, base, n); }
        return result;
    }
    private Complex[][] multiply(Complex[][] a, Complex[][] b, int n) {
        Complex[][] c = complexMatrix(n, n);
        for (int i = 1; i <= n; i++) for (int j = 1; j <= n; j++) { Complex s = Complex.ZERO; for (int k = 1; k <= n; k++) s = s.add(a[i][k].mul(b[k][j])); c[i][j] = s; }
        return c;
    }

    // ---------------- ordinary DIFFaX input / setup / command routines ----------------

    /** Reader matching DIFFaX GETLNE semantics: blank lines are skipped and
     * nested {...} comments are removed before left-justifying the result. */
    final class DifReader {
        final List<String> lines = new ArrayList<>();
        int pos;
        DifReader(Path path) throws IOException {
            String text = Files.readString(path, StandardCharsets.UTF_8);
            int depth = 0;
            StringBuilder line = new StringBuilder();
            for (int q = 0; q < text.length(); q++) {
                char c = text.charAt(q);
                if (c == '{') { depth++; continue; }
                if (c == '}' && depth > 0) { depth--; continue; }
                if (c == '\r') continue;
                if (c == '\n') {
                    String z = line.toString().trim();
                    if (!z.isEmpty()) lines.add(z);
                    line.setLength(0);
                } else if (depth == 0) line.append(c);
            }
            String z = line.toString().trim();
            if (!z.isEmpty()) lines.add(z);
        }
        boolean hasNext() { return pos < lines.size(); }
        String next() throws EOFException {
            if (!hasNext()) throw new EOFException("Unexpected end of DIFFaX input");
            return lines.get(pos++);
        }
        String peek() throws EOFException {
            if (!hasNext()) throw new EOFException("Unexpected end of DIFFaX input");
            return lines.get(pos);
        }
    }

    boolean fndctl() { cFile = Files.isRegularFile(Path.of(cfname)); return true; }

    String getfil(BufferedReader in, PrintStream out) throws IOException {
        out.println("Enter structure data filename (END to quit):");
        return in.readLine();
    }

    /** Read the standard DIFFaX data-file grammar used by versions 1.7xx-1.813. */
    boolean rdfile(String infile) {
        try {
            Path path = Path.of(infile).toAbsolutePath().normalize();
            currentInputDir = path.getParent() == null ? Path.of(".").toAbsolutePath().normalize() : path.getParent();
            DifReader r = new DifReader(path);

            requireStarts(r.next(), "INSTRUMENTAL");
            String radiation = r.next().toUpperCase(Locale.ROOT);
            if (radiation.startsWith("X-RAY")) radType = X_RAY;
            else if (radiation.startsWith("NEUTRON")) radType = NEUTRN;
            else if (radiation.startsWith("ELECTRON")) radType = ELECTN;
            else throw new IllegalArgumentException("radiation type must be X-RAY, NEUTRON or ELECTRON");

            lambda = firstNumber(r.next());
            if (!(lambda > 0.0)) throw new IllegalArgumentException("wavelength must be positive");
            radiationComponents = new RadiationComponent[]{new RadiationComponent(lambda, 1.0)};
            parseBroadening(r.next());

            requireStarts(r.next(), "STRUCTURAL");
            double[] cell = numbers(r.next());
            if (cell.length < 4) throw new IllegalArgumentException("STRUCTURAL cell line requires a, b, c, gamma");
            cellA = cell[0]; cellB = cell[1]; cellC = cell[2]; cellGamma = cell[3];
            if (!(cellA > 0 && cellB > 0 && cellC > 0 && cellGamma > 0 && cellGamma < 180))
                throw new IllegalArgumentException("illegal unit-cell dimensions or gamma");
//          System.out.println("Cell: " + cellA + " " +  cellB + " " + cellC + " " + cellGamma);
            cellGamma *= DEG2RAD;

            parsePointGroup(r.next());
            nLayers = (int)Math.rint(firstNumber(r.next()));
          if (nLayers < 1) throw new IllegalArgumentException("number of layers must be at least 1");
          MAX_L = nLayers;

            String maybeWidth = r.peek();
            if (!maybeWidth.toUpperCase(Locale.ROOT).startsWith("LAYER")) {
                String width = r.next();
                if (width.toUpperCase(Locale.ROOT).startsWith("INFINITE")) finiteWidth = false;
                else {
                    double[] v = numbers(width);
                    if (v.length < 1 || v.length > 2) throw new IllegalArgumentException("layer width requires Wa [Wb] or INFINITE");
                    wa = v[0]; wb = v.length == 1 ? wa : v[1];
                    if (wa <= 0 || wb <= 0) throw new IllegalArgumentException("layer widths must be positive");
                    finiteWidth = !(wa > INF_WIDTH && wb > INF_WIDTH);
                }
            } else finiteWidth = false;
//          System.out.println(nLayers + " " + finiteWidth);
            parseLayers(r);
            parseStacking(r);
            parseTransitions(r);
//            System.out.println("'" + path.getFileName() + "' read in.");
            return true;
        } catch (Exception ex) {
            System.err.println("ERROR reading DIFFaX input: " + ex.getMessage());
            return false;
        }
    }

    private void requireStarts(String line, String word) {
        if (!line.trim().toUpperCase(Locale.ROOT).startsWith(word))
            throw new IllegalArgumentException("expected '" + word + "', found: " + line);
    }

    private double firstNumber(String line) {
        double[] a = numbers(line); if (a.length == 0) throw new IllegalArgumentException("expected a number: " + line); return a[0];
    }

    private double[] numbers(String line) {
        String cleaned = line.replace('(', ' ').replace(')', ' ').replace(',', ' ');
        String[] t = cleaned.trim().split("\\s+");
        ArrayList<Double> v = new ArrayList<>();
        for (String x : t) {
            if (x.isBlank()) continue;
            try { v.add(rd_nmbr(x)); } catch (RuntimeException ignored) { }
        }
        double[] a = new double[v.size()]; for (int i=0;i<a.length;i++) a[i]=v.get(i); return a;
    }

    private void parseBroadening(String src) {
        String line = src.toUpperCase(Locale.ROOT);
        trimOrigin = line.contains(" TRIM");
        line = line.replace(" TRIM", "").trim();
        if (line.startsWith("NONE")) { blurring = NONE; return; }
        String key;
        if (line.startsWith("GAUSSIAN")) key="GAUSSIAN";
        else if (line.startsWith("LORENTZIAN")) key="LORENTZIAN";
        else if (line.startsWith("PSEUDO-VOIGT")) key="PSEUDO-VOIGT";
        else throw new IllegalArgumentException("illegal instrumental broadening: " + src);
        double[] v = numbers(line.substring(key.length()));
        if (key.equals("PSEUDO-VOIGT")) {
            if (v.length != 4) throw new IllegalArgumentException("PSEUDO-VOIGT requires u v w gamma");
            pvU=v[0];pvV=v[1];pvW=v[2];pvGamma=v[3]; blurring=PS_VGT;
            if (pvGamma < 0 || pvGamma > 1) throw new IllegalArgumentException("pseudo-Voigt gamma must be 0..1");
//          System.out.println(pvU + " " + pvV + " " + pvW + " " + pvGamma + " " + blurring);
            return;
        }
        if (v.length == 1) {
            fwhm=v[0]; if (fwhm < 0) throw new IllegalArgumentException("FWHM cannot be negative");
            blurring=key.equals("GAUSSIAN")?GAUSS:LORENZ;
            if (fwhm < EPS7) blurring=NONE;
        } else if (v.length == 3) {
            pvU=v[0];pvV=v[1];pvW=v[2];
            if (key.equals("GAUSSIAN")) { pvGamma=0; blurring=PV_GSS; }
            else { pvGamma=1; blurring=PV_LRN; }
        } else throw new IllegalArgumentException(key + " requires FWHM or u v w");
    }

    private void parsePointGroup(String src) {
        String line=src.trim().toUpperCase(Locale.ROOT);
        // Match the most specific names first.  In particular, 6/MMM must not
        // be swallowed by the 6/M prefix (and likewise -3M by -3).
        String[] keys={"6/MMM","4/MMM","2/M(1)","2/M(2)","UNKNOWN","AXIAL","-3M","6/M","4/M","MMM","-3","-1"};
        int[] groups={10,8,2,3,12,11,6,9,7,4,5,1};
        int found=-1; String key="";
        for(int i=0;i<keys.length;i++) if(line.startsWith(keys[i])) {found=groups[i];key=keys[i];break;}
        if(found<0) throw new IllegalArgumentException("unrecognized diffraction point group: "+src);
        pntGrp=key; tolerance=0.01;
        autoSymmetry = found==12;
        if(found<=11) symGrpNo=found;
        else {
            symGrpNo=UNKNOWN; String rest=line.substring(key.length()).trim();
            if(!rest.isEmpty()) { tolerance=firstNumber(rest)*EPS2; if(tolerance<EPS4)tolerance=EPS4; }
        }
//      System.out.println(pntGrp + " " + tolerance + " " + symGrpNo + " " + autoSymmetry);
    }

    private void parseLayers(DifReader r) throws Exception {
        for(int i=1;i<=nLayers;i++){highAtom[i]=0;lowAtom[i]=0;}
        int actual=0;
        for(int layer=1;layer<=nLayers;layer++) {
            String header=r.next(); requireStarts(header,"LAYER");
            String up=header.toUpperCase(Locale.ROOT);
            double[] hn=numbers(up.substring(5));
            if(hn.length<1 || (int)Math.rint(hn[0])!=layer) throw new IllegalArgumentException("LAYER numbers must be sequential; expected "+layer);
            int eq=up.indexOf('=');
            if(eq>=0) {
                int other=(int)Math.rint(firstNumber(up.substring(eq+1)));
                if(other<1 || other>=layer) throw new IllegalArgumentException("invalid layer equivalence LAYER "+layer+" = "+other);
                lActual[layer]=lActual[other];
                continue;
            }
            actual++; lActual[layer]=actual;
            String sym=r.next().toUpperCase(Locale.ROOT);
            if(sym.startsWith("CENTROSYMMETRIC")) lSymmetry[actual]=CENTRO;
            else if(sym.startsWith("NONE")) lSymmetry[actual]=NONE;
            else throw new IllegalArgumentException("invalid symmetry for LAYER "+layer+": "+sym);
            int count=0;
            while(r.hasNext()) {
                String atom=r.peek();
                String au=atom.toUpperCase(Locale.ROOT);
                if(au.startsWith("LAYER") || au.startsWith("STACKING")) break;
                r.next();
                count++;
                if(count>MAX_A) throw new IllegalArgumentException("too many atoms in layer "+layer);
                String padded=String.format("%-4s",atom);
                String name=padded.substring(0,4);
                String rest=atom.length()>4?atom.substring(4):"";
                String[] tok=rest.trim().split("\\s+");
                if(tok.length<6)
                  throw new IllegalArgumentException("atom line requires name, number, x, y, z, B, occupancy: "+atom);
                aName[count][actual]=name;
                aNumber[count][actual]=Integer.parseInt(tok[0]);
                aPos[1][count][actual]=rd_nmbr(tok[1]);
                aPos[2][count][actual]=rd_nmbr(tok[2]);
                aPos[3][count][actual]=rd_nmbr(tok[3]);
                aB[count][actual]=rd_nmbr(tok[4]);
                aOccup[count][actual]=rd_nmbr(tok[5]);
                if(aB[count][actual]<0 || aOccup[count][actual]<0 || aOccup[count][actual]>1) throw new IllegalArgumentException("illegal B/occupancy on atom line: "+atom);
                highAtom[actual]=Math.max(highAtom[actual],aPos[3][count][actual]); lowAtom[actual]=Math.min(lowAtom[actual],aPos[3][count][actual]);
            }
            if(count==0) throw new IllegalArgumentException("no atoms specified for LAYER "+layer);
            lNAtoms[actual]=count;
        }
        nActual=actual;
        for(int i=1;i<=nActual;i++) {
          if (lSymmetry[i] == CENTRO) {
            double z = Math.max(highAtom[i], -lowAtom[i]);
            highAtom[i] = z;
            lowAtom[i] = -z;
          }
/*          System.out.println(lSymmetry[i] + " " + lowAtom[i] + " " + highAtom[i]);
          for (int j = 1; j <= lNAtoms[i]; j++) {
            System.out.println(aName[j][i]
                + " " +  aNumber[j][i]
            + " " +  aPos[1][j][i]
            + " " +  aPos[2][j][i]
            + " " +  aPos[3][j][i]
            + " " +  aB[j][i]
            + " " +  aOccup[j][i]);
          }*/
        }
/*      for(int layer=1;layer<=nLayers;layer++)
        System.out.print(lActual[layer] + " ");
      System.out.println();*/
    }

    private void parseStacking(DifReader r) throws Exception {
        requireStarts(r.next(),"STACKING"); String mode=r.next().toUpperCase(Locale.ROOT);
        recrsv=false;xplcit=false;rndm=false;infThick=false;lCnt=0;
        if(mode.startsWith("RECURSIVE")) {
            recrsv=true; String thick=r.next().toUpperCase(Locale.ROOT);
            if(thick.startsWith("INFINITE")) infThick=true;
            else {lCnt=(int)Math.rint(firstNumber(thick)); if(lCnt<=0) throw new IllegalArgumentException("recursive layer count must be positive"); if(lCnt>RCSV_MAX){lCnt=0;infThick=true;}}
        } else if(mode.startsWith("EXPLICIT")) {
            xplcit=true; String seq=r.next().toUpperCase(Locale.ROOT);
            if(seq.startsWith("RANDOM")) {rndm=true;lCnt=(int)Math.rint(firstNumber(seq.substring(6)));if(lCnt<1||lCnt>XP_MAX)throw new IllegalArgumentException("random explicit layer count must be 1.."+XP_MAX);}
            else {
                ArrayList<Integer> vals=new ArrayList<>();
                while(true){ for(double d:numbers(seq)) vals.add((int)Math.rint(d)); if(r.peek().toUpperCase(Locale.ROOT).startsWith("TRANSITIONS"))break; seq=r.next(); }
                lCnt=Math.min(vals.size(),XP_MAX); if(lCnt<1)throw new IllegalArgumentException("empty explicit layer sequence");
                for(int i=1;i<=lCnt;i++){lSeq[i]=vals.get(i-1);if(lSeq[i]<1||lSeq[i]>nLayers)throw new IllegalArgumentException("illegal explicit layer number "+lSeq[i]);}
            }
        } else throw new IllegalArgumentException("STACKING must be EXPLICIT or RECURSIVE");
    }

    private void parseTransitions(DifReader r) throws Exception {
        requireStarts(r.next(),"TRANSITIONS");
        for(int i=1;i<=nLayers;i++) for(int j=1;j<=nLayers;j++) {
            String line=r.next(); boolean temp=line.indexOf('(')>=0;
            double[] v=numbers(line); if(v.length<1)throw new IllegalArgumentException("bad transition "+i+","+j);
            lAlpha[j][i]=v[0]; lR[1][j][i]=lR[2][j][i]=lR[3][j][i]=0;
            rB11[j][i]=rB22[j][i]=rB33[j][i]=rB12[j][i]=rB31[j][i]=rB23[j][i]=0;
            if(Math.abs(v[0])>=0) { // Luca modification EPS6) {
                if(v.length<4)throw new IllegalArgumentException("transition "+i+","+j+" requires alpha Rx Ry Rz");
                lR[1][j][i]=v[1];lR[2][j][i]=v[2];lR[3][j][i]=v[3];
                if(temp){if(v.length<10)throw new IllegalArgumentException("transition uncertainty requires six coefficients");rB11[j][i]=v[4];rB22[j][i]=v[5];rB33[j][i]=v[6];rB12[j][i]=v[7];rB31[j][i]=v[8];rB23[j][i]=v[9];}
                if(v[0]<0)throw new IllegalArgumentException("negative stacking probability at "+i+","+j);
                if(i==j && lR[3][j][i]<=0)throw new IllegalArgumentException("non-positive Rz for self-transition "+i);
            }
    //      System.out.println("Layer " +i + ", to " + j + " " + lAlpha[j][i] + " " + lR[1][j][i] + " " + lR[2][j][i] + " " + lR[3][j][i]);

        }
        for(int i=1;i<=nLayers;i++){double sum=0;for(int j=1;j<=nLayers;j++)sum+=lAlpha[j][i];if(Math.abs(sum-1)>EPS6&&(recrsv||rndm))throw new IllegalArgumentException("stacking probabilities from LAYER "+i+" sum to "+sum);}
    }

    /** Read the traditional fixed-column data.sfc file. */
    boolean sfc() {
        try {
            LinkedHashMap<String,Integer> types=new LinkedHashMap<>();
            for(int i=1;i<=nActual;i++)
              for(int j=1;j<=lNAtoms[i];j++) {
//                System.out.println(aName[j][i]);
                String name=normAtom(aName[j][i]);
                Integer idx=types.get(name);
                if(idx==null){idx=types.size()+1;
                  if(idx>MAX_TA)
                    throw new IllegalArgumentException("more than "+MAX_TA+" atom types");
                  types.put(name,idx);
                  atomL[idx]=name;}
                aType[j][i]=idx;
            }
            nAtoms=types.size();
            boolean[] found=new boolean[nAtoms+1];
            sfname = scat_database;
            Path sf=Path.of(sfname);
            if(!sf.isAbsolute()) {
              Path local=currentInputDir.resolve(sfname);
              sf=Files.isRegularFile(local)?local:Path.of(sfname);
            }
            if (!Files.isRegularFile(sf))
              throw new FileNotFoundException("scattering factor file '"+sfname+"' not found (looked beside input file and current directory)");
            for (String raw:Files.readAllLines(sf,StandardCharsets.UTF_8)) {
                if (raw.length()<4)
                  continue;
                String name=normAtom(raw.substring(0,4));
                Integer idx=types.get(name);
                if(idx==null||found[idx])
                  continue;
                if(radType==NEUTRN)
                  neutronSf(idx, fixedDouble(raw,103,11));
                else {
                    for(int k=1;k<=9;k++)
                      xSf[k][idx]=fixedDouble(raw,4+(k-1)*11,11);
                    if(radType==ELECTN)
                      eSf[idx]=(int)Math.rint(fixedDouble(raw,114,3));
                }
                found[idx]=true;
            }
            for (int i=1;i<=nAtoms;i++)
              if(!found[i])
                throw new IllegalArgumentException("data for atom '"+atomL[i]+"' not found in "+sf);
  //          System.out.println("Scattering factor data read from '"+sf+"'.");
            return true;
        } catch (Exception ex){
          System.err.println("ERROR reading scattering factors: "+ex.getMessage());
          return false;
        }
    }

    private String normAtom(String s) {
      return String.format("%-4s",s.toUpperCase(Locale.ROOT)).substring(0,4);
    }

    private double fixedDouble(String line,int start,int width){
        if(start>=line.length())throw new NumberFormatException("fixed-column field missing"); int e=Math.min(line.length(),start+width); String z=line.substring(start,e).trim(); if(z.isEmpty())return 0; return Double.parseDouble(z.replace('D','E').replace('d','e'));
    }

    boolean getG() { return getGKernel(); }
    boolean getlay() {
        Random random=new Random(1L); if(lCnt<1)return false;
        lSeq[1]=weighted(random.nextDouble(),lG); if(lSeq[1]<1)return false;
        for(int q=2;q<=lCnt;q++){double[] p=new double[nLayers+1];for(int i=1;i<=nLayers;i++)p[i]=lAlpha[i][lSeq[q-1]];lSeq[q]=weighted(random.nextDouble(),p);if(lSeq[q]<1)return false;} return true;
    }
    private int weighted(double x,double[] p){double s=0;for(int i=1;i<=nLayers;i++){s+=p[i];if(x<=s+EPS14)return i;}return -1;}

    void sphcst() {
        double sg=Math.sin(cellGamma), cg=Math.cos(cellGamma);
        a0=ONE/Math.pow(cellA*sg,2); b0=ONE/Math.pow(cellB*sg,2); c0=ONE/(cellC*cellC); d0=-TWO*cg/(cellA*cellB*sg*sg);
        ab0=Math.sqrt(a0*b0);bc0=Math.sqrt(b0*c0);ca0=Math.sqrt(c0*a0);
    }
    void detun() { for(int i=1;i<=nLayers;i++)for(int j=1;j<=nLayers;j++)detune[j][i]=ONE-EPS3; }

    /** Essential numerical precomputation from OPTIMZ. Symmetry auto-detection and
     * overlap diagnostics are intentionally not needed to evaluate user-specified hkl points. */
    void optimz() {
        for(int i=1;i<=nLayers;i++)for(int j=1;j<=nLayers;j++)there[j][i]=lAlpha[j][i]>=EPS7;
        // NMCOOR: atom coordinates are thereafter phases in radians.
        for(int m=1;m<=nActual;m++)for(int j=1;j<=lNAtoms[m];j++)for(int q=1;q<=3;q++)aPos[q][j][m]*=PI2;
        for(int i=1;i<=nActual;i++){
            double avg=0;for(int j=1;j<=lNAtoms[i];j++)avg+=aB[j][i];avg/=lNAtoms[i];double err=0;for(int j=1;j<=lNAtoms[i];j++)err+=Math.abs(aB[j][i]-avg);if(avg!=0)err/=avg*lNAtoms[i];oneB[i]=Math.abs(err)<=EPS3;
        }
        allBsZero=true;
        for(int i=1;i<=nLayers;i++)for(int j=1;j<=nLayers;j++){bsZero[j][i]=rB11[j][i]==0&&rB22[j][i]==0&&rB33[j][i]==0&&rB12[j][i]==0&&rB23[j][i]==0&&rB31[j][i]==0;allBsZero&=bsZero[j][i];}
        double[] av=new double[1]; sameBs=equalB(rB11,av);aB11=av[0];if(sameBs){sameBs=equalB(rB22,av);aB22=av[0];}if(sameBs){sameBs=equalB(rB33,av);aB33=av[0];}if(sameBs){sameBs=equalB(rB12,av);aB12=av[0];}if(sameBs){sameBs=equalB(rB23,av);aB23=av[0];}if(sameBs){sameBs=equalB(rB31,av);aB31=av[0];}
        onlyReal=true;for(int i=1;i<=nActual;i++)onlyReal&=lSymmetry[i]==CENTRO;
        lRz=0;int nr=0;for(int i=1;i<=nLayers;i++)for(int j=1;j<=nLayers;j++)if(there[j][i]){lRz+=lR[3][j][i];nr++;}if(nr>0)lRz/=nr;double er=0;for(int i=1;i<=nLayers;i++)for(int j=1;j<=nLayers;j++)if(there[j][i])er+=Math.abs(lR[3][j][i]-lRz);sameRz=Math.abs(er)<=EPS4;
        sameLayer=false;if(xplcit&&lCnt>0){sameLayer=true;for(int i=2;i<=lCnt;i++)sameLayer&=lSeq[i]==lSeq[1];}
        if(finiteWidth){int m=FFACT_SIZE/2;double incr=(THREE*N_SIGMAS*N_SIGMAS+ONE)/(TWO*N_SIGMAS*m);ffactScale=incr;double zz=ONE+N_SIGMAS*N_SIGMAS,tmp=ONE/(zz*zz);formfactor[m+1]=ONE;for(int n=1;n<=m;n++){double z=n*incr,x=z<=N_SIGMAS?ONE/(ONE+z*z):tmp*(THREE*N_SIGMAS*N_SIGMAS+ONE-TWO*N_SIGMAS*z);if(m+n+1<=FFACT_SIZE)formfactor[m+n+1]=x;if(m-n+1>0)formfactor[m-n+1]=x;}double t=wa*Math.sin(PI-cellGamma);ffwdth=Math.sqrt(ONE/(t*t)+ONE/(wb*wb));}
        if(symGrpNo==UNKNOWN){symGrpNo=1;pntGrp="-1";} hasLMirror=symGrpNo!=1&&symGrpNo!=3&&symGrpNo!=5&&symGrpNo!=6&&symGrpNo!=11;
    }
    private boolean equalB(double[][] b,double[] av){double sum=0;int m=0;for(int i=1;i<=nLayers;i++)for(int j=1;j<=nLayers;j++)if(there[j][i]){sum+=b[j][i];m++;}av[0]=m==0?0:sum/m;double e=0;for(int i=1;i<=nLayers;i++)for(int j=1;j<=nLayers;j++)if(there[j][i])e+=Math.abs(b[j][i]-av[0]);return Math.abs(e)<=Math.abs(EPS3*av[0]);}

    /** FORTRAN XYPHSE: cache in-plane atomic phases for the current h,k. */
    void xyphse(int h, int k) {
        for (int m = 1; m <= nActual; m++)
            for (int i = 1; i <= lNAtoms[m]; i++)
                hxKy[i][m] = h * aPos[1][i][m] + k * aPos[2][i][m];
    }

    /**
     * Public library entry point. Loads an ordinary DIFFaX .dif file plus data.sfc,
     * performs all setup used by point, powder, and SADP calculations, and leaves
     * this class ready for compute calls. All mutable DIFFaX state belongs to this DiffaxModel instance, so multiple
     * models may be loaded and calculated concurrently.
     */
    public synchronized void loadInput(Path input) throws IOException {
        initializeConstants();
        String file = input.toAbsolutePath().normalize().toString();
        boolean ok = rdfile(file);
        if (ok)
          ok = sfc();
        if (ok)
          ok = getG();
        if (ok && rndm)
          ok = getlay();
        if (!ok)
          throw new IOException("Unable to initialize DIFFaX input: "+input);
        sphcst();
        detun();
        optimz();
    }

  /**
   * Public library entry point. Loads an ordinary DIFFaX .dif file plus data.sfc,
   * performs all setup used by point, powder, and SADP calculations, and leaves
   * this class ready for compute calls. All mutable DIFFaX state belongs to this DiffaxModel instance, so multiple
   * models may be loaded and calculated concurrently.
   * int radiationType = X_RAY or NEUTRN or ELECTN
   * double singleLambda
   * double[4] cell: a, b, c, gamma in degrees
   * double[4] v: u, v, w of Caglioti HWHM, and gaussianity (0...1)
   * String pointGroupKey: one of "6/MMM","4/MMM","2/M(1)","2/M(2)","UNKNOWN","AXIAL","-3M","6/M","4/M","MMM","-3","-1"
   * double rest: tolerance in % if symmetry UNKNOWN
   * double[0...2] layerWidth: null or length 0 = infinite, length 1 = diameter, length 2 = width along a, width along b
   * Vector<DiffaxLayer> layers: all layers
   * boolean recursiveMode: true is recursive, false is explicit
   * int[dim] numberStacking: if recursive: null or dim 0 = infinite, dim = 1 = number of permutations
   *                          for explicit: dim = 1 is random and number of layers in the sequence,
   *                          otherwise the explicit sequence for dim > 1
   * double[nLayers][nLayers][3] transitions: all transitions
   * double[nLayers][nLayers][3] fatsWaller: if != null provide the Fats-Waller
   */
  public synchronized void loadModel(int cellDivisions,
                                     double[] v, String pointGroupKey, double rest,
                                     double[] layerWidth, Vector<DiffaxLayer> layers,
                                     boolean recursiveMode, int[] numberStacking,
                                     double[][][] transitions, double[][][] fatsWaller,
                                     int stepFactor)  throws Exception {

    setStepFactor(stepFactor);

    initializeConstants();

    trimOrigin = true;
    if (v.length != 5)
      throw new IllegalArgumentException("PSEUDO-VOIGT requires u v w gamma0 gamma1");
    pvU=v[2];
    pvV=v[1];
    pvW=v[0];
    pvGamma=v[3];
    pvGamma2=v[4];
    blurring=PS_VGT;

    // STRUCTURAL

    double[] cell = new double[4];
    for (int i = 0; i < cell.length - 1; i++)
      cell[i] = phase.getCellValue(i);
    cell[2] /= cellDivisions; // d-hco
    cell[cell.length - 1] = phase.getCellValue(5);
    if (cell[cell.length - 1] != 120)
      throw new IllegalArgumentException("This planar model requires an hexagonal lattice ");

    if (cell.length < 4)
      throw new IllegalArgumentException("STRUCTURAL cell line requires a, b, c, gamma");
    cellA = cell[0];
    cellB = cell[1];
    cellC = cell[2];
    cellGamma = cell[3];
//    System.out.println("Cell: " + cellA + " " +  cellB + " " + cellC + " " + cellGamma);
    if (!(cellA > 0 && cellB > 0 && cellC > 0 && cellGamma > 0 && cellGamma < 180))
      throw new IllegalArgumentException("illegal unit-cell dimensions or gamma");
    cellGamma *= DEG2RAD;

    String[] keys = {"6/MMM","4/MMM","2/M(1)","2/M(2)","UNKNOWN","AXIAL","-3M","6/M","4/M","MMM","-3","-1"};
    int[] groups = {10,8,2,3,12,11,6,9,7,4,5,1};
    int found = -1;
    String key = "";
    for (int i = 0; i < keys.length; i++)
      if (pointGroupKey.startsWith(keys[i])) {
        found = groups[i];
        key = keys[i];
        break;
      }
    if (found < 0)
      throw new IllegalArgumentException("unrecognized diffraction point group: " + pointGroupKey);
    pntGrp = key;
    tolerance = 0.01;
    autoSymmetry = found == 12;
    if (found <= 11) symGrpNo = found;
    else {
      symGrpNo = UNKNOWN;
      tolerance = rest * EPS2;
      if (tolerance < EPS4)
        tolerance = EPS4;
    }

//    System.out.println(pntGrp + " " + tolerance + " " + symGrpNo + " " + autoSymmetry);

    // LAYERS

    nLayers = layers.size();
    if (nLayers < 1 || nLayers > MAX_L)
      throw new IllegalArgumentException("number of layers must be between 1..." + MAX_L + ", actual value: " + nLayers);
    MAX_L = nLayers;

    if (layerWidth == null || layerWidth.length == 0)
      finiteWidth = false;
    else {
      wa = layerWidth[0];
      wb = layerWidth.length == 1 ? wa : layerWidth[1];
      if (wa <= 0 || wb <= 0)
        throw new IllegalArgumentException("layer widths must be positive");
      finiteWidth = !(wa > INF_WIDTH && wb > INF_WIDTH);
    }

//    System.out.println(nLayers + " " + finiteWidth);

    for (int i = 1; i <= nLayers; i++) {
      highAtom[i] = 0;
      lowAtom[i] = 0;
    }
    int actual = 0;
    for (int layer = 1; layer <= nLayers; layer++) {
      DiffaxLayer alayer = layers.elementAt(layer - 1);
      if(alayer.equalTo != null) {
        lActual[layer] = lActual[layers.indexOf(alayer.equalTo) + 1];
        continue;
      }
      actual++;
      lActual[layer] = actual;
      if (alayer.centroSymmetric)
        lSymmetry[actual]=CENTRO;
      else
        lSymmetry[actual]=NONE;

      if (alayer.atoms == null || alayer.atoms.size() == 0)
        throw new IllegalArgumentException("no atoms specified for LAYER " + layer);
      for (int count = 1; count <= alayer.atoms.size(); count++) {
        DiffaxAtom atom = alayer.atoms.get(count - 1);
        aName[count][actual] = atom.name;
        aNumber[count][actual] = count;
        aPos[1][count][actual] = atom.x;
        aPos[2][count][actual] = atom.y;
        aPos[3][count][actual] = atom.z;
        aB[count][actual] = atom.bIso;
        aOccup[count][actual] = atom.occupancy;
        if (aB[count][actual] < 0 || aOccup[count][actual] < 0 || aOccup[count][actual] > 1)
          throw new IllegalArgumentException("illegal B/occupancy on atom: " + atom.name + ", in line " + (count+1));
        highAtom[actual] = Math.max(highAtom[actual], aPos[3][count][actual]);
        lowAtom[actual] = Math.min(lowAtom[actual], aPos[3][count][actual]);
      }
      lNAtoms[actual] = alayer.atoms.size();
    }
    nActual = actual;
    for (int i = 1; i <= nActual; i++) {
      if (lSymmetry[i] == CENTRO) {
        double z = Math.max(highAtom[i], -lowAtom[i]);
        highAtom[i] = z;
        lowAtom[i] = -z;
      }
/*      System.out.println(lSymmetry[i] + " " + lowAtom[i] + " " + highAtom[i]);
      for (int j = 1; j <= lNAtoms[i]; j++) {
        System.out.println(aName[j][i]
            + " " +  aNumber[j][i]
            + " " +  aPos[1][j][i]
            + " " +  aPos[2][j][i]
            + " " +  aPos[3][j][i]
            + " " +  aB[j][i]
            + " " +  aOccup[j][i]);
      }*/
    }
  //  for(int layer=1;layer<=nLayers;layer++)
  //    System.out.print(lActual[layer] + " ");
  //  System.out.println();

    //    STACKING
    recrsv = recursiveMode;
    xplcit = !recursiveMode;
    rndm = false;
    infThick = false;
    lCnt = 0;
    if (recrsv) {
      if (numberStacking == null || numberStacking.length == 0)
        infThick=true;
      else {
        lCnt = numberStacking[0];
        if (lCnt <= 0 || lCnt > RCSV_MAX) {
          lCnt = 0;
          infThick = true;
        }
      }
    } else {
      if (numberStacking == null || numberStacking.length == 0) {
        rndm = true;
        lCnt = XP_MAX;
      } else if (numberStacking.length == 1) {
        rndm = true;
        lCnt = numberStacking[0];
      } else {
        lCnt = Math.min(numberStacking.length, XP_MAX);
        for (int i = 1; i <= lCnt; i++) {
          lSeq[i] = numberStacking[i - 1];
          if (lSeq[i] < 1 || lSeq[i] > nLayers)
            throw new IllegalArgumentException("illegal explicit layer number " + lSeq[i]);
        }
      }
      if (lCnt < 1)
        throw new IllegalArgumentException("illegal explicit layer sequence");
    }

    // TRANSITIONS

    for (int i = 1; i <= nLayers; i++)
      for(int j = 1; j <= nLayers; j++) {
        lAlpha[j][i] = transitions[i-1][j-1][0];
        lR[1][j][i] = lR[2][j][i] = lR[3][j][i] = 0;
        rB11[j][i] = rB22[j][i] = rB33[j][i] = rB12[j][i] = rB31[j][i] = rB23[j][i] = 0;
        if (transitions[i-1][j-1][0] >= 0.0) { //EPS6) {
          for (int k = 0; k < 3; k++)
            lR[k+1][j][i] = transitions[i-1][j-1][k+1];
          if (fatsWaller != null) {
            rB11[j][i] = fatsWaller[i-1][j-1][0];
            rB22[j][i] = fatsWaller[i-1][j-1][1];
            rB33[j][i] = fatsWaller[i-1][j-1][2];
            rB12[j][i] = fatsWaller[i-1][j-1][3];
            rB31[j][i] = fatsWaller[i-1][j-1][4];
            rB23[j][i] = fatsWaller[i-1][j-1][5];
          }
          if (i == j && lR[3][j][i] <= 0)
            throw new IllegalArgumentException("non-positive Rz for self-transition " + i);

        } else if (transitions[i-1][j-1][0] < 0)
          throw new IllegalArgumentException("negative stacking probability at "+i+","+j);
//        System.out.println("Layer " +i + ", to " + j + " " + lAlpha[j][i] + " " + lR[1][j][i] + " " + lR[2][j][i] + " " + lR[3][j][i]);
      }
    for (int i = 1; i <= nLayers; i++) {
      double sum = 0;
      for (int j = 1; j <= nLayers; j++)
        sum += lAlpha[j][i];
      if (Math.abs(sum - 1) > EPS6 && (recrsv || rndm))
        throw new IllegalArgumentException("stacking probabilities from LAYER " + i + " sum to " + sum);
    }

    boolean ok = sfc();
    if (ok)
      ok = getG();
    if (ok && rndm)
      ok = getlay();
    if (!ok)
      throw new IOException("Unable to initialize DIFFaX input");

    sphcst();
    detun();
    optimz();

  }

  public synchronized void loadInput(String input) throws IOException { loadInput(Path.of(input)); }

    /** Compute an h,k,l point using the same kernel used by the original POINT routine. */
    public synchronized double pointIntensity(int h,int k,double l) {
        double s2=q2(h,k,l);
        xyphse(h,k);
        preMat(h,k); Complex[] f=new Complex[MAX_L+1];Arrays.fill(f,Complex.ZERO);getF(f,s2,l);BoolRef ok=new BoolRef(true);double x=xplcit?inten2(f,h,k,l,ok):intens(f,h,k,l,ok);if(!ok.value)throw new ArithmeticException("intensity kernel failed");
        // Original POINT applies the unpolarized X-ray polarization factor W4.
        if(radType==X_RAY && s2>=0){double arg=HALF*lambda*Math.sqrt(s2);if(arg<=ONE){double theta=Math.asin(arg);x*=HALF*(ONE+Math.pow(Math.cos(TWO*theta),2));}}
        return x;
    }

    void point() {
        try { BufferedReader br=new BufferedReader(new InputStreamReader(System.in));System.out.println("Enter h k l:");String[] t=br.readLine().trim().split("\\s+");int h=Integer.parseInt(t[0]),k=Integer.parseInt(t[1]);double l=rd_nmbr(t[2]);System.out.printf(Locale.ROOT,"Intensity at %d %d %.8g = %.12g%n",h,k,l,pointIntensity(h,k,l)); }
        catch(Exception ex){System.err.println("POINT failed: "+ex.getMessage());}
    }

    /** CLI helper for ordinary files: java org.diffax.Diffax file.dif [h k l]. */
    public boolean runFile(String file,String[] args) {
        initializeConstants();
        boolean ok=rdfile(file);if(ok)ok=sfc();if(ok)ok=getG();if(ok&&rndm)ok=getlay();if(!ok)return false;sphcst();detun();optimz();
        if(args.length>=2 && args[1].equalsIgnoreCase("powder")) {
            if(args.length<5) throw new IllegalArgumentException("Usage: file.dif powder 2thetaMin 2thetaMax step [adaptive|fixed]");
            double lo=rd_nmbr(args[2]), hi=rd_nmbr(args[3]), step=rd_nmbr(args[4]);
            boolean adaptive=args.length<6 || !args[5].equalsIgnoreCase("fixed");
            try { Path out=powder(file,lo,hi,step,adaptive); System.out.println("Spectrum written to "+out); }
            catch(IOException ex){throw new UncheckedIOException(ex);}
        } else if(args.length>=2 && args[1].equalsIgnoreCase("sadp")) {
            if(args.length<4) throw new IllegalArgumentException("Usage: file.dif sadp view(1-4) lMax [adaptive|fixed] [8|16] [linear|log] [brightness] [raw|png|both] [size]");
            int view=Integer.parseInt(args[2]); double lmax=rd_nmbr(args[3]);
            boolean adaptive=args.length<5 || !args[4].equalsIgnoreCase("fixed");
            int bits=args.length<6 ? 8 : Integer.parseInt(args[5]);
            boolean linear=args.length<7 || !args[6].equalsIgnoreCase("log");
            double bright=args.length<8 ? ONE : rd_nmbr(args[7]);
            String format=args.length<9 ? "both" : args[8].toLowerCase(Locale.ROOT);
            int size=args.length<10 ? DEFAULT_SADP_SIZE : Integer.parseInt(args[9]);
            try {
                SadpResult r=computeSadp(view,lmax,adaptive,bits,linear,bright,size);
                String base=Path.of(file).getFileName().toString();int dot=base.lastIndexOf('.');if(dot>=0)base=base.substring(0,dot);
                if(format.equals("raw")||format.equals("both")){Path out=currentInputDir.resolve(base+".sadp");writeSadpRaw(out,r);System.out.println("SADP raw image written to "+out);}
                if(format.equals("png")||format.equals("both")){Path out=currentInputDir.resolve(base+".sadp.png");writeSadpPng(out,r);System.out.println("SADP PNG written to "+out);}
                if(!format.equals("raw")&&!format.equals("png")&&!format.equals("both")) throw new IllegalArgumentException("SADP format must be raw, png, or both");
                System.out.printf(Locale.ROOT,"SADP: %dx%d, view=%d, lMax=%.10g, hkLim=%d, %d-bit, %s scaling%n",r.size,r.size,view,r.lUpper,r.hkLim,bits,linear?"linear":"log");
            } catch(IOException ex){throw new UncheckedIOException(ex);}
        } else if(args.length>=4){int h=Integer.parseInt(args[1]);int k=Integer.parseInt(args[2]);double l=rd_nmbr(args[3]);System.out.printf(Locale.ROOT,"I(%d,%d,%.10g) = %.15g%n",h,k,l,pointIntensity(h,k,l));}
        else System.out.println("Input/setup complete. Use: file.dif h k l  OR  file.dif powder min max step [adaptive|fixed]  OR  file.dif sadp view lMax [adaptive|fixed] [8|16] [linear|log] [brightness] [raw|png|both] [size]"); return true;
    }

    /** Adaptive 16-point Gauss-Legendre quadrature, translated from AGLQ16. */
    double aglq16(int h, int k, double a, double b, BoolRef ok) {
        final int maxstk = 200;
        final double epsilon = FIVE * EPS4;
        double[] stk = new double[maxstk + 1];
        double sum3 = glq16(h, k, a, b, ok);
        if (!ok.value)
          return ZERO;
        int stp = maxstk;
        double d1 = a, d3 = b, sum = ZERO;
        while (true) {
            double d2 = HALF * (d1 + d3);
            double sum1 = glq16(h, k, d1, d2, ok);
            if (!ok.value) return ZERO;
            double sum2 = glq16(h, k, d2, d3, ok);
            if (!ok.value) return ZERO;
            double x = sum1 + sum2;
            double epsilon2 = Math.max(epsilon, Math.abs(x) * epsilon);
            if (Math.abs(x - sum3) > epsilon2) {
                if (stp < 3) {
                  ok.value = false;
                  return ZERO;
                }
                stk[stp] = sum2;
                stk[stp - 1] = d2;
                stk[stp - 2] = d3;
                stp -= 3;
                d3 = d2;
                sum3 = sum1;
            } else {
                sum += x;
                if (stp == maxstk)
                  break;
                d3 = stk[stp + 1];
                d1 = stk[stp + 2];
                sum3 = stk[stp + 3];
                stp += 3;
            }
        }
        ok.value = true;
        return sum;
    }

    /** Angle of an h,k vector relative to 1,0, matching FORTRAN HKANGL. */
    private double hkAngle(double kVal, double hVal) {
        return Math.atan2(kVal * Math.sqrt(a0 * b0 - d0 * d0 * QUARTER),
                hVal * a0 + kVal * d0 * HALF);
    }

    /**
     * FORTRAN GET_BDS subset used by powder integration.  This restores the
     * original symmetry wedge and multiplicity/boundary weighting instead of
     * replacing it with a complete h,k scan.
     */
    private void setupPowderBounds() {
        switch (symGrpNo) {
            case 1 -> { hStart=1; kStart=0; hEnd=1; kEnd=0; mltplcty=1; bndsWt=1; rotOnly=true; }
            case 2 -> { hStart=1; kStart=0; hEnd=-1; kEnd=0; mltplcty=2; bndsWt=1; rotOnly=true; }
            case 4 -> { hStart=1; kStart=0; hEnd=0; kEnd=1; mltplcty=4; bndsWt=HALF; rotOnly=false; }
            case 5 -> { hStart=1; kStart=0; hEnd=-1; kEnd=1; mltplcty=3; bndsWt=1; rotOnly=true; }
            case 6 -> { hStart=1; kStart=0; hEnd=0; kEnd=1; mltplcty=6; bndsWt=HALF; rotOnly=false; }
            case 7 -> { hStart=1; kStart=0; hEnd=0; kEnd=1; mltplcty=4; bndsWt=1; rotOnly=true; }
            case 8 -> { hStart=1; kStart=0; hEnd=1; kEnd=1; mltplcty=8; bndsWt=HALF; rotOnly=false; }
            case 9 -> { hStart=1; kStart=0; hEnd=0; kEnd=1; mltplcty=6; bndsWt=1; rotOnly=true; }
            case 10 -> { hStart=1; kStart=0; hEnd=1; kEnd=1; mltplcty=12; bndsWt=HALF; rotOnly=false; }
            case 11 -> { hStart=1; kStart=0; hEnd=1; kEnd=0; mltplcty=1; bndsWt=1; rotOnly=true; }
            default -> {
                // 2/M(2) depends on which vertical mirror OPTIMZ detected.  If
                // unavailable, use the safe centrosymmetric full scan.
                hStart=1; kStart=0; hEnd=1; kEnd=0; mltplcty=1; bndsWt=1; rotOnly=true;
                symGrpNo=1;
            }
        }
        theta1 = hkAngle(kStart, hStart);
        theta2 = hkAngle(kEnd, hEnd);
        if (symGrpNo==1 || symGrpNo==11) { theta1=-PI; theta2=PI; }
    }

    /**
     * Compute a powder pattern for the configured incoherent radiation mixture.
     * Each wavelength is mapped independently into 2theta and its intensity is
     * added with the normalized relative spectral weight.
     */
    int computePowder(double twoThetaMinDeg, double twoThetaMaxDeg,
                             double twoThetaStepDeg, boolean adaptive) {
        RadiationComponent[] components = radiationComponents.length == 0
                ? new RadiationComponent[]{new RadiationComponent(lambda, 1.0)}
                : radiationComponents;
        if (components.length == 1) {
            double saved = lambda;
            try {
                lambda = components[0].wavelength;
                int n = computePowderSingleWavelength(twoThetaMinDeg, twoThetaMaxDeg, twoThetaStepDeg, adaptive);
                double w = components[0].relativeWeight;
                if (w != 1.0) for (int i = 1; i <= n; i++) spec[i] *= w;
                return n;
            } finally { lambda = saved; }
        }

        double saved = lambda;
        double[] total = new double[MAX_SP + 1];
        int npts = -1;
        try {
            for (RadiationComponent component : components) {
                if (component.relativeWeight == 0.0) continue;
                lambda = component.wavelength;
                int n = computePowderSingleWavelength(twoThetaMinDeg, twoThetaMaxDeg, twoThetaStepDeg, adaptive);
                if (npts < 0) npts = n;
                else if (n != npts) throw new IllegalStateException("Internal powder-grid mismatch between wavelengths");
                for (int i = 1; i <= n; i++) total[i] += component.relativeWeight * spec[i];
            }
            Arrays.fill(spec, ZERO);
            if (npts < 0) throw new IllegalStateException("Radiation mixture has no non-zero component");
            System.arraycopy(total, 0, spec, 0, npts + 1);
            Arrays.fill(brdSpc, ZERO);
            return npts;
        } finally {
            lambda = saved;
        }
    }

    /** Original single-wavelength GETSPC translation used internally by the mixture wrapper. */
    private int computePowderSingleWavelength(double twoThetaMinDeg, double twoThetaMaxDeg,
                             double twoThetaStepDeg, boolean adaptive) {
        if (!(twoThetaMinDeg >= 0.0 && twoThetaMinDeg < twoThetaMaxDeg && twoThetaMaxDeg <= 180.0))
            throw new IllegalArgumentException("Require 0 <= 2theta_min < 2theta_max <= 180 degrees");
        if (!(twoThetaStepDeg > 0.0 && twoThetaStepDeg <= twoThetaMaxDeg - twoThetaMinDeg))
            throw new IllegalArgumentException("2theta increment is invalid");
        if (twoThetaMaxDeg == 180.0) twoThetaMaxDeg -= EPS4;

        th2Min = twoThetaMinDeg * DEG2RAD;
        th2Max = twoThetaMaxDeg * DEG2RAD;
        dTheta = HALF * DEG2RAD * twoThetaStepDeg;
        double minTh = HALF * th2Min, maxTh = HALF * th2Max;
        int npts = (int) (HALF * (th2Max - th2Min) / dTheta) + 1;
        if (npts > MAX_SP) {
            dTheta = HALF * (th2Max - th2Min) / (MAX_SP - 1);
            npts = MAX_SP;
        }
        Arrays.fill(spec, ZERO);
        Arrays.fill(brdSpc, ZERO);

        setupPowderBounds();
        double qMax = TWO * Math.sin(maxTh) / lambda;
        double fact = TWO / lambda; fact *= fact;
        double discr = FOUR * a0 * b0 - d0 * d0;
        if (discr <= ZERO) throw new IllegalArgumentException("Illegal cell parameters: 4*a0*b0-d0*d0 <= 0");
        double tmp3 = TWO * qMax * Math.sqrt(ONE / discr);
        int hUpper = (int) (tmp3 * Math.sqrt(b0));
        int kUpper = (int) (tmp3 * Math.sqrt(a0));
        BoolRef ok = new BoolRef(true);

        for (int h = -hUpper; h <= hUpper; h++) {
            for (int k = -kUpper; k <= kUpper; k++) {
                if (q2(h, k, ZERO) > qMax * qMax + EPS14) continue;
                boolean lAxis = h == 0 && k == 0;
                double hkTh = lAxis ? theta1 : hkAngle(k, h);
                boolean inWedge = ((theta1-hkTh)*(theta2-hkTh) <= EPS3) || symGrpNo==1;
                if (!inWedge) continue;
                if (rotOnly && (theta2-hkTh) <= EPS3 && symGrpNo!=1) continue;
                if (symGrpNo==11 && !lAxis) continue;
                boolean onBndry = Math.abs(hkTh-theta1)<=EPS3 || Math.abs(hkTh-theta2)<=EPS3;

                xyphse(h, k);
                preMat(h, k);
                if (finiteWidth) {
                    double sg = Math.sin(PI - cellGamma);
                    double tmp2 = (h + k * Math.cos(PI - cellGamma)) / (wa * sg);
                    double tmp3w = k / wb;
                    ffhkcnst = QUARTER * lambda * Math.sqrt(a0 * tmp2 * tmp2 + b0 * tmp3w * tmp3w);
                }

                double theta;
                double l1;
                if (lAxis) {
                    double t = Math.min(dTheta, maxTh);
                    if (t < minTh) t = minTh;
                    l1 = powderLL(t, h, k, fact);
                    theta = t;
                } else {
                    theta = powderAngle(h, k, ZERO);
                    if (theta < minTh) {
                        l1 = powderLL(minTh, h, k, fact);
                        theta = powderAngle(h, k, l1);
                    } else l1 = ZERO;
                }
                if (!Double.isFinite(l1) || theta > maxTh + EPS10) continue;
                int m = (int) ((theta - minTh) / dTheta) + 1;
                if (m < 1) m = 1;

                for (double th = theta; th < maxTh - EPS10 && m <= npts; th += dTheta, m++) {
                    double l0 = l1;
                    double panel = Math.min(dTheta, maxTh - th);
                    l1 = powderLL(th + panel, h, k, fact);
                    if (!Double.isFinite(l1)) break;
                    double x = adaptive ? aglq16(h, k, l0, l1, ok) : glq16(h, k, l0, l1, ok);
                    if (!ok.value) throw new ArithmeticException("quadrature failed at h="+h+" k="+k+" l="+l0+".."+l1);
                    double mid = th + HALF * panel;
                    double lMid = HALF * (l0 + l1);
                    x *= TWO * effectiveGeometryWeight(mid, h, k, lMid);
                    if (!lAxis) {
                        x *= mltplcty;
                        if (onBndry) x *= bndsWt;
                    }
                    if (finiteWidth) chwdth(h, k, l0, l1, x, m, npts);
                    else spec[m] += x;
                }
            }
        }
        return npts;
    }



    /** Translation of DIFFaX CHWDTH: redistribute intensity for finite lateral layer widths. */
    void chwdth(int h, int k, double l0, double l1, double x, int m, int maxIndx) {
        if (m < 1 || m > maxIndx || x == ZERO) return;

        // Special phenomenological treatment of 00l: balance streak-like and disk-like broadening.
        if (h == 0 && k == 0 && x > TEN * tinyInty) {
            double l = HALF * (l0 + l1);
            double denom = lambda * cellC * ffwdth * ffwdth;
            if (!(denom > ZERO)) { spec[m] += x; return; }
            double dHk = TWO * l * dTheta / denom;
            double norm;
            int indx;
            double xx = wa / wb;
            if (xx > ONE) xx = ONE / xx;

            // For a square lateral shape (Wa == Wb), the Fortran dx simplifies to
            // 1/(1+i*dHk).  Sum the same finite series via digamma rather than
            // iterating several million tail terms for small dHk.
            if (Math.abs(xx - ONE) <= EPS14 && dHk > ZERO) {
                double threshold = (ONE / EPS5 - ONE) / dHk;
                indx = (int)Math.floor(threshold) + 1; // first i for which dx < eps5
                double aa = ONE / dHk;
                norm = (digamma(indx + 1.0 + aa) - digamma(1.0 + aa)) / dHk;
                int last = Math.min(indx - 1, maxIndx - m);
                for (int i = 0; i <= last; i++) {
                    int ii = i + 1;
                    brdSpc[m + i] = ONE / (ONE + ii * dHk);
                }
            } else {
                norm = ZERO;
                indx = 0;
                while (true) {
                    indx++;
                    double tmp = ONE / (ONE + indx * dHk);
                    double dx = (((ONE - xx) * Math.sqrt((double) indx) * tmp) + xx) * tmp;
                    if (m + indx - 1 <= maxIndx) brdSpc[m + indx - 1] = dx;
                    norm += dx;
                    if (dx < EPS5) break;
                }
            }
            if (!(norm > ZERO)) { spec[m] += x; return; }
            norm = x / norm;
            for (int i = 0; i <= indx - 1; i++) {
                if (m + i <= maxIndx) spec[m + i] += norm * brdSpc[m + i];
            }
            return;
        }

        // Off-axis broadening uses the precomputed symmetric pseudo-Lorentzian form factor.
        double sMid = q2(h, k, HALF * (l0 + l1));
        if (!(sMid > ZERO)) { spec[m] += x; return; }
        double hWdth = ffhkcnst / Math.sqrt(sMid);
        double scale = (hWdth > ZERO && ffactScale > ZERO)
                ? dTheta / (ffactScale * hWdth) : FFACT_SIZE;

        int p = FFACT_SIZE / 2 + 1;
        double norm = ONE;
        brdSpc[m] = ONE;
        int indx = 0;
        while (true) {
            indx++;
            double nHw = indx * scale;
            int n = (int) nHw;
            if (n >= p - 1) break;
            double frac = nHw - n;
            double avg = (ONE - frac) * formfactor[p + n] + frac * formfactor[p + n + 1];
            if (m + indx <= maxIndx) brdSpc[m + indx] = avg;
            if (m - indx > 0) brdSpc[m - indx] = avg;
            norm += TWO * avg;
        }

        norm = x / norm;
        spec[m] += norm * brdSpc[m];
        for (int i = 1; i <= indx - 1; i++) {
            if (m + i <= maxIndx) spec[m + i] += norm * brdSpc[m + i];
            if (m - i > 0) spec[m - i] += norm * brdSpc[m - i];
        }
    }

    /** Digamma for positive arguments, used only to accelerate the exact CHWDTH harmonic sum. */
    double digamma(double x) {
        double r = ZERO;
        while (x < 8.0) { r -= ONE / x; x += ONE; }
        double inv = ONE / x, inv2 = inv * inv;
        r += Math.log(x) - HALF * inv
                - inv2 * (1.0/12.0 - inv2 * (1.0/120.0 - inv2 * (1.0/252.0
                - inv2 * (1.0/240.0 - inv2 * (5.0/660.0)))));
        return r;
    }

    /** Translation of DIFFaX TRMSPC. Returns the 2theta cut-off in radians for broadening. */
    double trimSpectrumCutoff(int npts) {
        int iMax = npts;
        // spec[1] is the origin and is always zero in the original indexing.
        int i = 2;
        while (true) {
            i++;
            if (i >= iMax + 1) {
                System.err.println("No peaks were found in spectrum; TRIM cut-off remains zero.");
                return ZERO;
            }
            // Locate the first minimum after the huge low-angle peak.
            if (spec[i] > spec[i - 1]) {
                int iMin = i - 1;
                // Fortran: th2_low = i_min * d_theta. This is a 2theta cut-off in radians.
                return iMin * dTheta;
            }
        }
    }

    private double powderLL(double theta, int h, int k, double fact) {
        double v = (fact * Math.pow(Math.sin(theta), 2) - h*h*a0 - k*k*b0 - h*k*d0) / c0;
        if (v < 0 && v > -EPS12()) v = 0;
        return v < 0 ? Double.NaN : Math.sqrt(v);
    }
    private double EPS12() { return 1e-12; }
    private double powderAngle(int h, int k, double l) {
        double arg = HALF * lambda * Math.sqrt(Math.max(ZERO, q2(h,k,l)));
        if (arg > ONE && arg < ONE + EPS12()) arg = ONE;
        return arg > ONE ? Double.NaN : Math.asin(arg);
    }
    /** Original DIFFaX powder Lorentz-polarization/geometrical factor. */
    private double powderWeight(double theta) {
        double den = Math.sin(theta) * Math.sin(TWO * theta);
        if (Math.abs(den) < EPS14) return ZERO;
        if (radType == X_RAY) return (ONE + Math.pow(Math.cos(TWO * theta), 2)) / den;
        if (radType == NEUTRN || radType == ELECTN) return ONE / den;
        throw new IllegalStateException("Undefined radiation type");
    }

    /**
     * Effective host-or-DIFFaX geometry weight for one powder integration panel.
     * The host callback replaces powderWeight(theta); it does not replace the
     * separate factor TWO in the rod/powder integration.
     */
    private double effectiveGeometryWeight(double theta, int h, int k, double l) {
        GeometryCorrection correction = geometryCorrection;
        if (correction == null) return powderWeight(theta);
        double twoThetaDegrees = TWO * theta / DEG2RAD;
        double w = correction.weight(twoThetaDegrees, h, k, l, lambda);
        if (!Double.isFinite(w) || w < ZERO)
            throw new IllegalArgumentException(
                    "Geometry correction must be finite and >= 0 at 2theta=" +
                    twoThetaDegrees + " deg, h=" + h + " k=" + k +
                    " l=" + l + " wavelength=" + lambda + "; got " + w);
        return w;
    }

    void broadenPowder(double cutOffDeg) {
        if (blurring == NONE) { System.arraycopy(spec, 0, brdSpc, 0, spec.length); return; }
        if (blurring == GAUSS) gaussn(cutOffDeg);
        else if (blurring == LORENZ) lornzn(cutOffDeg);
        else if (blurring == PS_VGT || blurring == PV_GSS || blurring == PV_LRN) pv(cutOffDeg);
        else throw new IllegalStateException("Undefined instrumental broadening type: " + blurring);
    }

    void gaussn(double th2LowDeg) {
        if (fwhm <= ZERO) { blurring = NONE; return; }
        double stdDev = fwhm / Math.sqrt(EIGHT * Math.log(TWO));
        double th2Low = th2LowDeg * DEG2RAD;
        if (th2Low < ZERO || th2Low >= th2Max) th2Low = ZERO;
        int nLow = (int)(HALF * th2Low / dTheta) + 1;
        int nHigh = (int)(HALF * (th2Max - th2Min) / dTheta) + 1;
        Arrays.fill(brdSpc, ZERO);
        double stepDeg = TWO * RAD2DEG * dTheta;
        double k1 = stepDeg / (Math.sqrt(PI2) * stdDev);
        double k2 = HALF * Math.pow(stepDeg / stdDev, 2);
        int max = (int)Math.rint(TWO * TWENTY * stdDev / stepDeg);
        if (max > nHigh) max = nHigh;
        for (int i=0;i<=max;i++) {
            double gss = k1 * Math.exp(-k2 * i * i);
            for (int j=nLow+1;j<=nHigh;j++) {
                double t1 = (j-i)>nLow ? spec[j-i] : ZERO;
                double t2 = (j+i)<=nHigh ? spec[j+i] : ZERO;
                double t=t1+t2; if(i==0)t*=HALF;
                brdSpc[j]+=gss*t;
            }
        }
    }

    void lornzn(double th2LowDeg) {
        if (fwhm <= ZERO) { blurring = NONE; return; }
        double th2Low = th2LowDeg * DEG2RAD;
        if (th2Low < ZERO || th2Low >= th2Max) th2Low = ZERO;
        int nLow=(int)(HALF*th2Low/dTheta)+1;
        int nHigh=(int)(HALF*(th2Max-th2Min)/dTheta)+1;
        Arrays.fill(brdSpc,ZERO);
        double stepDeg=TWO*RAD2DEG*dTheta;
        double k1=stepDeg*TWO/(PI*fwhm), k2=Math.pow(stepDeg*TWO/fwhm,2);
        for(int i=0;i<=nHigh;i++){
            double lrnz=k1/(ONE+k2*i*i);
            for(int j=nLow+1;j<=nHigh;j++){
                double t1=(j-i)>nLow?spec[j-i]:ZERO, t2=(j+i)<=nHigh?spec[j+i]:ZERO;
                double t=t1+t2;if(i==0)t*=HALF;brdSpc[j]+=lrnz*t;
            }
        }
    }

    void pv(double th2LowDeg) {

        double th2Low=th2LowDeg*DEG2RAD;
        if(th2Low<ZERO||th2Low>=th2Max)
          th2Low=ZERO;
        int nLow=(int)(HALF*th2Low/dTheta)+1;
        int nHigh=(int)(HALF*(th2Max-th2Min)/dTheta)+1;
        Arrays.fill(brdSpc,ZERO);

        double stepDeg = TWO * RAD2DEG * dTheta;
        double k3 = -c00;
        double th0 = HALF * th2Min;

        boolean useExternalBroadening = (instBroad != null &&
            instBroad.IDlabel.equalsIgnoreCase(InstrumentBroadeningPVCaglioti.modelID));
        DiffrDataFile adatafile = dataFiles[activeDatafileNumber];
        double cryst = phase.getMeanCrystallite();
        double micros = phase.getMeanMicrostrain();

        for (int i = nLow; i <= nHigh; i++) {
          double v = 0;
          double pvG = 0;
          double x = i * dTheta + th0;
          double x2 = 2.0 * x * Constants.PITODEG;
          if (useExternalBroadening) {
            Vector<double[]> instBroadFactor = instBroad.getInstrumentBroadeningAt(x2, adatafile);
            double d_space = lambda / (2.0 * Math.sin(x));
            double[] betaf = new double[2];
            betaf[0] = phase.getActiveSizeStrain().getBetaChauchy(d_space, cryst, micros);
            betaf[1] = phase.getActiveSizeStrain().getBetaGauss(d_space, cryst, micros);
            double broadFactorHWHM = InstrumentBroadening.minimumHWHM;
            double broadFactorEta = 0;
            if (instBroadFactor != null) {
              double[] broadFactorTotal = PseudoVoigtPeak.getHwhmEtaFromIntegralBeta(betaf, instBroadFactor);
              broadFactorHWHM = broadFactorTotal[0];
              broadFactorEta = broadFactorTotal[1];
            }
            v = broadFactorHWHM * 2.0;
            pvG = broadFactorEta;
          } else {
            double tn = Math.tan(x);
            v = (pvU * tn + pvV) * tn + pvW;
            v = Math.sqrt(Math.max(v, InstrumentBroadening.minimumHWHM));
            pvG = pvGamma + pvGamma2 * x2;
            if (pvG < 0)
              pvG = 0;
            if (pvG > 1)
              pvG = 1;
          }

          double inv = 1.0 / v;
          double k1 = pvG * TWO_OVER_PI;
          double k2 = (1.0 - pvG) * sqrtC0PI;
          double q = stepDeg * inv;
          double k4 = k1 * q;
          double k5 = k2 * q;
          q *= q;
          double si = spec[i];
          for (int j = nLow - i; j <= nHigh - i; j++) {
            double r = q * j * j;
            double p = (k4 / (ONE + FOUR * r) + k5 * Math.exp(k3 * r)) * si;
            brdSpc[i + j] += p;
          }
        }
    }

    /** In-memory powder result for package integration. Arrays are one element per 2theta sample. */
    public final class PowderResult {
        public final double[] twoThetaDeg, rawIntensity, broadenedIntensity;
        public final boolean hasInstrumentalBroadening;
        PowderResult(double[] t,double[] raw,double[] broadened,boolean has){twoThetaDeg=t;rawIntensity=raw;broadenedIntensity=broadened;hasInstrumentalBroadening=has;}
    }

    /** Compute powder data in memory, including CHWDTH, TRIM and instrumental broadening. */
    public synchronized PowderResult computePowderPattern(double minDeg,double maxDeg,double stepDeg,boolean adaptive){
        int n=computePowder(minDeg,maxDeg,stepDeg,adaptive);double cut=ZERO;if(trimOrigin&&Math.abs(th2Min)<=EPS14)cut=trimSpectrumCutoff(n);broadenPowder(cut*RAD2DEG);
        double[] t=new double[n],raw=new double[n],broadened=new double[n];double tt=th2Min*RAD2DEG,step=TWO*RAD2DEG*dTheta;
        for(int i=0;i<n;i++,tt+=step) {
          t[i]=tt;
          raw[i]=spec[i+1];
          broadened[i]=blurring==NONE?spec[i+1]:brdSpc[i+1];
          broadened[i] *= intensityCorrection * stepFactor;
        }
        return new PowderResult(t,raw,broadened,blurring!=NONE);
    }

    Path writePowder(Path output, int npts) throws IOException {
        try (BufferedWriter w=Files.newBufferedWriter(output, StandardCharsets.UTF_8)) {
            double twoTheta=th2Min*RAD2DEG;
            double step=TWO*RAD2DEG*dTheta;
            for(int i=1;i<=npts;i++,twoTheta+=step){
                if(blurring==NONE) w.write(String.format(Locale.ROOT," %.5E\t%.6G%n",twoTheta,spec[i]));
                else w.write(String.format(Locale.ROOT," %.5E\t%.6G\t%.6G%n",twoTheta,spec[i],brdSpc[i]));
            }
        }
        return output;
    }

    public synchronized Path powder(String infile,double minDeg,double maxDeg,double stepDeg,boolean adaptive) throws IOException {
        int n=computePowder(minDeg,maxDeg,stepDeg,adaptive);
        double cut=ZERO;
        if (trimOrigin && Math.abs(th2Min) <= EPS14) cut = trimSpectrumCutoff(n);
        broadenPowder(cut * RAD2DEG);
        String name=Path.of(infile).getFileName().toString();int dot=name.lastIndexOf('.');if(dot>=0)name=name.substring(0,dot);
        Path out=currentInputDir.resolve(name+".spc");
        writePowder(out,n);
        return out;
    }

    void dump(String infile) { System.out.println("DUMP output is not required for calculation; Java setup summary: layers="+nLayers+", unique layers="+nActual+", atom types="+nAtoms); }
    void gostrk(String infile) { System.out.println("STREAK menu wrapper is not yet interactive; use pointIntensity()/GLQ16 from the API."); }
    void gointr() { System.out.println("Integrated intensity is available through glq16() from the Java API."); }
    void gospec(String infile) {
        try {
            BufferedReader br=new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
            System.out.println("Enter angular range: 2theta min, 2theta max, 2theta increment.");
            String[] t=br.readLine().trim().split("\\s+");
            double lo=rd_nmbr(t[0]), hi=rd_nmbr(t[1]), step=rd_nmbr(t[2]);
            System.out.println("Enter 1 for adaptive quadrature (recommended for sharp peaks), 0 for fixed GLQ16:");
            boolean adaptive=Integer.parseInt(br.readLine().trim())==1;
            Path out=powder(infile,lo,hi,step,adaptive);
            System.out.println("Spectrum written to '"+out+"'.");
        } catch(Exception ex){System.err.println("POWDER failed: "+ex.getMessage());}
    }
    /** Result of a selected-area diffraction pattern calculation. */
    public final class SadpResult {
        public final int[][] pixels;       // [row][column], values 0..maxPixel
        public final int view, hkLim, bitDepth, maxPixel, size;
        public final double lUpper, scale;
        public final boolean linearScaling;
        SadpResult(int[][] pixels,int view,int hkLim,double lUpper,int bitDepth,int maxPixel,double scale,boolean linearScaling,int size){
            this.pixels=pixels;this.view=view;this.hkLim=hkLim;this.lUpper=lUpper;this.bitDepth=bitDepth;this.maxPixel=maxPixel;this.scale=scale;this.linearScaling=linearScaling;this.size=size;
        }
    }

    private int nint(double x){ return x>=ZERO ? (int)Math.floor(x+HALF) : (int)Math.ceil(x-HALF); }

    /**
     * Determine only the horizontal (l -> -l) mirror needed by GETSAD/WRTSAD when
     * the input requested UNKNOWN symmetry.  DIFFaX's full GET_SYM classifies ten
     * point groups; SADP raster centering needs just this one consequence.
     */
    boolean detectLMirrorForSadp(){
        if(!autoSymmetry)return hasLMirror;
        int[][] hk={{1,0},{0,1},{1,1},{2,0},{2,1}};
        double[] ls={0.173,0.417,0.731};
        double qlim=FOUR/(lambda*lambda);int tested=0;
        for(int[] pair:hk)for(double l:ls){int h=pair[0],k=pair[1];if(q2(h,k,l)>=qlim||q2(h,k,-l)>=qlim)continue;
            double a=pointIntensity(h,k,l),b=pointIntensity(h,k,-l),avg=HALF*(Math.abs(a)+Math.abs(b));
            double tol=avg<tinyInty?tinyInty:Math.max(EPS14,avg*tolerance);
            tested++;if(Math.abs(a-b)>=tol)return false;
        }
        return tested>0;
    }

    /** Compute SADP using this model's configured default resolution (historically 256). */
    public synchronized SadpResult computeSadp(int view,double requestedLUpper,boolean adaptive,int bits,boolean linear,double bright){
        return computeSadp(view,requestedLUpper,adaptive,bits,linear,bright,DEFAULT_SADP_SIZE);
    }

    /**
     * Translation of DIFFaX GETSAD + WRTSAD with a runtime-selected square raster size.
     * The size must be even and >=16.  The SADP integration workspace is allocated to
     * the exact requirement for this call instead of consuming the powder spectrum array.
     */
    public synchronized SadpResult computeSadp(int view,double requestedLUpper,boolean adaptive,int bits,boolean linear,double bright,int size){
        validateSadpSize(size);
        if(view<1||view>4) throw new IllegalArgumentException("SADP view must be 1 (k=0), 2 (h=0), 3 (h=k), or 4 (h=-k)");
        if(bits!=8&&bits!=16) throw new IllegalArgumentException("SADP bit depth must be 8 or 16");
        if(!(bright>ZERO)) throw new IllegalArgumentException("SADP brightness must be positive");
        if(!(requestedLUpper>ZERO)) throw new IllegalArgumentException("SADP lMax must be positive");
        if(autoSymmetry) hasLMirror=detectLMirrorForSadp();
        bitdepth=bits; maxsad=bits==16 ? (int)(FIFTEENBITS-ONE) : (int)(EIGHTBITS-ONE);
        loglin=linear?1:0; brightness=bright;
        double lUpper=requestedLUpper;
        double q2lim=FOUR/(lambda*lambda);
        double s00=q2(0,0,lUpper);
        if(!(s00>ZERO)) throw new IllegalArgumentException("Illegal SADP upper bound: 1/d^2 <= 0");
        if(s00>q2lim) lUpper=TWO/(lambda*Math.sqrt(c0))-EPS10;
        int hkLim;
        if(view==1) hkLim=(int)(lUpper*Math.sqrt(c0/a0));
        else if(view==2) hkLim=(int)(lUpper*Math.sqrt(c0/b0));
        else if(view==3) hkLim=(int)(lUpper*Math.sqrt(c0/(a0+b0+d0)));
        else hkLim=(int)(lUpper*Math.sqrt(c0/(a0+b0-d0)));
        double dl=lUpper/(size/2.0);
        lUpper-=HALF*dl;
        double lLower;
        final int block = hasLMirror ? size/2 : size;
        if(hasLMirror) lLower=-HALF*dl; else lLower=-lUpper;
        long needed=(long)(hkLim+1)*block+2L;
        if(needed>Integer.MAX_VALUE-8) throw new IllegalArgumentException("Requested SADP is too large for a Java array");
        double[] sadSpec=new double[(int)needed];
        int cnt=0, origin=-1;
        BoolRef ok=new BoolRef(true);
        for(int i=0;i<=hkLim;i++){
            int h,k;
            if(view==1){h=i;k=0;}else if(view==2){h=0;k=i;}else if(view==3){h=i;k=i;}else{h=i;k=-i;}
            xyphse(h,k);preMat(h,k);
            if(!hasLMirror){cnt++;sadSpec[cnt]=ZERO;}
            int nSteps=hasLMirror ? size/2 : size-1;
            for(int step=0;step<nSteps;step++){
                double l=lLower+step*dl;
                double x;
                if(i==0 && Math.abs(l+dl)<=dl+EPS10){x=ZERO;origin=cnt+1;}
                else if(q2(h,k,l+dl)>q2lim){x=ZERO;}
                else {
                    x=adaptive?aglq16(h,k,l,l+dl,ok):glq16(h,k,l,l+dl,ok);
                    if(!ok.value) throw new ArithmeticException("SADP quadrature failed at h="+h+" k="+k+" l="+l);
                    if(radType==X_RAY){double sm=q2(h,k,l+HALF*dl);double arg=HALF*lambda*Math.sqrt(Math.max(ZERO,sm));if(arg<=ONE){double theta=Math.asin(arg);x*=HALF*(ONE+Math.pow(Math.cos(TWO*theta),2));}}
                }
                cnt++;sadSpec[cnt]=x;
            }
        }
        if(cnt<2||origin<1) throw new IllegalStateException("SADP scan produced too few points or no origin");
        if(hasLMirror) sadSpec[origin]=(ONE+EPS4)*sadSpec[origin+1];
        else sadSpec[origin]=(ONE+EPS4)*Math.max(sadSpec[origin-1],sadSpec[origin+1]);

        double high1=ZERO,high2=ZERO;
        for(int i=0;i<=hkLim;i++){
            for(int j=1;j<=block-1;j++){
                int n=i*block+j;
                if(!linear) sadSpec[n]=(ONE+sadSpec[n]>ZERO)?Math.log(ONE+sadSpec[n]):ZERO;
                if(n==1&&origin==1){high1=sadSpec[origin];continue;}
                double x=sadSpec[n];
                boolean peak=(j==1)?x>sadSpec[n+1]:(x>sadSpec[n-1]&&x>sadSpec[n+1]);
                if(peak){if(x>high1){high2=high1;high1=x;}else if(x>high2)high2=x;}
            }
        }
        if(linear&&!(high2>ZERO)) throw new ArithmeticException("SADP linear intensity scaling failed: second-highest peak <= 0");
        if(!linear&&!(high1>ZERO)) throw new ArithmeticException("SADP logarithmic intensity scaling failed: highest peak <= 0");
        scaleint=bright*(maxsad-ONE)/(linear?high2:high1);
        int[][] pixels=renderSadpPixels(view,lUpper,hkLim,size,block,sadSpec);
        return new SadpResult(pixels,view,hkLim,lUpper,bits,maxsad,scaleint,linear,size);
    }

    /** Translation of WRTSAD's reciprocal-space placement. */
    private int[][] renderSadpPixels(int view,double lUpper,int hkLim,int size,int block,double[] sadSpec){
        int[][] img=new int[size][size];
        double incr=(size/2.0)/lUpper;
        if(view==1)incr*=Math.sqrt(a0/c0);else if(view==2)incr*=Math.sqrt(b0/c0);else if(view==3)incr*=Math.sqrt((a0+b0+d0)/c0);else incr*=Math.sqrt((a0+b0-d0)/c0);
        int outRow=0;
        for(int j=block-1;j>=0;j--) img[outRow++]=sadpRow(j,incr,hkLim,false,size,block,sadSpec);
        if(hasLMirror){for(int j=1;j<=block-1;j++)img[outRow++]=sadpRow(j,incr,hkLim,true,size,block,sadSpec);img[outRow++]=new int[size];}
        if(outRow!=size)throw new IllegalStateException("Internal SADP row count="+outRow);
        return img;
    }

    private int[] sadpRow(int j,double incr,int hkLim,boolean mirroredBottom,int size,int block,double[] sadSpec){
        double[] row=new double[size];
        for(int i=0;i<=hkLim;i++){
            int p1=size/2+nint(i*incr);
            int p2=size/2-nint(i*incr);
            if(p1<0||p1>=size||p2<0||p2>=size)continue;
            double x=sadSpec[i*block+j+1]*scaleint;if(x>maxsad)x=maxsad;row[p1]=x;
            if(mirroredBottom||hasLMirror)row[p2]=x;
            else {x=sadSpec[i*block+block-j]*scaleint;if(x>maxsad)x=maxsad;row[p2]=x;}
        }
        smudge(row,ZERO);
        int[] ans=new int[size];for(int i=0;i<size;i++){double x=Math.max(ZERO,Math.min(maxsad,row[i]));ans[i]=(bitdepth==8?nint(x):(int)x);}return ans;
    }

    /** Translation of SMUDGE. WRTSAD currently calls this with sigma=0, but it is exposed for parity. */
    void smudge(double[] array,double sigma){
        if(sigma==ZERO)return;int n=array.length;double[] tmp=array.clone();Arrays.fill(array,ZERO);
        for(int i=0;i<n;i++)if(tmp[i]>maxsad)tmp[i]=maxsad;
        int m=Math.min(n,nint(FIVE*sigma));double k1=HALF/(sigma*sigma),norm=ONE;
        for(int i=1;i<=m;i++)norm+=TWO*Math.exp(-k1*i*i);if(norm==ZERO)throw new ArithmeticException("SMUDGE zero normalization");norm=ONE/norm;
        for(int d=0;d<=m;d++){double g=Math.exp(-k1*d*d);for(int j=0;j<n;j++){double a=j-d>=0?tmp[j-d]:ZERO,b=j+d<n?tmp[j+d]:ZERO,v=a+b;if(d==0)v*=HALF;array[j]+=g*v*norm;}}
    }

    /** Raw 256x256 raster: 8-bit unsigned or 16-bit unsigned big-endian. No compiler record markers. */
    public Path writeSadpRaw(Path output,SadpResult r)throws IOException{
        try(OutputStream os=Files.newOutputStream(output)){for(int[] row:r.pixels)for(int v:row){if(r.bitDepth==8)os.write(v&255);else{os.write((v>>>8)&255);os.write(v&255);}}}return output;
    }

    /** Portable PNG representation of the same scaled SADP raster. */
    public Path writeSadpPng(Path output,SadpResult r)throws IOException{
        int height=r.pixels.length, width=height==0?0:r.pixels[0].length;
        BufferedImage im=new BufferedImage(width,height,BufferedImage.TYPE_USHORT_GRAY);
        for(int y=0;y<height;y++){if(r.pixels[y].length!=width)throw new IllegalArgumentException("Ragged SADP pixel matrix");for(int x=0;x<width;x++){int v=r.pixels[y][x];int g=(int)Math.round(65535.0*v/r.maxPixel);im.getRaster().setSample(x,y,0,g);}}
        if(!ImageIO.write(im,"PNG",output.toFile()))throw new IOException("No PNG writer available");return output;
    }

    void gosadp(String infile) {
        try{
            BufferedReader br=new BufferedReader(new InputStreamReader(System.in,StandardCharsets.UTF_8));
            System.out.println("Enter 1 for adaptive quadrature, 0 for fixed GLQ16:");boolean adaptive=Integer.parseInt(br.readLine().trim())==1;
            System.out.println("Choose plane: 1 k=0, 2 h=0, 3 h=k, 4 h=-k:");int view=Integer.parseInt(br.readLine().trim());
            System.out.println("Enter maximum l:");double lu=rd_nmbr(br.readLine());
            System.out.println("Choose bit depth (8 or 16):");int bits=Integer.parseInt(br.readLine().trim());
            System.out.println("Choose scaling: 0 logarithmic, 1 linear:");boolean linear=Integer.parseInt(br.readLine().trim())==1;
            System.out.println("Enter positive brightness:");double bright=rd_nmbr(br.readLine());
            SadpResult r=computeSadp(view,lu,adaptive,bits,linear,bright);
            String base=Path.of(infile).getFileName().toString();int dot=base.lastIndexOf('.');if(dot>=0)base=base.substring(0,dot);
            Path raw=currentInputDir.resolve(base+".sadp"),png=currentInputDir.resolve(base+".sadp.png");writeSadpRaw(raw,r);writeSadpPng(png,r);
            System.out.println("SADP written to '"+raw+"' and '"+png+"'.");
        }catch(Exception ex){System.err.println("SADP failed: "+ex.getMessage());}
    }

    public void salute() {
        System.out.println("DIFFaX Java port");
        System.out.println("Based on DIFFaX 1.812/1.813-era FORTRAN source by Treacy and Deem.");
    }
}
