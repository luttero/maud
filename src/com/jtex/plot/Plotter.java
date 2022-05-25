/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import com.jtex.arrays.Array1D;
import com.jtex.geom.Miller;
import com.jtex.geom.projection.PlainProjection;
import com.jtex.geom.projection.Projector;
import com.jtex.geom.projection.SchmidtProjection;
import com.jtex.geom.Quaternion;
import com.jtex.geom.Vec3;
import com.jtex.qta.ODF;
import com.jtex.qta.kernel.DeLaValleePoussin;
import com.jtex.qta.PoleFigure;
import java.text.DecimalFormat;
import java.util.prefs.Preferences;
import javax.swing.JComponent;
import javax.swing.JFrame;

/**
 *
 * @author hios
 */
public class Plotter {

    public static Preferences prefs = Preferences.userNodeForPackage(Plotter.class);

    static {
//        try {
//            prefs.clear();
//        } catch (BackingStoreException ex) {
//            Logger.getLogger(Plotter.class.getName()).log(Level.SEVERE, null, ex);
//        }
        prefs.put("ColorMap", prefs.get("ColorMap", "WHITEJET"));
        prefs.putDouble("MarkerSize", prefs.getDouble("MarkerSize", 5));

    }

    protected static ColorMap getDefaultColorMap() {
        Preferences pref = Preferences.userNodeForPackage(Plotter.class);
        String name = pref.get("ColorMap", "JET");
//        System.out.println(name);
        ColorMap.Name v = ColorMap.Name.valueOf("JET");
        return ColorMap.getColormap(v, 200);
    }

    protected static double getDefaultMarkerSize() {
        Preferences pref = Preferences.userNodeForPackage(Plotter.class);
        return pref.getDouble("MarkerSize", 5);
    }

    public static class PlotGrid {

        private Array1D theta;
        private Array1D rho;
        private Vec3 r;
        private SphericalBounds bounds;
        int nt, nr;

        public PlotGrid(double res, SphericalBounds bounds) {
            if (bounds == null) {
                bounds = new SphericalBounds();
            }

            res = Math.toRadians(res);
            nt = (int) (bounds.getThetamax() / res);
            nr = (int) (bounds.getRhomax() / res);
            this.theta = Array1D.linspace(0, bounds.getThetamax(), nt);
            this.rho = Array1D.linspace(0, bounds.getRhomax(), nr);
            this.bounds = bounds;
        }

        public Vec3 getR() {
            if (r == null) {
                r = new Vec3(getThetaE(), getRhoE());
            }
            return r;
        }

        public Array1D getRho() {
            return rho;
        }

        public Array1D getTheta() {
            return theta;
        }

        public Array1D getRhoE() {
            return rho.expand(nt);
        }

        public Array1D getThetaE() {
            return theta.repeat(nr);
        }

        public SphericalBounds getBounds() {
            return bounds;
        }

    }

    public static JComponent plot(Vec3 v) {
        return plot(v, Array1D.zeros(v.size()));
    }

    public static JComponent plot(Vec3 v, ScatterOptions opts) {
        return plot(v, Array1D.zeros(v.size()), opts);
    }

    private static SimpleMultiPlotComponent createSimpleMultiPlotComponent() {
//        return new JComponent() {
//            {
//                setLayout(new MultiPlotLayout());
//            }
//        };
        return new SimpleMultiPlotComponent();

    }

    private static PlotGrid createPlotGrid(double res, SphericalBounds bounds) {
        return new PlotGrid(res, bounds);
    }

    public static SimpleMultiPlotComponent plotphi1(ODF odf) {
        SimpleMultiPlotComponent cmp = createSimpleMultiPlotComponent();

        PlotGrid grid = createPlotGrid(2.5, new SphericalBounds(Math.PI / 2, Math.PI / 2));

        Projector prjtr = new PlainProjection();
        Array1D phi1 = Array1D.linspace(0, Math.PI / 2, 10);
        for (int i = 0; i < phi1.size(); i++) {
            Array1D th = grid.getThetaE();
            Quaternion q = Quaternion.rotZXZ(Array1D.fill(th.size(), phi1.get(i)), grid.getThetaE(), grid.getRhoE());
            Array1D f = odf.eval(q);
            SphericalCanvas canvas = new SphericalCanvasSmooth(grid, f, prjtr,
                    new PlotAnnotation(f.max(), f.min(), PlotAnnotation.getFormatted("\u03C6_1\n%4.1f", Math.toDegrees(phi1.get(i)))));
            cmp.add(canvas);
        }
        return cmp;
    }

    public static SimpleMultiPlotComponent plotDiff(ODF odf, PoleFigure pfsource) {

        return plot(odf.calcErrorPF(pfsource));

    }

    public static SimpleMultiPlotComponent plotsigma(ODF odf) {
        SimpleMultiPlotComponent cmp = createSimpleMultiPlotComponent();

        PlotGrid grid = createPlotGrid(2.5, null);

        Projector prjtr = new SchmidtProjection();

        Array1D gamma = Array1D.linspace(0, Math.PI / 2, 10);
        for (int i = 0; i < gamma.size(); i++) {
            Quaternion q = Quaternion.rotZYZ(grid.getRhoE(), grid.getThetaE(), grid.getRhoE().lminus(gamma.get(i)));

            Array1D f = odf.eval(q);
            SphericalCanvasSmooth canvas = new SphericalCanvasSmooth(grid, f, prjtr,
                    new PlotAnnotation(f.max(), f.min(), PlotAnnotation.getFormatted("\u03C3\n%4.1f", Math.toDegrees(gamma.get(i)))));
            cmp.add(canvas);
        }
        return cmp;
    }

    public static SimpleMultiPlotComponent plotpdf(ODF odf, Miller h) {
        SimpleMultiPlotComponent cmp = createSimpleMultiPlotComponent();
        PlotGrid grid = createPlotGrid(2.5D, new SphericalBounds());

        Projector prjtr = new SchmidtProjection();

        PoleFigure pf = odf.calcPoleFigure(h, grid.getR());
        for (int i = 0; i < pf.size(); i++) {
            PoleFigure pfi = pf.get(i);
            SphericalCanvasSmooth canvas = new SphericalCanvasSmooth(grid, pfi.getData(),
                    prjtr, new PlotAnnotation(pfi.getData().max(), pfi.getData().min(), pfi.getH().toHKL()));
            cmp.add(canvas);
        }
        return cmp;
    }

    public static SimpleMultiPlotComponent plotipdf(ODF odf, Vec3 s) {

        SimpleMultiPlotComponent cmp = createSimpleMultiPlotComponent();
        PlotGrid grid = createPlotGrid(2.5D, new SphericalBounds());

        Projector prjtr = new SchmidtProjection();
        Miller h = new Miller(grid.getR(), odf.getCS());
        DecimalFormat df = new DecimalFormat("#");
        for (int i = 0; i < s.size(); i++) {

            Array1D pdf = odf.pdf(h, s.get(i));
            String anno = df.format(s.get(i).x()[0]) + df.format(s.get(i).y()[0])
                    + df.format(s.get(i).z()[0]);

            SphericalCanvasSmooth canvas = new SphericalCanvasSmooth(grid, pdf,
                    prjtr, new PlotAnnotation(pdf.max(), pdf.min(), anno));
            cmp.add(canvas);
        }
        return cmp;
    }

    public static SimpleMultiPlotComponent plot(PoleFigure pf) {
        SimpleMultiPlotComponent cmp = createSimpleMultiPlotComponent();
        SphericalBounds sphericalBounds = new SphericalBounds(Math.PI / 2, 2 * Math.PI);
        SchmidtProjection schmidtProjection = new SchmidtProjection();
        ScatterOptions scatterOptions = new ScatterOptions(Shapes.Named.CIRCLE, .5);
        for (int i = 0; i < pf.size(); i++) {
            PoleFigure pft = pf.get(i);
//	        System.out.println("Plotting pole figure of length: " + pft.getData().size());
            Array1D data = pft.getData();
            SphericalCanvas canvas = new SphericalCanvasScatter(
                    pft.getR(),
                    data,
                    schmidtProjection,
                    sphericalBounds,
                    new PlotAnnotation(data.max(), data.min(), pft.getH().toHKL(), null),
                    scatterOptions);
            cmp.add(canvas);
        }
        return cmp;
    }

    public static SimpleMultiPlotComponent plotQuadratureWeights(PoleFigure pf) {
        SimpleMultiPlotComponent cmp = createSimpleMultiPlotComponent();
        for (int i = 0; i < pf.size(); i++) {
            PoleFigure pft = pf.get(i);
            Array1D quadratureWeights = pft.getQuadratureWeights(new DeLaValleePoussin(Math.toRadians(5)));

            SphericalCanvas canvas = new SphericalCanvasScatter(
                    pft.getR(), quadratureWeights, pft.getH().toHKL(), null);
            cmp.add(canvas);
        }

        return cmp;
    }

    public static SimpleMultiPlotComponent plot(ODF odf) {

        Array1D phi1 = Array1D.linspace(0, 2 * Math.PI, 72);
        Array1D Phi = Array1D.linspace(0, Math.PI / 2, 19);
        Array1D phi2 = new Array1D(0, 10, 20, 30, 40, 50, 60, 70, 80, 90).toDegrees();
        phi1 = phi1.repeat(19);
        Phi = Phi.expand(72);

        PoleFigure sections = new PoleFigure();
        for (int i = 0; i < phi2.size(); i++) {
            Quaternion g = Quaternion.rotZXZ(phi1, Phi, new Array1D(phi2.get(i)));
            Array1D f = odf.eval(g);
            g = null;
            sections.add(new PoleFigure(new Miller(), new Vec3(Phi, phi1), f));
        }
        return Plotter.plot(sections);

    }

    public static SimpleMultiPlotComponent plotphi2full(ODF odf) {

        SimpleMultiPlotComponent cmp = createSimpleMultiPlotComponent();

        PlotGrid grid = createPlotGrid(2.5, new SphericalBounds(Math.PI / 2, 2 * Math.PI));

        Projector prjtr = new PlainProjection();

        Array1D phi2 = Array1D.linspace(0, Math.PI / 2, 10);
        for (int i = 0; i < phi2.size(); i++) {
            Quaternion q = Quaternion.rotZXZ(grid.getRhoE(), grid.getThetaE(), Array1D.fill(grid.nr * grid.nt, phi2.get(i)));
            Array1D f = odf.eval(q);
            SphericalCanvasSmooth canvas = new SphericalCanvasSmooth(grid, f, prjtr,
                    new PlotAnnotation(f.max(), f.min(), PlotAnnotation.getFormatted("\u03C6_2\n%4.1f", Math.toDegrees(phi2.get(i)))));
            cmp.add(canvas);
        }
        return cmp;
    }

    public static SimpleMultiPlotComponent plotphi2(ODF odf) {

        SimpleMultiPlotComponent cmp = createSimpleMultiPlotComponent();

        PlotGrid grid = createPlotGrid(2.5, new SphericalBounds(Math.PI / 2, Math.PI / 2));

        Array1D phi2 = Array1D.linspace(0, Math.PI / 2, 10);
        for (int i = 0; i < phi2.size(); i++) {
            Quaternion q = Quaternion.rotZXZ(grid.getRhoE(), grid.getThetaE(), new Array1D(phi2.get(i)));
            Array1D f = odf.eval(q);
            SphericalCanvasSmooth canvas = new SphericalCanvasSmooth(grid, f, new PlainProjection(),
                    new PlotAnnotation(f.max(), f.min(), PlotAnnotation.getFormatted("\u03C6_2\n%4.1f", Math.toDegrees(phi2.get(i)))));
            cmp.add(canvas);
        }
        return cmp;
    }

    public static JComponent plot(Vec3 v, Array1D d) {
        return new SphericalCanvasScatter(v, d, new ScatterOptions());
    }

    public static JComponent plot(Vec3 v, Array1D d, ScatterOptions opts) {
        return new SphericalCanvasScatter(v, d, opts);
    }

    public static void show(JComponent cmp) {
        show(cmp, "plot");
    }

    public static void show(JComponent cmp, String title) {
        show(cmp, title, true);
    }

    public static void show(JComponent cmp, String title, boolean exit) {

        JFrame f = new JFrame(title);
        f.setSize(500, 500);
        f.getContentPane().add(cmp);
        f.setVisible(true);
        if (exit) {
            f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        }

    }

}
