/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import com.jtex.arrays.Array1D;
import com.jtex.geom.projection.Projector;
import com.jtex.geom.projection.SchmidtProjection;
import com.jtex.geom.projection.SphericalProjection;
import com.jtex.geom.Vec2;
import com.jtex.geom.Vec3;
import com.jtex.interp.interp2;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Ellipse2D;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import javax.swing.JFrame;

/**
 *
 * @author hios
 */
public class SphericalCanvasSmooth extends SphericalCanvas implements
        ColorMapCanvas {

    public static void main(String[] args) {

        Array1D theta = Array1D.linspace(0, Math.PI / 2, 19);
        Array1D rho = Array1D.linspace(0, 2 * Math.PI, 12);

        Array1D data = theta.repeat(12).cos().multiply(rho.expand(19).sin());

        SphericalCanvas smd = new SphericalCanvasScatter(new Vec3(theta.repeat(12), rho.expand(19)), data,
                new SchmidtProjection(), new SphericalBounds(Math.PI / 2, Math.PI), new PlotAnnotation(data.max(), data.min()), new ScatterOptions());

        SphericalCanvasSmooth smo = new SphericalCanvasSmooth(theta, rho, data);

        JFrame frm = new JFrame();

        SimpleMultiPlotComponent p = new SimpleMultiPlotComponent();

        p.add(smo);
        p.add(smd);

        frm.getContentPane().add(p);
        frm.getContentPane().setBackground(Color.WHITE);
        frm.setVisible(true);
        frm.setSize(500, 500);
        frm.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

    }

    Array1D data;
    double dmin, dmax, cmin, cmax;

    interp2 polant;

    ColorMap cmp;
    Color color = Color.BLUE;

    public SphericalCanvasSmooth(Array1D theta, Array1D rho, Array1D data) {
        this(theta, rho, data, new SchmidtProjection());
    }

    public SphericalCanvasSmooth(Array1D theta, Array1D rho, Array1D data, Projector prj) {
        this(theta, rho, data, null, null, prj);
    }

    public SphericalCanvasSmooth(Array1D theta, Array1D rho, Array1D data, String tr, String br, Projector prj) {
        this(theta, rho, data, prj, new SphericalBounds(), new PlotAnnotation(data.max(), data.min()));
    }

    public SphericalCanvasSmooth(Plotter.PlotGrid grid, Array1D data, Projector prj, PlotAnnotation annon) {
        this(grid.getTheta(), grid.getRho(), data, prj, grid.getBounds(), annon);
    }

    public SphericalCanvasSmooth(Array1D theta, Array1D rho, Array1D data, Projector projector, SphericalBounds bounds, PlotAnnotation annon) {
        super(projector, bounds);
        this.data = data;

        dmin = data.min();
        dmax = data.max();

        polant = new interp2(theta.toDoubleArray(), rho.toDoubleArray(), data.toDoubleArray());

        setColorMap(Plotter.getDefaultColorMap());
        setCLim(dmin, dmax);
        setAnnotation(annon);

    }

    @Override
    protected BufferedImage renderCanvas(int sz) {

        double intend = (getProjector() instanceof SphericalProjection) ? 2D : -1;
        double scale = Math.min((sz - 2 * intend) / (getDx()), (sz - 2 * intend) / (getDy()));

        int sh = (int) ((double) sz / getRatio());
        BufferedImage canvas = new BufferedImage(sz, sh, BufferedImage.TYPE_4BYTE_ABGR);
        Array1D ii = Array1D.index(sz).repeat(sh);
        Array1D jj = Array1D.index(sh).expand(sz);

        Vec2 inv = new Vec2(ii, jj).minus(intend).divide(scale).plus(new Vec2(getXmin(), getYmin()));

        Vec2 invPrj = getProjector().projectInv2(inv);

        double[] x = invPrj.x();
        double[] y = invPrj.y();
        for (int i = 0; i < invPrj.y().length; i++) {
            if (x[i] < 0 || x[i] > 2 * Math.PI) {
                x[i] = (x[i] + 4 * Math.PI) % (2 * Math.PI);
            }
            if (y[i] < 0 || y[i] > 2 * Math.PI) {
                y[i] = (y[i] + 4 * Math.PI) % (2 * Math.PI);
            }
        }
        double dat[] = polant.f(x, y);

        double[] id = ii.toDoubleArray();
        double[] jd = jj.toDoubleArray();

        WritableRaster raster = canvas.getRaster();
        int[] red = cmp.getRed();
        int[] green = cmp.getGreen();
        int[] blue = cmp.getBlue();
        int[] alpha = cmp.getAlpha();
        int nc = red.length;
        int i, j;

        for (int k = 0; k < sz * sh; k++) {
            i = (int) id[k];
            j = (int) jd[k];
            if (x[k] > getSphericalBounds().getThetamax() || Double.isNaN(dat[k])) {
                continue;
            }
            int cd = (int) ((dat[k] - cmin) / (cmax - cmin) * (nc));
            if (cd < 0) {
                cd = 0;
            } else if (cd > nc - 1) {
                cd = nc - 1;
            }
            raster.setSample(i, j, 0, red[cd]);
            raster.setSample(i, j, 1, green[cd]);
            raster.setSample(i, j, 2, blue[cd]);
            raster.setSample(i, j, 3, alpha[cd]);
        }

        Graphics2D g2d = (Graphics2D) canvas.getGraphics();
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        if (getProjector() instanceof SphericalProjection) {
            double sr = 2 * getDr() * scale;
            Ellipse2D.Double eli2 = new Ellipse2D.Double(intend - getXmin() * scale - sr / 2, intend - getYmin() * scale - sr / 2, sr, sr);
            g2d.setColor(Color.black);
            g2d.setStroke(new BasicStroke(1f));
            g2d.draw(eli2);
            g2d.clip(eli2);
        }
        return canvas;
    }

    @Override
    public double getCLimMin() {
        return dmin;
    }

    @Override
    public double getCLimMax() {
        return dmax;
    }

    @Override
    public void setCLim(double cmin, double cmax) {
        this.cmin = cmin;
        this.cmax = cmax;
        canvas = renderCanvas((int) (500 * getRatio()));
    }

    @Override
    public void setColorMap(ColorMap cmp) {
        this.cmp = cmp;
//        renderCanvas((int) (500 * getRatio()));
    }

}
