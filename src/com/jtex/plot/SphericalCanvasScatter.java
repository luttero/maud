/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import com.jtex.arrays.Array1D;
import com.jtex.geom.grid.S2Grid;
import com.jtex.geom.projection.Projector;
import com.jtex.geom.projection.SchmidtProjection;
import com.jtex.geom.projection.SphericalProjection;
import com.jtex.geom.Vec2;
import com.jtex.geom.Vec3;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.util.Arrays;
import javax.swing.JComponent;

/**
 *
 * @author hios
 */
public class SphericalCanvasScatter extends SphericalCanvas implements
        ColorMapCanvas, ScatterCanvas {

    public static void main(String[] args) {
//        makeColorMap();
//        vec3 v = vec3.rand(1000);
        S2Grid g = S2Grid.equispacedS2Grid(Math.toRadians(2.5), 0, Math.PI / 2, 0, 2 * Math.PI, false);

        Vec3 v = g.toVec3();
        Array1D d = Array1D.linspace(0, Math.PI, v.size());
//        vec3 v = vec3.concat(vec3.yvector());
//        JComponent plot = plot(v,);
        ScatterOptions scatterOptions = new ScatterOptions();
        scatterOptions.setMarker(Shapes.Named.CIRCLE);
        scatterOptions.setMarkerSize(2);
//        scatterOptions.setMarkerFaceColor(Color.white);

        long nanoTime = System.nanoTime();
//        scatterOptions.setMarkerEdgeColor(Color.darkGray);
//        JComponent plot = Plotter.plot(v, Array1D.linspace(0, 100, v.size()), scatterOptions);
        JComponent plot = Plotter.plot(v, d, scatterOptions);
        System.out.println("e1 : " + (System.nanoTime() - nanoTime) / 1e9);
        System.out.println(v.size());
        Plotter.show(plot);

    }

    private int sz = 760;

    Vec3 v;
    Array1D data;
    int[] datam;

    ColorMap cmp;

    double gx[];
    double gy[];

    double dmin, dmax;

    ScatterOptions options;

    public SphericalCanvasScatter(Vec3 v, Array1D data, ScatterOptions opts) {
        this(v, data, new SchmidtProjection(), new SphericalBounds(), new PlotAnnotation(data.max(), data.min()), opts);
    }

    public SphericalCanvasScatter(Vec3 v, Array1D data, String tr, String br) {
        this(v, data, new SchmidtProjection(), new SphericalBounds(Math.PI / 2, 2 * Math.PI), new PlotAnnotation(data.max(), data.min(), tr, br), new ScatterOptions());
    }

    SphericalCanvasScatter(Plotter.PlotGrid grid, Array1D f, Projector projector, PlotAnnotation annon) {
        this(grid.getR(), f, projector, grid.getBounds(), annon, new ScatterOptions());
    }

    public SphericalCanvasScatter(Vec3 v, Array1D data, Projector projector, SphericalBounds bounds, PlotAnnotation annon, ScatterOptions options) {
        this(v, data, projector, bounds, annon, options, Plotter.getDefaultColorMap());
    }

    public SphericalCanvasScatter(Vec3 v, Array1D data, Projector projector, SphericalBounds bounds, PlotAnnotation annon, ScatterOptions options, ColorMap cmap) {
        super(projector, bounds);

        this.v = v;
        this.data = data;
        this.options = options;

        setAnnotation(annon);
        setColorMap(cmap);

        init();
    }

    private void init() {
        Vec2 projection = getProjector().project(v);

        gx = projection.x();
        gy = projection.y();

        double xmin = getXmin();
        double ymin = getYmin();
        for (int i = 0; i < gx.length; i++) {
            gx[i] -= xmin;
            gy[i] -= ymin;
        }

        dmin = data.nanmin();
        dmax = data.nanmax();
        setCLim(dmin, dmax);
    }

    public void setScatterOption(ScatterOptions options) {
        this.options = options;
//        canvas = renderCanvas((int) (sz * getRatio()));
    }

    @Override
    public ScatterOptions getScatterOptions() {
        return this.options;
    }

    @Override
    public void setScatterOptions(ScatterOptions options) {
        this.options = options;
    }

    @Override
    protected BufferedImage renderCanvas(int sz) {

        if (sz < 50) {
            sz = 50;
        }
        double intend = 2D;
        double scale = Math.min((sz - 2 * intend) / (getDx()), (sz - 2 * intend) / (getDy()));

        double[] X = new double[gx.length];
        double[] Y = new double[gx.length];

        for (int i = 0; i < Y.length; i++) {
            X[i] = gx[i] * scale + intend;
            Y[i] = gy[i] * scale + intend;
        }

        int sh = (int) (sz / getRatio());

//        System.out.println(sz + " " + sh);
        BufferedImage canvas = new BufferedImage(sz, sh, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2d = (Graphics2D) canvas.getGraphics();
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        double sr = 2 * getDr() * scale;
        Ellipse2D.Double eli2 = new Ellipse2D.Double(intend - getXmin() * scale - sr / 2, intend - getYmin() * scale - sr / 2, sr, sr);
        if (getProjector() instanceof SphericalProjection) {
            g2d.clip(eli2);
        }

        Stroke lineStroke = options.getLineStroke();
        if (lineStroke != null) {
            Line2D.Double line = new Line2D.Double();
            g2d.setStroke(lineStroke);
            g2d.setColor(options.getLineColor());
            for (int i = 0; i < gx.length - 1; i++) {
                line.setLine(X[i], Y[i], X[i + 1], Y[i + 1]);
                g2d.draw(line);
            }
        }

        double ms = 5D * options.getMarkerSize() * Math.sqrt(getRatio());
        Shape shp = options.getMarker().create((float) ms);
        if (shp != null) {
            Color face = options.getMarkerFaceColor();
            Color edge = options.getMarkerEdgeColor();

            int nx = (int) (ms * 2.5D);
            BufferedImage markerImage = new BufferedImage(nx, nx, BufferedImage.TYPE_INT_ARGB);
            Graphics2D markerGraphics = (Graphics2D) markerImage.getGraphics();
            AffineTransform markerTransform = AffineTransform.getTranslateInstance(nx / 2, nx / 2);

            if (face != null) {
                markerGraphics.setColor(face);
            }
            markerGraphics.fill(markerTransform.createTransformedShape(shp));
            if (edge != null) {
                markerGraphics.setColor(edge);
            }
            markerGraphics.draw(markerTransform.createTransformedShape(shp));

            DataBuffer usedBuffer = markerImage.getRaster().getDataBuffer();
            int[] idx = new int[nx * nx];
            int count = 0;
            for (int j = 0; j < nx * nx; j++) {
                if (usedBuffer.getElem(j) != 0) {
                    idx[count++] = j;
                }
            }
            idx = Arrays.copyOf(idx, count);
            double shiftx = nx / 2;
            for (int i = 0; i < gx.length; i++) {
                if (datam != null) {
                    int rgb = cmp.getColor(datam[i]).getRGB();
                    for (int j : idx) {
                        usedBuffer.setElem(j, rgb);
                    }
                }
                g2d.drawImage(markerImage, AffineTransform.getTranslateInstance(X[i] - shiftx, Y[i] - shiftx), null);
            }
        }

        if (getProjector() instanceof SphericalProjection) {
            g2d.setColor(Color.black);
            g2d.setStroke(new BasicStroke(1f));
            g2d.setClip(null);
            g2d.draw(eli2);
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

        if (dmax - dmin == 0) {
            datam = null;
        } else {
            datam = data.minus(cmin).divided((cmax - cmin) / (cmp.size() - 1)).toIntArray();

            for (int i = 0; i < datam.length; i++) {
                if (datam[i] < 0) {
                    datam[i] = 0;
                } else if (datam[i] > cmp.size()) {
                    datam[i] = cmp.size();
                }
            }
        }
//        canvas = renderCanvas((int) (sz * getRatio()));
    }

    @Override
    public void setColorMap(ColorMap cmp) {
        this.cmp = cmp;
    }

}
