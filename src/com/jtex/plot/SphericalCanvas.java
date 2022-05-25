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
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.image.BufferedImage;
import javax.swing.JComponent;

/**
 *
 * @author hios
 */
public abstract class SphericalCanvas extends JComponent implements RatioComponent {

    private PlotAnnotation annon;
    BufferedImage canvas;

    private double xmin, ymin, dx, dy, dr, ratio;

    private Projector projector;
    private SphericalBounds bounds;

    public SphericalCanvas(Projector projector, SphericalBounds bounds) {
        this.projector = projector;
        this.bounds = bounds;

        if (projector == null) {
            this.projector = new SchmidtProjection();
        }

        if (bounds == null) {
            this.bounds = new SphericalBounds();
        }

        addComponentListener(new ComponentAdapter() {

            @Override
            public void componentResized(ComponentEvent e) {
                super.componentResized(e); //To change body of generated methods, choose Tools | Templates.
            }

        });

        setLayout(new BorderLayout());
        calcBounds();

    }

    public void setAnnotation(PlotAnnotation annon) {
        this.annon = annon;
    }

    public Projector getProjector() {
        return projector;
    }

    private void calcBounds() {

        Array1D th = Array1D.linspace(bounds.getThetamin(), bounds.getThetamax(), 36);
        Array1D rh = Array1D.linspace(bounds.getRhomin(), bounds.getRhomax(), 36);

        Vec3 bounded = Vec3.concat(new Vec3(th, Array1D.fill(th.size(), bounds.getRhomin())),
                new Vec3(th, Array1D.fill(th.size(), bounds.getRhomax())),
                new Vec3(Array1D.fill(rh.size(), bounds.getThetamin()), rh),
                new Vec3(Array1D.fill(rh.size(), bounds.getThetamax()), rh));

        Vec2 prjbounds = projector.project(bounded);

        Array1D x = prjbounds.getX();
        Array1D y = prjbounds.getY();

        xmin = x.nanmin();
        ymin = y.nanmin();
        dx = x.nanmax() - xmin;
        dy = y.nanmax() - ymin;

        dr = projector.project(new Vec3(bounds.getThetamax(), 0)).norm().get(0);

        ratio = dx / dy;

    }

    public double getYmin() {
        return ymin;
    }

    public double getXmin() {
        return xmin;
    }

    public double getDx() {
        return dx;
    }

    public double getDy() {
        return dy;
    }

    public double getDr() {
        return dr;
    }

    public SphericalBounds getSphericalBounds() {
        return bounds;
    }

    @Override
    public double getRatio() {
        return ratio;
    }

    protected abstract BufferedImage renderCanvas(int sz);

    @Override
    public void paintComponent(Graphics g) {
        int sz = (int) Math.min(getWidth(), getHeight() * ratio);
        Graphics2D g2d = (Graphics2D) g;

        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
//        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

        int h = (int) (sz / ratio - 1);
        canvas = renderCanvas((int) (sz * getRatio()));
//        if (canvas == null) {
//            canvas = renderCanvas((int) (500 * getRatio()));
//        }

//        int cs = Math.max(canvas.getWidth(), canvas.getHeight());
//        if (sz > 1000 && cs < sz) {
//            System.out.println("1000");
//            canvas = renderCanvas((int) (1000 * getRatio()));
//
//        } else if (sz > 500 && cs < sz) {
//            System.out.println("500");
//            canvas = renderCanvas((int) (500 * getRatio()));
//
//        } else if (sz > 200 && cs < sz) {
//            System.out.println("200");
//            canvas = renderCanvas((int) (200 * getRatio()));
//
//        }
        if (projector instanceof SphericalProjection) {
            g.drawImage(canvas, 1, h + 1, sz - 1, -h, null);
        } else {
            g.drawImage(canvas, 1, 0, sz - 1, h, null);
        }
        g2d.setColor(Color.BLACK);
        g2d.drawRect(0, 0, getWidth() - 1, getHeight() - 1);

        if (annon != null) {
            annon.paint(g, getWidth(), getHeight());
        }
    }

}
