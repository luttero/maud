/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;
import java.awt.LayoutManager2;
import java.awt.Rectangle;
import javax.swing.BoxLayout;

/**
 *
 * @author hios
 */
public class MultiPlotLayout implements LayoutManager {

    BoxLayout b;
    int ix = 0;
    int iy = 0;
    int l = 0;
    int marginx = 2;
    int marginy = 2;
    int outerx = 5;

    public MultiPlotLayout() {

    }

    @Override
    public void addLayoutComponent(String name, Component comp) {
    }

    @Override
    public void removeLayoutComponent(Component comp) {

    }

    @Override
    public Dimension preferredLayoutSize(Container target) {
        synchronized (target.getTreeLock()) {

            int w = -Integer.MAX_VALUE;
            int h = -Integer.MAX_VALUE;
            for (Component c : target.getComponents()) {
                w = Math.max(w, c.getLocation().x + c.getWidth());
                h = Math.max(h, c.getLocation().y + c.getHeight());
            }
//            Rectangle bounds = target.getBounds();
//            return new Dimension((int) bounds.getWidth(), (int) bounds.getHeight());
            return new Dimension(w, h);

        }
    }

    @Override
    public Dimension minimumLayoutSize(Container target) {

        synchronized (target.getTreeLock()) {
            return new Dimension(4 * outerx, 4 * outerx);
        }
    }

    @Override
    public void layoutContainer(Container target) {

        synchronized (target.getTreeLock()) {

            double dxdy = 1;

            for (Component c : target.getComponents()) {
                if (c instanceof RatioComponent) {
                    dxdy = Math.max(((RatioComponent) c).getRatio(), dxdy);
                }

            }
            dxdy = 1 / dxdy;

            int nmembers = target.getComponentCount();

            bestFit(target.getBounds(), dxdy, nmembers);
            for (int i = 0; i < nmembers; i++) {
                Component m = target.getComponent(i);
                int ldxdy = (int) ((int) l * dxdy);
                m.setSize(l, ldxdy);

                int vi = i % ix;
                int vj = (i - vi) / ix;
                m.setLocation(outerx + vi * (l + marginx), outerx + vj * (ldxdy + marginy));
            }
        }

    }

    private void bestFit(Rectangle bounds, double dxdy, int count) {

        double dx = bounds.getWidth() - 2 * outerx;
        double dy = bounds.getHeight() - 2 * outerx;

        int by = 1;
        int bx = count;
        int nx;

        double ll = lx(dx, dy, 1, bx, by);

        if (count > 1) {
            for (int ny = 1; ny <= count; ny++) {
                nx = (int) Math.ceil(((double) count) / ((double) ny));
                if (lx(dx, dy, dxdy, nx, ny) > ll) {
                    bx = nx;
                    by = ny;
                    ll = lx(dx, dy, dxdy, nx, ny);
                }
            }
        }

        this.ix = bx;
        this.iy = by;
        this.l = (int) Math.floor(ll);
    }

    private double lx(double dx, double dy, double dxdy, double nx, double ny) {
        return (Math.min((dx - (nx - 1) * marginx) / nx, (dy - (ny - 1) * marginy) / dxdy / ny));
    }

}
