/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import com.jtex.arrays.Array1D;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ContainerAdapter;
import java.awt.event.ContainerEvent;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Arrays;
import java.util.Locale;
import javax.swing.AbstractAction;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

/**
 *
 * @author hios
 */
public class SimpleMultiPlotComponent extends JComponent implements
        ScatterCanvas {

    private JComponent panel;
    private ColorBar colorbar;
    private double dmin, dmax;
    private ColorMap.Name cmapname = Plotter.getDefaultColorMap().getName();
    private int ncolors = 200;

    boolean equalCLim = true;

//    JSlider s;
    public SimpleMultiPlotComponent() {
        setLayout(new BorderLayout());
        panel = new JComponent() {
        };
        panel.setLayout(new MultiPlotLayout());
        colorbar = new ColorBar();

        add(panel, "Center");
        add(colorbar, "East");

        panel.addContainerListener(new ContainerAdapter() {
            @Override
            public void componentAdded(ContainerEvent e) {
                setComponentPopupMenu(contextMenu());
                updateCLims();
            }

            @Override
            public void componentRemoved(ContainerEvent e) {
                updateCLims();
            }
        });

//        setComponentPopupMenu(contextMenu());

    }

    private JPopupMenu contextMenu() {
        JPopupMenu menu = new JPopupMenu();

        JCheckBoxMenuItem cbar = new JCheckBoxMenuItem("Colorbar");
        cbar.setState(equalCLim);
        cbar.addActionListener(new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent e) {
                setColorRangeEqual(!equalCLim);
                SimpleMultiPlotComponent.this.setColorRangeEqual(equalCLim);
            }
        });
        menu.add(cbar);

        JMenu colors = new JMenu("Colormap");
        for (ColorMap.Name mapname : ColorMap.Name.values()) {
            JMenuItem add = colors.add(mapname.name());
            add.addActionListener(new ActionListener() {
                @Override public void actionPerformed(ActionEvent e) {
                    cmapname = ColorMap.Name.valueOf(e.getActionCommand());
                    setColorMap(ColorMap.getColormap(cmapname, ncolors));
                    updatePlot();
                }
            });
        }
        menu.add(colors);

        JMenu ncolorsn = new JMenu("Colormap Size");
        for (int d : Array1D.concat(Array1D.fill(5, 15, 1), Array1D.fill(20, 200, 20)).toIntArray()) {
            JMenuItem add = ncolorsn.add(String.valueOf(d));
            add.addActionListener(new ActionListener() {
                @Override public void actionPerformed(ActionEvent e) {
                    ncolors = Integer.parseInt(e.getActionCommand());
                    setColorMap(ColorMap.getColormap(cmapname, ncolors));
                    updatePlot();
                }
            });
        }
        menu.add(ncolorsn);

        Component[] components = panel.getComponents();
        if (components.length > 0) {
            if (components[0] instanceof SphericalCanvasScatter) {
                menu.addSeparator();

                JMenu marker = new JMenu("Marker");
                for (Shapes.Named val : Shapes.Named.values()) {
                    JMenuItem add = marker.add(val.name());
                    add.addActionListener(new ActionListener() {
                        @Override public void actionPerformed(ActionEvent e) {
                            Shapes.Named valueOf = Shapes.Named.valueOf(e.getActionCommand());
                            SimpleMultiPlotComponent.this.getScatterOptions().setMarker(valueOf);
                            updatePlot();
                        }
                    });
                }
                menu.add(marker);

                JMenu markersize = new JMenu("Marker Size");
                for (double d : new double[]{.5, 1, 2, 2.5, 3, 4, 5, 8, 10, 12, 16, 20}) {
                    JMenuItem add = markersize.add(String.valueOf(d));
                    add.addActionListener(new ActionListener() {
                        @Override public void actionPerformed(ActionEvent e) {
                            double parseDouble = Double.parseDouble(e.getActionCommand());
                            SimpleMultiPlotComponent.this.getScatterOptions().setMarkerSize(parseDouble / 4D);
                            updatePlot();
                        }
                    });

                }
                menu.add(markersize);
            }

        }
        return menu;
    }

    public Component[] getPanelComponents() {
        return panel.getComponents();
    }

    @Override
    public Component add(Component comp) {
        return panel.add(comp); //To change body of generated methods, choose Tools | Templates.
    }

    public void setColorRangeEqual(boolean eql) {
        if (this.equalCLim != eql) {
            this.equalCLim = eql;
            updateCLims();
            repaint();
        }
    }

    public void setColorMap(ColorMap map) {
        cmapname = map.getName();
        for (Component comp : panel.getComponents()) {
            if (comp instanceof ColorMapCanvas) {
                ((ColorMapCanvas) comp).setColorMap(map);
            }
        }
        colorbar.setColorMap(map);
        updateCLims();
    }

    public void setColorbarVisible(boolean vis) {

        colorbar.setVisible(vis);

    }

    private void updatePlot() {
        invalidate();
        repaint();
    }

    @Override
    public void doLayout() {
        super.doLayout(); //To change body of generated methods, choose Tools | Templates.
        panel.doLayout();
        colorbar.doLayout();
    }

    public void updateCLims() {
        Component[] components = panel.getComponents();

        colorbar.setVisible(equalCLim);

        if (equalCLim) {
            dmin = Double.MAX_VALUE;
            dmax = -Double.MAX_VALUE;
            for (Component comp : components) {
                if (comp instanceof ColorMapCanvas) {
                    dmin = Math.min(dmin, ((ColorMapCanvas) comp).getCLimMin());
                    dmax = Math.max(dmax, ((ColorMapCanvas) comp).getCLimMax());
                };
            }

            for (Component comp : components) {
                if (comp instanceof ColorMapCanvas) {
                    ((ColorMapCanvas) comp).setCLim(dmin, dmax);
                }
            }
        } else {
            for (Component comp : components) {
                if (comp instanceof ColorMapCanvas) {
                    ColorMapCanvas cnvs = ((ColorMapCanvas) comp);
                    cnvs.setCLim(cnvs.getCLimMin(), cnvs.getCLimMax());
                }
            }
        }
    }

    @Override
    public ScatterOptions getScatterOptions() {

        for (Component cmp : panel.getComponents()) {
            if (cmp instanceof ScatterCanvas) {
                return (((ScatterCanvas) cmp)).getScatterOptions();
            }
        }
        return null;
    }

    @Override
    public void setScatterOptions(ScatterOptions options) {
        for (Component cmp : panel.getComponents()) {
            if (cmp instanceof ScatterCanvas) {
                ((ScatterCanvas) cmp).setScatterOptions(options);

            }

        }
    }

    private class ColorBar extends JComponent {

        private ColorMap cmp;
        private BufferedImage colormap;
        private DecimalFormat f = new DecimalFormat(".##", DecimalFormatSymbols.getInstance(Locale.US));

        public ColorBar() {
            setPreferredSize(new Dimension(75, 50));
            setColorMap(Plotter.getDefaultColorMap());

        }

        private BufferedImage renderColorMap() {
            BufferedImage bf = new BufferedImage(1, cmp.size(), BufferedImage.TYPE_4BYTE_ABGR);
            WritableRaster raster = bf.getRaster();
            Color colors[] = cmp.colors();
            for (int i = 0; i < cmp.size(); i++) {
                Color cc = colors[cmp.size() - i - 1];
                raster.setSample(0, i, 0, cc.getRed());
                raster.setSample(0, i, 1, cc.getGreen());
                raster.setSample(0, i, 2, cc.getBlue());
                raster.setSample(0, i, 3, cc.getAlpha());
            }
            return bf;
        }

        public void setColorMap(ColorMap cmp) {
            this.cmp = cmp;
            colormap = renderColorMap();
        }

//        @Override
//        public void paint(Graphics g) {
//            super.paint(g); //To change body of generated methods, choose Tools | Templates.
//        }
//
//        
        @Override
        protected void paintComponent(Graphics g) {

            int w2 = SimpleMultiPlotComponent.this.getSize().width - panel.getPreferredSize().width;
            Graphics2D g2d = (Graphics2D) g;
            g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);

            int height = 0;
            int margin = Integer.MAX_VALUE;
            int width = 10;

            synchronized (getTreeLock()) {
                Component[] components = panel.getComponents();
                for (Component c : components) {
                    Rectangle bounds = c.getBounds();
                    height = Math.max(height, bounds.y + bounds.height);
                    margin = Math.min(margin, bounds.y);
                }
            }

            Rectangle2D stringBounds = g.getFont().getStringBounds("1", 0, 1, g.getFontMetrics().getFontRenderContext());
            int w = (int) stringBounds.getWidth();
            int h2 = (int) stringBounds.getHeight() / 2;

            int marginy = margin + h2;
            height -= 1.5 * marginy;

            g.drawImage(colormap, margin, marginy, width, height, null);

            g.setColor(Color.BLACK);
            g.drawRect(margin, marginy, width, height - 1);

            int sep = Math.abs(height / (8 * h2) + 1);

            for (int i = 0; i <= sep; i++) {
                int hh = (int) (marginy + i * (height - 1) / sep);
                g.drawString(f.format(dmin + (sep - i) * (dmax - dmin) / sep), margin + width + w, hh + h2);
                g.drawLine(margin, hh, margin + width + w - 1, hh);

            }

        }

    }

}
