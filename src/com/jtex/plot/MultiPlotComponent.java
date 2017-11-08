/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

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
import java.util.Locale;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.JSlider;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 *
 * @author hios
 */
public class MultiPlotComponent extends JComponent {

    private JComponent panel;
    private ColorBar colorbar;
    private ControlBar control;
    private double dmin, dmax;

    boolean equalCLim = false;

//    JSlider s;
    public MultiPlotComponent() {
        setLayout(new BorderLayout());
        panel = new JComponent() {
        };
        panel.setLayout(new MultiPlotLayout());
        colorbar = new ColorBar();
        control = new ControlBar();

//        s = new JSlider(10, 200);
//
//        s.addChangeListener(new ChangeListener() {
//
//            @Override
//            public void stateChanged(ChangeEvent e) {
//                updateCLims();
//            }
//        }
//        );
        add(control, "North");
        add(panel, "Center");
        add(colorbar, "East");

        panel.addContainerListener(new ContainerAdapter() {
            @Override
            public void componentAdded(ContainerEvent e) {
                updateCLims();
            }

            @Override
            public void componentRemoved(ContainerEvent e) {
                updateCLims();
            }
        });

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
        for (Component comp : panel.getComponents()) {
            if (comp instanceof ColorMapCanvas) {
                ((ColorMapCanvas) comp).setColorMap(map);
            }
        }
        colorbar.setColorMap(map);

        updateCLims();
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

    private class ControlBar extends JComponent {

        JComboBox jColorMap;
        JComboBox jContours;
        JToggleButton jBtnColorBar;

        Object[] val = new Object[]{5, 8, 10, 15, 20, 30, 200};

        public ControlBar() {
            setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS));

            JToolBar b = new JToolBar("Colormap");
            add(b);
            jColorMap = new JComboBox(ColorMap.Name.values());
//            jColorMap.setPreferredSize(new Dimension(280, 18));
            jContours = new JComboBox(val);
//            jContours.setPreferredSize(new Dimension(28, 18));
            jBtnColorBar = new JToggleButton("equal");
//            jBtnColorBar.setPreferredSize(new Dimension(22, 22));

            jBtnColorBar.addActionListener(
                    new ActionListener() {

                        public void actionPerformed(ActionEvent e) {
                            JToggleButton tBtn = (JToggleButton) e.getSource();
                            setColorRangeEqual(tBtn.isSelected());
                        }
                    });
            b.add(jBtnColorBar);
            b.add(jColorMap);
            b.add(jContours);

//            boolean makeScatterToolbar = false;
//            for (Component comp : panel.getComponents()) {
//                if (comp instanceof SphericalCanvasScatter) {
//                    makeScatterToolbar = true;
//                }
//            }
//
//            if (makeScatterToolbar) {
            JToolBar c = new JToolBar("Scatter Options");
            add(c);
//            if (panel instanceof SphericalCanvasScatter) {
            final JComboBox jSz = new JComboBox(new Object[]{1, 2, 5, 10});

            JPopupMenu popup = new JPopupMenu();

            final JSlider jSl = new JSlider(1, 20);
            popup.add(jSl);

//            DropDownButton drop = new DropDownButton("Size", popup);
//            c.add(drop);
//            jSl.addChangeListener(new ChangeListener() {
//
//                @Override
//                public void stateChanged(ChangeEvent e) {
//                    for (Component comp : panel.getComponents()) {
//                        if (comp instanceof SphericalCanvasScatter) {
//                            SphericalCanvasScatter cnvs = ((SphericalCanvasScatter) comp);
//                            ScatterOptions scatterOptions = cnvs.getScatterOptions();
//
//                            scatterOptions.setMarkerSize((Integer) jSl.getValue());
//                            cnvs.setScatterOption(scatterOptions);
//                        }
//                    }
//                    MultiPlotComponent.this.repaint();
//                }
//            });

            final JComboBox<Shapes.Named> marker = new JComboBox<Shapes.Named>(Shapes.Named.values());
            c.add(marker);

            marker.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    Shapes.Named selectedItem = marker.getItemAt(marker.getSelectedIndex());

                    for (Component comp : panel.getComponents()) {
                        if (comp instanceof SphericalCanvasScatter) {
                            SphericalCanvasScatter cnvs = ((SphericalCanvasScatter) comp);
                            ScatterOptions scatterOptions = cnvs.getScatterOptions();
                            scatterOptions.setMarker(selectedItem);
                            cnvs.setScatterOption(scatterOptions);
                        }

                    }
                    MultiPlotComponent.this.repaint();

                }
            });

            ActionListener listener = new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    ColorMap.Name selectedItem = (ColorMap.Name) ControlBar.this.jColorMap.getSelectedItem();
                    int ncolors = (Integer) ControlBar.this.jContours.getSelectedItem();
                    MultiPlotComponent.this.setColorMap(ColorMap.getColormap(selectedItem, ncolors));
                    MultiPlotComponent.this.repaint();
                }
            };

            jColorMap.addActionListener(listener);

            jContours.addActionListener(listener);
            add(Box.createHorizontalGlue());

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

            int w2 = MultiPlotComponent.this.getSize().width - panel.getPreferredSize().width;
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
