/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot.gui;

import com.jtex.arrays.Array1D;
import com.jtex.plot.Plotter;
import com.jtex.qta.ODF;
import com.jtex.qta.ODFDemo;
import com.jtex.qta.file.BrukerGpolLoader;
import com.jtex.qta.file.DubnaLoader;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTabbedPane;
import javax.swing.JTextPane;

/**
 *
 * @author hios
 */
public class SimpleMenu extends JMenuBar {

    SimpleApp data;
    JTabbedPane pane;
    JMenu pfmenu;
    JMenu odfmenu;
//    JMenu currentplotmenu;
//    SimpleMultiPlotComponent currentplot;

    public SimpleMenu(SimpleApp app, JTabbedPane pane) {
        super();

        this.pane = pane;
//        this.pane.addChangeListener(new ChangeListener() {
//
//            @Override
//            public void stateChanged(ChangeEvent e) {
//                Component selectedComponent = SimpleMenu.this.pane.getSelectedComponent();
//                if (selectedComponent instanceof SimpleMultiPlotComponent) {
//                    SimpleMenu.this.currentplot = (SimpleMultiPlotComponent) selectedComponent;
//                    updateActions();
//
//                } else {
//                    SimpleMenu.this.currentplot = null;
//                }
//            }
//        });
        this.data = app;
        this.data.addAppListener(new SimpleAppListener() {

            @Override
            public void appChanged(SimpleApp app) {
                updateActions();

            }

        });
        add(createDataMenu());
        pfmenu = createPFMenu();
        add(pfmenu);
        odfmenu = createODFMenu();
        add(odfmenu);
//        currentplotmenu = createCurrentPlotMenu();
//        add(currentplotmenu);
        updateActions();

    }

    private JMenu createDataMenu() {
        JMenu menu = new JMenu("Data");

        menu.add(new AbstractAction("Dubna Data") {

            @Override
            public void actionPerformed(ActionEvent e) {
                pane.removeAll();
                data.setPf(DubnaLoader.loadExample());
            }
        });

        menu.add(new AbstractAction("Bruker GPol Data") {

            @Override
            public void actionPerformed(ActionEvent e) {
                pane.removeAll();

                data.setPf(BrukerGpolLoader.loadExample());
            }
        });

        menu.add(new AbstractAction("SantaFe PF") {

            @Override
            public void actionPerformed(ActionEvent e) {
                pane.removeAll();
                data.setPf(ODFDemo.SantaFePF());
            }
        });

        return menu;
    }

    private JMenu createPFMenu() {
        JMenu menu = new JMenu("Pole Figure");

        menu.add(new AbstractAction("Plot") {

            @Override
            public void actionPerformed(ActionEvent e) {
                addTab("Pole Figures", new Callable<JComponent>() {
                    @Override public JComponent call() throws Exception {
                        return Plotter.plot(data.getPf());
                    }
                });
            }
        });
        menu.addSeparator();
        menu.add(new AbstractAction("Estimate ODF") {

            @Override
            public void actionPerformed(ActionEvent e) {

                SimpleODFOptionsDialog opt = new SimpleODFOptionsDialog(null, true);
                opt.setPoleFigure(data.getPf());
                opt.setVisible(true);

                if (!opt.wasCanceled()) {
	                com.jtex.qta.ODF estimate = new com.jtex.qta.ODF();
	                estimate = estimate.estimate(data.getPf(), opt.getODFPoptions());
	                data.setOdf(estimate);
                }

            }
        });

        return menu;
    }

    private void updateActions() {

        pfmenu.setVisible(data.getPf() != null);
        odfmenu.setVisible(data.getOdf() != null);
//        currentplotmenu.setVisible(currentplot != null);

    }

    private JMenu createODFMenu() {
        JMenu menu = new JMenu("ODF");

        menu.add(new AbstractAction("Plot pole density") {
            @Override public void actionPerformed(ActionEvent e) {
                addTab("Estimated pole density", new Callable<JComponent>() {
                    @Override public JComponent call() throws Exception {
                        return Plotter.plotpdf(data.getOdf(), data.getPf().getH());
                    }
                });
            }
        });

        menu.add(new AbstractAction("Plot residuals") {
            @Override
            public void actionPerformed(ActionEvent e) {
                addTab("Residual plot", new Callable<JComponent>() {
                    @Override public JComponent call() throws Exception {
                        return Plotter.plotDiff(data.getOdf(), data.getPf());
                    }
                });
            }
        });

        menu.addSeparator();

        menu.add(new AbstractAction("Plot phi1-Sections") {
            @Override
            public void actionPerformed(ActionEvent e) {
                addTab("phi1-sections", new Callable<JComponent>() {
                    @Override public JComponent call() throws Exception {
                        return Plotter.plotphi1(data.getOdf());
                    }
                });
            }
        });
        menu.add(new AbstractAction("Plot reduced phi2-Sections") {
            @Override
            public void actionPerformed(ActionEvent e) {
                addTab("phi2-sections", new Callable<JComponent>() {
                    @Override public JComponent call() throws Exception {
                        return Plotter.plotphi2(data.getOdf());
                    }
                });
            }
        });

        menu.add(new AbstractAction("Plot complete phi2-Sections") {
            @Override
            public void actionPerformed(ActionEvent e) {
                addTab("phi2-sections", new Callable<JComponent>() {
                    @Override public JComponent call() throws Exception {
                        return Plotter.plotphi2full(data.getOdf());
                    }
                });
            }
        });

        menu.add(new AbstractAction("Plot sigma-Sections") {
            @Override
            public void actionPerformed(ActionEvent e) {
                addTab("sigma-sections", new Callable<JComponent>() {
                    @Override public JComponent call() throws Exception {
                        return Plotter.plotsigma(data.getOdf());
                    }
                });
            }
        });

        menu.addSeparator();

        menu.add(new AbstractAction("Plot Power-Spectrum") {
            @Override
            public void actionPerformed(ActionEvent e) {
                addTab("Power plot", new Callable<JComponent>() {
                    @Override public JComponent call() throws Exception {
                        Array1D powerSpectrum = data.getOdf().powerSpectrum();

                        JTextPane text = new JTextPane();
                        text.setText(Array1D.print(powerSpectrum.toDoubleArray()));
                        return text;
                    }
                });
            }
        });

        menu.add(new AbstractAction("Texture Index") {
            @Override
            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(pane, "Texture index: " + data.getOdf().textureindex());
            }
        });

        menu.add(new AbstractAction("Entropy") {
            @Override
            public void actionPerformed(ActionEvent e) {
                JOptionPane.showMessageDialog(pane, "Entropy: " + data.getOdf().entropy());
            }
        });

        return menu;
    }

    ExecutorService executor = Executors.newCachedThreadPool();

    private class ThreadedPlot extends JComponent {

        JPanel prg;
        Callable<JComponent> cmpt;

        public ThreadedPlot(String message, Callable<JComponent> callable) {
            setLayout(new BorderLayout());

            this.cmpt = callable;
            add(prg = createProgressMessage(message), "Center");

            // slightly weird construction
            new Thread(new Runnable() {
                @Override public void run() {
                    try {
                        Future<JComponent> submit = executor.submit(cmpt);
                        JComponent call = submit.get();
                        prg.setVisible(false);
                        SimpleMenu.ThreadedPlot.this.remove(prg);
                        SimpleMenu.ThreadedPlot.this.add(call);
                    } catch (Exception ex) {
                        Logger.getLogger(SimpleMenu.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
            }).start();
        }

        private JPanel createProgressMessage(String message) {
            JPanel prog = new JPanel();
            prog.setOpaque(false);
            prog.setLayout(new BoxLayout(prog, BoxLayout.PAGE_AXIS));
            prog.add(Box.createVerticalGlue());
            JProgressBar progress = new JProgressBar(0, 10);
            progress.setAlignmentX(.5f);
            progress.setPreferredSize(new Dimension(200, 24));
            progress.setMaximumSize(new Dimension(200, 24));
            progress.setIndeterminate(true);
            prog.add(progress);
            JLabel lbl = new JLabel(message);
            lbl.setAlignmentX(.5f);
            prog.add(lbl);
            prog.add(Box.createVerticalGlue());
            return prog;
        }

    }

    private void addTab(String name, Callable<JComponent> runnable) {
        ThreadedPlot threadedPlot = new ThreadedPlot("Creating " + name, runnable);
        pane.add(name, threadedPlot);
        pane.setSelectedComponent(threadedPlot);
    }

    private JMenu createCurrentPlotMenu() {
        return new JMenu("Current Plot");
    }

}
