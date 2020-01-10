/*
 * @(#)TexturePlot.java created 18/09/1999 Pergine Valsugana
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.awt;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.rsa.TensorHomogenization;
import it.unitn.ing.rista.diffr.rta.*;
import it.unitn.ing.rista.interfaces.i3DCanvas;
import it.unitn.ing.rista.models.simplehklTableModel;
import it.unitn.ing.rista.render3d.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableModel;
import javax.media.opengl.*;
import javax.media.opengl.awt.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;
import java.util.StringTokenizer;
import java.io.BufferedReader;

/**
 * Display a TexturePlot window to manage pole figure and coverage plotting.
 *
 *
 *
 * @version $Revision: 1.17 $, $Date: 2006/12/04 14:30:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class TexturePlot extends myJFrame {

  JRadioButton[] plotTypeRB;
  JRadioButton[] plotWhatRB;
  JCheckBox grayShadedCB;
  JCheckBox logScaleCB;
  JTable hkltable = null;
  Phase thephase = null;
  Sample thesample = null;
  JComboBox phaseC = null;
  JComboBox sampleC = null;
  JTextField maxAngleTF = null;
  JTextField pointsTF = null;
  JTextField zoomTF = null;
  JTextField smoothTF = null;
  JSlider expansionJS = null;
  JTextField alphaStartTF, alphaEndTF, alphaStepTF;
  JTextField betaStartTF, betaEndTF, betaStepTF;
  JComboBox odf3DplotCB;

  FilePar parameterfile = null;

  public TexturePlot(Frame parentFrame) {
    super(parentFrame);

    framePositionX = "texturePlot.framePositionX";
    framePositionY = "texturePlot.framePositionY";
    defaultFramePositionX = 10;
    defaultFramePositionY = 20;
    setOwnPosition = true;

    JMenuBar amenuBar = createDefaultMenuBar();

    setTitle("Texture plotting");

    setHelpFilename("poleFiguresPlotting.txt");

    parameterfile = getFileParent();
    parameterfile.refreshAll(false);

    String[] labelRB = {"2D map", "3D surface"};
    plotTypeRB = new JRadioButton[labelRB.length];

    String[] labelWhatRB = {"Pole figure coverage", "Reconstructed intensity",
                            "Experimental intensity", "Inverse pole figures",
                            "Sample shape absorption", "Reconstructed strain",
                            "Experimental strain"};
    plotWhatRB = new JRadioButton[labelWhatRB.length];

    Container cp = getContentPane();
    JPanel principalPanel = new JPanel();
    principalPanel.setLayout(new BorderLayout());
    principalPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));
    cp.add(principalPanel);

    JPanel c2 = new JPanel();
    c2.setLayout(new FlowLayout());
    principalPanel.add(BorderLayout.CENTER, c2);
    JPanel c1 = new JPanel();
    c1.setLayout(new BorderLayout());
    c2.add(c1);

    JPanel comboPanel = new JPanel();
    comboPanel.setLayout(new GridLayout(2, 1, 3, 3));
    comboPanel.setBorder(new TitledBorder("Sample and Phase"));
    c1.add(comboPanel, BorderLayout.NORTH);
    JPanel jp1 = new JPanel();
    jp1.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    comboPanel.add(jp1);
    jp1.add(new JLabel("Sample: "));
    sampleC = new JComboBox();
    sampleC.setEditable(false);
    sampleC.setMaximumRowCount(4);
    jp1.add(sampleC);
    jp1 = new JPanel();
    jp1.setLayout(new FlowLayout(FlowLayout.LEFT, 1, 1));
    comboPanel.add(jp1);
    jp1.add(new JLabel("Phase:  "));
    phaseC = new JComboBox();
    phaseC.setEditable(false);
    phaseC.setMaximumRowCount(4);
    jp1.add(phaseC);

    JPanel optionsPanel = new JPanel();
    optionsPanel.setLayout(new BorderLayout(0, 0));
    optionsPanel.setBorder(new TitledBorder("Pole figure options"));
    c1.add(BorderLayout.CENTER, optionsPanel);

    JPanel mainOptionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 6));
    optionsPanel.add(BorderLayout.NORTH, mainOptionsPanel);

    JPanel specialPanel = new JPanel();
    specialPanel.setLayout(new GridLayout(0, 1, 3, 3));
    mainOptionsPanel.add(specialPanel);

    ButtonGroup rbg = new ButtonGroup();

    for (int i = 0; i < labelWhatRB.length; i++) {
      plotWhatRB[i] = new JRadioButton(labelWhatRB[i]);
      specialPanel.add(plotWhatRB[i]);
      rbg.add(plotWhatRB[i]);
    }

    specialPanel = new JPanel();
    specialPanel.setLayout(new GridLayout(0, 1, 3, 3));
    mainOptionsPanel.add(specialPanel);

    rbg = new ButtonGroup();

    for (int i = 0; i < labelRB.length; i++) {
      plotTypeRB[i] = new JRadioButton(labelRB[i]);
      specialPanel.add(plotTypeRB[i]);
      rbg.add(plotTypeRB[i]);
    }

    logScaleCB = new JCheckBox("Log scale");
    specialPanel.add(logScaleCB);
    grayShadedCB = new JCheckBox("Gray shaded");
    specialPanel.add(grayShadedCB);

    specialPanel = new JPanel();
    specialPanel.setLayout(new GridLayout(0, 1, 3, 3));
    optionsPanel.add(BorderLayout.CENTER, specialPanel);

    JPanel jp2 = new JPanel();
    specialPanel.add(jp2);
    jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    jp2.add(new JLabel("Max azimuthal angle: "));
    maxAngleTF = new JTextField(6);
    jp2.add(maxAngleTF);
    jp2 = new JPanel();
    specialPanel.add(jp2);
    jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    jp2.add(new JLabel("Number grid points: "));
    pointsTF = new JTextField(6);
    pointsTF.setToolTipText("Number of grid points used for calculation (more = better, but slower)");
    jp2.add(pointsTF);
    jp2 = new JPanel();
    specialPanel.add(jp2);
    jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    jp2.add(new JLabel("Zoom factor: "));
    zoomTF = new JTextField(6);
    zoomTF.setToolTipText("Zoom factor for plotting (1 = no zoom = 200 pixels)");
    jp2.add(zoomTF);
    jp2 = new JPanel();
    specialPanel.add(jp2);
    jp2.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    jp2.add(new JLabel("Gauss smooth width: "));
    smoothTF = new JTextField(6);
    smoothTF.setToolTipText("Smooth factor for Gauss filtering (0 = no smooth; in pixels; 1.2 as a good value)");
    jp2.add(smoothTF);

    specialPanel = new JPanel();
    specialPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
    optionsPanel.add(BorderLayout.SOUTH, specialPanel);

    specialPanel.add(new JLabel("Number of colors: "));
    expansionJS = new JSlider();
    expansionJS.setToolTipText("Change the number of colors for the 2D plot");
    specialPanel.add(expansionJS);

    JPanel p2 = new JPanel();
    p2.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    c1.add(BorderLayout.SOUTH, p2);

    setHelpButton(p2);

    JButton wizardButton = new JIconButton("Bulb.gif", "Optimize coverage");
    wizardButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        wizard_action();
      }
    });
    p2.add(wizardButton);

    JButton plotButton = new JIconButton("LineGraph.gif", "Plot");
    plotButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        (new PersistentThread() {
        public void executeJob() {
            plot_action();
          }
        }).start();
      }
    });
    p2.add(plotButton);

    JButton closeButton = new JIconButton("Exit.gif", "Close");
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        setVisible(false);
        dispose();
      }
    });
    p2.add(closeButton);

    JPanel polePanel = new JPanel();
    polePanel.setLayout(new BorderLayout());
    polePanel.setBorder(new TitledBorder("Pole figure"));
    principalPanel.add(BorderLayout.WEST, polePanel);

    TableModel hklModel = new simplehklTableModel(thephase);
    hkltable = new JTable(hklModel);
    hkltable.setPreferredScrollableViewportSize(new Dimension(200, 200));
    JScrollPane scrollpane = new JScrollPane(hkltable);
//		scrollpane.setBorder(new LineBorder(Color.black));
    polePanel.add(scrollpane, BorderLayout.CENTER);

    JPanel odfPanel = new JPanel(new BorderLayout());
    odfPanel.setBorder(new TitledBorder("ODF map plot"));
    polePanel.add(odfPanel, BorderLayout.SOUTH);
    JPanel controlODF = new JPanel(new GridLayout(3,4,3,3));
    controlODF.setBorder(new TitledBorder("plot limits"));
    odfPanel.add(controlODF, BorderLayout.CENTER);

    controlODF.add(new JLabel("ODF angles"));
    controlODF.add(new JLabel("Start (deg)"));
    controlODF.add(new JLabel("End (deg)"));
    controlODF.add(new JLabel("Step (deg)"));
    controlODF.add(new JLabel("Alpha/Gamma"));

    alphaStartTF = new JTextField(8);
    controlODF.add(alphaStartTF);
    alphaStartTF.setText(minAlphaAngle);
    alphaEndTF = new JTextField(8);
    controlODF.add(alphaEndTF);
    alphaEndTF.setText(maxAlphaAngle);
    alphaStepTF = new JTextField(8);
    controlODF.add(alphaStepTF);
    alphaStepTF.setText(stepAlphaAngle);

    controlODF.add(new JLabel("Beta"));
    betaStartTF = new JTextField(8);
    controlODF.add(betaStartTF);
    betaStartTF.setText(minBetaAngle);
    betaEndTF = new JTextField(8);
    controlODF.add(betaEndTF);
    betaEndTF.setText(maxBetaAngle);
    betaStepTF = new JTextField(8);
    controlODF.add(betaStepTF);
    betaStepTF.setText(stepBetaAngle);

    JPanel launchPlot = new JPanel(new FlowLayout());
    JButton launchB = new JIconButton("LineGraph.gif", "Plot ODF");
    launchB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        (new PersistentThread() {
        public void executeJob() {
            plot_ODF_action();
          }
        }).start();
      }
    });
    launchPlot.add(launchB);

    odf3DplotCB = new JComboBox();
    for (int i = 0; i < Texture.odfPlotMode.length; i++)
      odf3DplotCB.addItem(Texture.odfPlotMode[i]);
    launchPlot.add(odf3DplotCB);
    odfPanel.add(launchPlot, BorderLayout.SOUTH);

    amenuBar.add(createPFPlotMenu());

    initParameters();
    initListener();

    pack();
  }

  public JMenu createPFPlotMenu() {

    // to plot pole figure loaded from a file (Beartex format)

    JMenuItem menuitem = null;

    JMenu optionsMenu = new JMenu("Options");
    optionsMenu.setMnemonic('c');
    optionsMenu.add(menuitem = new JMenuItem("Homogenize tensor properties"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        TensorHomogenization work = new TensorHomogenization(thephase);
        TensorHomogenization.TensorHomogenizationFrame frame = work.getFrame();
        frame.initmyFrame();
        frame.ODFlabel.setText("ODF from phase: " + thephase.getLabel());
        frame.setVisible(true);

      }
    });
    optionsMenu.add(menuitem = new JMenuItem("Load Pole Figures file....."));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadingPFs();
      }
    });
    optionsMenu.add(menuitem = new JMenuItem("Plot radial distribution....."));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        radialPF();
      }
    });


    return optionsMenu;
  }

  public void loadingPFs() {
    lastResolution = Math.abs(Integer.valueOf(pointsTF.getText()).intValue());
    MaudPreferences.setPref(gridResString, Integer.toString(lastResolution));
    zoom = Math.abs(Double.parseDouble(zoomTF.getText()));
    filterWidth = Math.abs(Double.valueOf(smoothTF.getText()).doubleValue());
    MaudPreferences.setPref(zoomString, Double.toString(zoom));
    MaudPreferences.setPref("texturePlot.gaussFilterWidth", Double.toString(filterWidth));
    String maxAngleS = maxAngleTF.getText();
    double maxAngle = Double.valueOf(maxAngleS).doubleValue();
    maxAngle = Constants.sqrt2 * Math.sin(maxAngle * Constants.DEGTOPI / 2.0);
    MaudPreferences.setPref(maxAngleString, maxAngleS);
    boolean logScale = logScaleCB.isSelected();
    String logValue = "false";
    if (logScale)
      logValue = "true";
    MaudPreferences.setPref(logTexturePlotString, logValue);

    int colrsNumber = expansionJS.getValue();
    if (colrsNumber == 0) {
      colrsNumber = 8;
      expansionJS.setValue(colrsNumber);
    }
    MaudPreferences.setPref(numberofColors, colrsNumber);


    String filename = Utility.openFileDialog(this, "Open PF file (Beartex, apf, xrdml or CIF format)", FileDialog.LOAD,
        MaudPreferences.getPref(FilePar.analysisPath, Constants.documentsDirectory),
        null, MaudPreferences.getPref(FilePar.analysisPath, Constants.documentsDirectory));
    if (filename != null) {
      Vector expPF = poleFigureInput(filename, thesample);
      int numberPoleFiguresPF = (expPF.size() - 1) / 2;
      if (numberPoleFiguresPF > 0) {
        int[][] hklPF = new int[numberPoleFiguresPF][3];

        for (int i = 0; i < numberPoleFiguresPF; i++) {
          int[] hkl = (int[]) expPF.elementAt(i * 2);
          for (int j = 0; j < 3; j++) {
            hklPF[i][j] = hkl[j];
          }
        }

        double[][][] trialPole = new double[numberPoleFiguresPF][lastResolution][lastResolution];

        double max = 0.0;
        double min = 10000.0;
        for (int i = 0; i < numberPoleFiguresPF; i++) {
          double[][] pfInt = (double[][]) expPF.elementAt(i * 2 + 1);
          double[][] poleFigures = PlotPoleFigure.getExpPoleFigureGrid(pfInt,
              lastResolution, maxAngle);
          for (int k = 0; k < lastResolution; k++)
            for (int j = 0; j < lastResolution; j++) {
              trialPole[i][k][j] = poleFigures[k][j];
              if (poleFigures[k][j] > max)
                max = poleFigures[k][j];
              if (poleFigures[k][j] < min)
                min = poleFigures[k][j];
            }
        }
        if (min < 0.0)
          min = 0.0;

        new BeartexPFPlot(trialPole, hklPF, numberPoleFiguresPF, lastResolution, 0, lastResolution,
                zoom, logScale, maxAngle, min, max, filterWidth,
                grayShadedCB.isSelected(), colrsNumber, "Pole figures", false);
      }
    }
  }

  public static Vector poleFigureInput(String filename, Sample thesample) {
    int[] numberOfPFPoint;
    double[][][] textureAngles;
//    double[] expInt = null;
    int[] hkli = null;
    double[] weight = null;
    Vector expPF = null;
    String token = null;
    int maxizoveri_local = 1;

    double[] sampleAngles;
	  if (thesample != null)
		  sampleAngles = thesample.getSampleAngles();
	  else
		  sampleAngles = new double[3];

	  if (filename.toLowerCase().endsWith(".xrdml")) {
      XRDMLPoleFigureReader xreader = new XRDMLPoleFigureReader();
      xreader.readFile(Misc.getDataBufferedInputStream(filename));
      Vector polesAndAngles = xreader.getPoleFigure();

      int numberPoleFiguresPF = polesAndAngles.size() / 2;
      numberOfPFPoint = new int[numberPoleFiguresPF];
      int totmax = 0;
      for (int i = 0; i < numberPoleFiguresPF; i++) {
        double[][] pf = (double[][]) polesAndAngles.elementAt(i * 2 + 1);
        numberOfPFPoint[i] = pf[0].length;
        if (numberOfPFPoint[i] > totmax)
          totmax = numberOfPFPoint[i];
      }
      expPF = new Vector(0, 1);
      for (int i = 0; i < numberPoleFiguresPF; i++) {
        hkli = (int[]) polesAndAngles.elementAt(i * 2);
        double[][] pfIntAndAngles = new double[3][numberOfPFPoint[i]];
        double[][] pf = (double[][]) polesAndAngles.elementAt(i * 2 + 1);
        for (int j = 0; j < numberOfPFPoint[i]; j++) {
          pfIntAndAngles[0][j] = pf[0][j] + sampleAngles[1];
          pfIntAndAngles[1][j] = pf[1][j] + sampleAngles[2];
          pfIntAndAngles[2][j] = pf[2][j];
        }
        expPF.addElement(hkli);
        expPF.addElement(pfIntAndAngles);
      }
      int[] izoveriv = new int[1];
      izoveriv[0] = maxizoveri_local;
      expPF.addElement(izoveriv);
    } else {
      BufferedReader PFreader = Misc.getReader(filename);
      if (PFreader != null) {
        try {

          int izoveriCheck = 0;
          int izoveri = 1;
          hkli = new int[3];
          weight = new double[1];
          weight[0] = 1.0;
	        double dummy;

          String line = PFreader.readLine();
          StringTokenizer st = null;
          if (filename.toLowerCase().endsWith(".apf")) {
            // texture weights Maud export format

            line = PFreader.readLine();
            st = new StringTokenizer(line, "' ,\t\r\n");
            int totmax = 0;
            int numberPoleFiguresPF = Integer.valueOf(st.nextToken()).intValue();
            numberOfPFPoint = new int[numberPoleFiguresPF];
            expPF = new Vector(0, 1);
	          for (int i = 0; i < numberPoleFiguresPF; i++) {
              line = PFreader.readLine();
              st = new StringTokenizer(line, "' ,\t\r\n");
		          int[] hkl = new int[3];
              hkl[0] = Integer.valueOf(st.nextToken()).intValue();
              hkl[1] = Integer.valueOf(st.nextToken()).intValue();
              hkl[2] = Integer.valueOf(st.nextToken()).intValue();
              line = PFreader.readLine();
              st = new StringTokenizer(line, "' ,\t\r\n");
              numberOfPFPoint[i] = Integer.valueOf(st.nextToken()).intValue();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
              double[][] poleFig = new double[3][numberOfPFPoint[i]];
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                line = PFreader.readLine();
                st = new StringTokenizer(line, "' ,\t\r\n");
                double[] pf = new double[4];
	              poleFig[0][j] = Double.valueOf(st.nextToken()).doubleValue() + sampleAngles[1];
	              poleFig[1][j] = Double.valueOf(st.nextToken()).doubleValue() + sampleAngles[2];
	              poleFig[2][j] = Double.valueOf(st.nextToken()).doubleValue();
                if (st.hasMoreTokens()) st.nextToken();
                if (st.hasMoreTokens()) st.nextToken();
                if (st.hasMoreTokens())
                  dummy = Double.valueOf(st.nextToken()).doubleValue();
              }
              expPF.add(hkl);
              expPF.add(poleFig);
            }
            maxizoveri_local = 1;
          } else if (line.toLowerCase().startsWith("data_") || line.startsWith("_")) {
            // cif format
            Vector allPFs = new Vector(0, 1);
            while (line != null) {
              int[] hkl = new int[3];
              while (line != null && !line.toLowerCase().startsWith("loop_")) {
//          	System.out.println(linedata);
                st = new StringTokenizer(line, "' ,\t\r\n");

                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  if (token.equalsIgnoreCase("_diffrn_refln_index_h")) {
                    token = st.nextToken();
                    hkl[0] = Integer.valueOf(token).intValue();
                  } else if (token.equalsIgnoreCase("_diffrn_refln_index_k")) {
                    token = st.nextToken();
                    hkl[1] = Integer.valueOf(token).intValue();
                  } else if (token.equalsIgnoreCase("_diffrn_refln_index_l")) {
                    token = st.nextToken();
                    hkl[2] = Integer.valueOf(token).intValue();
                  }
                }
                line = PFreader.readLine();
              }

//						System.out.println("Reading pole figure: " + hkl[0] + " " + hkl[1] + " " + hkl[2]);

              int index = -1;
              int chiIndex = -1, phiIndex = -1, intensityIndex = -1, sigmaIndex = -1;

              line = PFreader.readLine();
              while (line != null && line.toLowerCase().startsWith("_")) {
//          	System.out.println(line);
                st = new StringTokenizer(line, "' ,\t\r\n");
                while (st.hasMoreTokens()) {
                  index++;
                  token = st.nextToken();
                  if (token.equalsIgnoreCase("_diffrn_refln_angle_chi") ||
                      token.equalsIgnoreCase("_pd_meas_angle_chi")) {
                    chiIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_angle_phi") ||
                      token.equalsIgnoreCase("_pd_meas_angle_phi")) {
                    phiIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_intensity_net") ||
                      token.equalsIgnoreCase("_pd_meas_intensity_total")) {
                    intensityIndex = index;
                  } else if (token.equalsIgnoreCase("_diffrn_refln_intensity_sigma")) {
                    sigmaIndex = index;
                  }
                }
                line = PFreader.readLine();
              }

//						System.out.println("Reading data for pole figure: " + chiIndex + " " + phiIndex + " " + intensityIndex);

              Vector poleFig = new Vector(10, 10);
              while (line != null && !line.toLowerCase().startsWith("_")
                  && !line.toLowerCase().startsWith("data_")) {
//          	System.out.println(line);
                st = new StringTokenizer(line, "' ,\t\r\n");

                index = 0;
                boolean found = false;
                double[] pf = new double[3];
                while (st.hasMoreTokens()) {
                  token = st.nextToken();
                  if (index == chiIndex) {
                    pf[0] = Double.valueOf(token).doubleValue();
                  } else if (index == phiIndex) {
                    pf[1] = Double.valueOf(token).doubleValue();
                  } else if (index == intensityIndex) {
                    pf[2] = Double.valueOf(token).doubleValue();
                    found = true;
                  } else if (index == sigmaIndex) {
//                    pf[3] = Double.valueOf(token).doubleValue();
                  }
                  index++;
                }
                if (found) {
                  poleFig.add(pf);
//						System.out.println("Adding to pole figure: " + pf[0] + " " + pf[1] + " " + pf[2]);
                }
                line = PFreader.readLine();
              }
              allPFs.add(hkl);
              allPFs.add(poleFig);
            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 2;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 2 + 1);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              hkli = new int[3];
              int[] hkl = (int[]) allPFs.elementAt(i * 2);
              for (int i1 = 0; i1 < 3; i1++)
                hkli[i1] = hkl[i1];
              double[][] pfInt = new double[3][numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 2 + 1);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                pfInt[0][j] = pf[0] + sampleAngles[1];
                pfInt[1][j] = pf[1] + sampleAngles[2];
                pfInt[2][j] = pf[2];
              }
              expPF.addElement(hkli);
              expPF.addElement(pfInt);
            }
            maxizoveri_local = 1;
          } else if (filename.toLowerCase().endsWith(".gpf")) {
            Vector allPFs = new Vector(3, 3);
            line = PFreader.readLine();
            while (line != null) {

              Vector poleFig = new Vector(10, 10);

              System.out.println("Reading: " + line);
              st = new StringTokenizer(line, " ,\t\r\n");
              hkli = new int[3];
              int index = 1;
              for (int i = 0; i < 3; i++) {
//              token = st.nextToken();
//              hkli[i][0] = Integer.valueOf(token).intValue();
                hkli[i] = Integer.valueOf(line.substring(index, index + 1)).intValue();
                index++;
//              System.out.println(hkli[i][0]);
              }
              double[] thetaAndRes = new double[3];
              thetaAndRes[0] = 0.0;
              thetaAndRes[1] = 90.0;
              thetaAndRes[2] = 5.0;

              allPFs.addElement(hkli);
//            double weightTot = 0.0;
              izoveri = 1;
              weight = new double[izoveri];
              for (int i = 0; i < izoveri; i++)
                weight[i] = 1.0;
              allPFs.addElement(weight);
//            expPF.addElement(thetaAndRes);

              double res = thetaAndRes[2];

              int alphamax = Uwimvuo.getAlphamax(res);
              int betamax = (Uwimvuo.getBetamax(res) - 1) / 2 + 1;
              int totData = 0;
              int a = 0;
              int b = 0;
              do {
                line = PFreader.readLine();
                for (int i = 1; i < 72; i += 4) {
                  if (totData < (alphamax - 1) * betamax) {
                    String subData = line.substring(i, i + 4);
                    totData++;
//                System.out.println(i + " " + subData);
                    double pfvalue = Double.valueOf(Misc.toStringDeleteBlank(subData)).doubleValue() / 100.0;
                    double theta = b * res; //(totData / (alphamax - 1)) * res;
                    double phi = a * res;
                    if (theta >= thetaAndRes[0] && theta <= thetaAndRes[1]) {
                      double[] pf = new double[3];
                      pf[0] = theta;
                      pf[1] = phi;
                      pf[2] = pfvalue;
//                    System.out.println(theta + " " + phi + " " + pfvalue);
                      poleFig.add(pf);
                    }
                    a++;
                    if (a >= alphamax - 1) {
                      a = 0;
                      b++;
                    }
                  }
                }
              } while (totData < (alphamax - 1) * betamax);
              allPFs.add(poleFig);
              line = PFreader.readLine();
              line = PFreader.readLine();

// next pole figure
              line = PFreader.readLine();
            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 3;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              int[][] hkl = (int[][]) allPFs.elementAt(i * 3);
              double[][] pfInt = new double[3][numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                pfInt[0][j] = pf[0] + sampleAngles[1];
                pfInt[1][j] = pf[1] + sampleAngles[2];
                pfInt[2][j] = pf[2];
              }
              expPF.addElement(hkl);
              expPF.addElement(pfInt);
            }
          } else if (filename.toLowerCase().endsWith(".ppf")) {
 //           Vector allPFs = new Vector(3, 3);

            for (int i = 0; i < 4; i++)
              line = PFreader.readLine();
            st = new StringTokenizer(line, " ,\t\r\n");
            int poleFiguresNumber = Integer.valueOf(st.nextToken());
            line = PFreader.readLine();

            int[][] indices = new int[3][poleFiguresNumber];
            double[][] grid = new double[6][poleFiguresNumber];
            double[] cutoff = new double[poleFiguresNumber];
            for (int i = 0; i < poleFiguresNumber; i++) {
              line = PFreader.readLine();
              st = new StringTokenizer(line, " ,\t\r\n");
              st.nextToken();
              grid[0][i] = Double.valueOf(st.nextToken());
              grid[1][i] = Double.valueOf(st.nextToken());
              grid[2][i] = Double.valueOf(st.nextToken());
              grid[3][i] = Double.valueOf(st.nextToken());
              grid[4][i] = Double.valueOf(st.nextToken());
              grid[5][i] = Double.valueOf(st.nextToken());
              st.nextToken();
              indices[0][i] = Integer.valueOf(st.nextToken());
              indices[1][i] = Integer.valueOf(st.nextToken());
              indices[2][i] = Integer.valueOf(st.nextToken());
              st.nextToken();
              st.nextToken();
              cutoff[i] = grid[2][i];
              if (st.hasMoreTokens())
                cutoff[i] = Double.valueOf(st.nextToken());
            }

            expPF = new Vector(0, 1);

            int totmax = 0;
            numberOfPFPoint = new int[poleFiguresNumber];
            for (int i = 0; i < poleFiguresNumber; i++) {
              numberOfPFPoint[i] = (int) ((grid[1][i] - grid[0][i]) / grid[2][i] + 1.01);
              numberOfPFPoint[i] *= (int) (360 / grid[2][i] + 0.00001);
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            textureAngles = new double[2][poleFiguresNumber][totmax];

            for (int i = 0; i < poleFiguresNumber; i++) {
              int[][] hkl = new int[3][1];
              for (int j = 0; j < 3; j++)
                hkl[j][0] = indices[j][i];

              double[] weig = new double[1];
              weig[0] = 1.0;

              double[][] pfInt = new double[3][numberOfPFPoint[i]];

              System.out.println("Pole figure: " + indices[0][i] + " " + indices[1][i] + " " + indices[2][i]
                                  + ", number of points to read: " + numberOfPFPoint[i]);

              int j = 0;
              line = PFreader.readLine();
              double alpha = grid[3][i];
              double beta = grid[0][i];
              while (j < numberOfPFPoint[i] && line != null) {
                st = new StringTokenizer(line, " ,\t\r\n");
                while (st.hasMoreTokens()) {
                  pfInt[0][j] = beta;
                  pfInt[1][j] = alpha;
                  pfInt[2][j] = Double.valueOf(st.nextToken());
                  if (beta > cutoff[i])
                    pfInt[2][j] = -999;
                  alpha += grid[5][i];
                  if (alpha > grid[4][i]) {
//                    System.out.println(beta + " " + alpha + " " + pfInt[2][j]);
                    alpha = grid[3][i];
                    beta += grid[2][i];
                  }
                  j++;
                }
                line = PFreader.readLine();
              }
              if (cutoff[i] > 0) {
                expPF.addElement(hkl);
                expPF.addElement(pfInt);
              }
            }

          } else {
            Vector allPFs = new Vector(3, 3);
            for (int i = 0; i < 4; i++) {
              if (line != null) {
                line = PFreader.readLine();
                if (line != null && !line.equals("")) {
                  st = new StringTokenizer(line, " ,\t\r\n");
                  while (st.hasMoreTokens()) {
                    switch (izoveriCheck) {
                      case 0:
                        izoveri = Integer.valueOf(token = st.nextToken()).intValue();
                        if (maxizoveri_local < izoveri)
                          maxizoveri_local = izoveri;
                        hkli = new int[3];
                        weight = new double[izoveri];
                        izoveriCheck++;
                        break;
                      default:
                        {
                          hkli[0] = Integer.valueOf(token = st.nextToken()).intValue();
                          hkli[1] = Integer.valueOf(token = st.nextToken()).intValue();
                          hkli[2] = Integer.valueOf(token = st.nextToken()).intValue();
                          weight[izoveriCheck - 1] = Double.valueOf(token = st.nextToken()).doubleValue();
                          izoveriCheck++;
                        }
                    }
                  }
                }
              }
            }

            while (line != null) {

              Vector poleFig = new Vector(10, 10);

              PFreader.readLine();
              line = PFreader.readLine();
//              System.out.println("Reading: " + line);
              int repeat = 1;
              int number = 11;
              int[] digits = {4,3,3,5,5,5,5,5,5,2,2};

              String[] data = Misc.readFormattedLine(line, digits, number, repeat);
              st = new StringTokenizer(line, " ,\t\r\n");
              for (int i = 0; i < 3; i++) {
                hkli[i] = Integer.valueOf(Misc.toStringDeleteBlankAndTab(data[i])).intValue();
//              System.out.println(hkli[i][0]);
              }
              double[] thetaAndRes = new double[3];
              double[] phiAndRes = new double[3];
              for (int i = 0; i < 3; i++) {
                thetaAndRes[i] = Double.valueOf(Misc.toStringDeleteBlankAndTab(data[i+3])).doubleValue();
                phiAndRes[i] = Double.valueOf(Misc.toStringDeleteBlankAndTab(data[i+6])).doubleValue();
              }

              allPFs.addElement(hkli);
              double weightTot = 0.0;
              for (int i = 0; i < izoveri; i++)
                weightTot += weight[i];
              if (weightTot == 0) {
                for (int i = 0; i < izoveri; i++)
                  weight[i] = 1.0 / izoveri;
              } else
                for (int i = 0; i < izoveri; i++)
                  weight[i] /= weightTot;
              allPFs.addElement(weight);
//            expPF.addElement(thetaAndRes);

              double res = thetaAndRes[2];

              int alphamax = Uwimvuo.getAlphamax(res);
              int betamax = (Uwimvuo.getBetamax(res) - 1) / 2 + 1;
              int totData = 0;
              int a = 0;
              int b = 0;
              double firstPFvalue = 0.0;
              do {
                line = PFreader.readLine();
                for (int i = 1; i < 72; i += 4) {
                  if (totData < alphamax * betamax) {
                    String subData = line.substring(i, i + 4);
                    totData++;
//                System.out.println(i + " " + subData);
                    double pfvalue = Double.valueOf(Misc.toStringDeleteBlank(subData)).doubleValue() / 100.0;
                    double theta = b * res; //(totData / (alphamax - 1)) * res;
                    double phi = a * res;
                    if (a == 0)
                      firstPFvalue = pfvalue;
                    if (theta >= thetaAndRes[0] && theta <= thetaAndRes[1] &&
                        phi >= phiAndRes[0] && phi <= phiAndRes[1] && pfvalue >= 0) {
                      double[] pf = new double[3];
                      pf[0] = theta;
                      pf[1] = phi;
                      pf[2] = pfvalue;
//                    System.out.println(theta + " " + phi + " " + pfvalue);
                      poleFig.add(pf);
                    }
                    a++;
                    if (a == alphamax - 1) {
//                  System.out.println(totData + " " + alphamax + " " + Double.toXRDcatString(firstPFvalue));
                      totData++;
                      theta = b * res; //(totData / (alphamax - 1)) * res;
                      phi = a * res;
                      if (theta >= thetaAndRes[0] && theta <= thetaAndRes[1] && firstPFvalue >= 0) {
                        double[] pf = new double[3];
                        pf[0] = theta;
                        pf[1] = phi;
                        pf[2] = firstPFvalue;
//                    System.out.println(theta + " " + phi + " " + pfvalue);
                        poleFig.add(pf);
                      }
                      a++;
                    }
                    if (a >= alphamax) {
                      a = 0;
                      b++;
                    }
                  }
                }

              } while (totData < alphamax * betamax);
              allPFs.add(poleFig);
              line = PFreader.readLine();

// next pole figure
              line = PFreader.readLine();

              izoveriCheck = 0;
              izoveri = 1;
              hkli = new int[3];
              weight = new double[1];
              weight[0] = 1.0;
              for (int i = 0; i < 4; i++) {
                if (line != null) {
                  line = PFreader.readLine();
                  if (line != null && !line.equals("")) {
                    st = new StringTokenizer(line, " ,\t\r\n");
                    while (st.hasMoreTokens()) {
                      switch (izoveriCheck) {
                        case 0:
                          izoveri = Integer.valueOf(token = st.nextToken()).intValue();
                          if (maxizoveri_local < izoveri)
                            maxizoveri_local = izoveri;
                          hkli = new int[3];
                          weight = new double[izoveri];
                          izoveriCheck++;
                          break;
                        default:
                          {
                            hkli[0] = Integer.valueOf(token = st.nextToken()).intValue();
                            hkli[1] = Integer.valueOf(token = st.nextToken()).intValue();
                            hkli[2] = Integer.valueOf(token = st.nextToken()).intValue();
                            weight[izoveriCheck - 1] = Double.valueOf(token = st.nextToken()).doubleValue();
                            izoveriCheck++;
                          }
                      }
                    }
                  }
                }
              }

            }

            int totmax = 0;
            int numberPoleFiguresPF = allPFs.size() / 3;
            numberOfPFPoint = new int[numberPoleFiguresPF];
            for (int i = 0; i < numberPoleFiguresPF; i++) {
//						int[] hkl = (int[]) allPFs.elementAt(i * 2);
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              numberOfPFPoint[i] = poleFig.size();
              if (numberOfPFPoint[i] > totmax)
                totmax = numberOfPFPoint[i];
            }
            expPF = new Vector(0, 1);
            for (int i = 0; i < numberPoleFiguresPF; i++) {
              int[] hkl = (int[]) allPFs.elementAt(i * 3);
              double[][] pfInt = new double[3][numberOfPFPoint[i]];
              Vector poleFig = (Vector) allPFs.elementAt(i * 3 + 2);
              for (int j = 0; j < numberOfPFPoint[i]; j++) {
                double[] pf = (double[]) poleFig.elementAt(j);
                pfInt[0][j] = pf[0] + sampleAngles[1];
                pfInt[1][j] = pf[1] + sampleAngles[2];
                pfInt[2][j] = pf[2];
              }
              expPF.addElement(hkl);
              expPF.addElement(pfInt);
            }
          }


          int[] izoveriv = new int[1];
          izoveriv[0] = maxizoveri_local;
          expPF.addElement(izoveriv);

          PFreader.close();
        } catch (Throwable io) {
          io.printStackTrace();
          try {
            PFreader.close();
          } catch (Throwable to) {
          }
          System.out.println("Error catched, check it out or send it to maud@ing.unitn.it");
          System.out.println("Your command was not completed successfully!");
        }
      }
    }

    return expPF;
  }

  public void initParameters() {
    plotTypeRB[0].setSelected(true);
    plotWhatRB[0].setSelected(false);
    plotWhatRB[1].setSelected(true);
    plotWhatRB[2].setSelected(false);
    plotWhatRB[3].setSelected(false);
    plotWhatRB[4].setSelected(false);
    plotWhatRB[5].setSelected(false);
    grayShadedCB.setSelected(Constants.grayShaded);
    logScaleCB.setSelected(MaudPreferences.getBoolean(logTexturePlotString, false));
    int samplenumber = parameterfile.samplesNumber();
    for (int i = 0; i < samplenumber; i++)
      sampleC.addItem(parameterfile.getSample(i).toXRDcatString());
    if (samplenumber > 0) {
      sampleC.setSelectedIndex(0);
    }
    thesample = parameterfile.getSample(0);
    int phasenumber = thesample.phasesNumber();
    for (int i = 0; i < phasenumber; i++)
      phaseC.addItem(thesample.getPhase(i).toXRDcatString());
    if (phasenumber > 0) {
      phaseC.setSelectedIndex(0);
      thephase = thesample.getPhase(0);
      thephase.refreshReflectionv = true; //force reflection list recomputing
//	    thephase.refreshIndices(thesample);
      thephase.sghklcompute(false); // use the largest range for reflections
      TableModel hklModel = new simplehklTableModel(thephase);
      hkltable.setModel(hklModel);
      hkltable.invalidate();
//      hkltable.setDefaultEditor(Boolean.class, new DefaultTableCellEditor());
    }
    maxAngleTF.setText(MaudPreferences.getPref(maxAngleString, "90"));
    pointsTF.setText(Integer.toString(lastResolution));
    zoomTF.setText(Double.toString(zoom));
    smoothTF.setText(Double.toString(filterWidth));
    setExpansionSlider();
  }

  public void setExpansionSlider() {
    expansionJS.setMaximum(256);
    expansionJS.setMinimum(0);
    expansionJS.setPaintTicks(true);
    expansionJS.setMajorTickSpacing(32);
    expansionJS.setMinorTickSpacing(8);

    expansionJS.setValue(MaudPreferences.getInteger(numberofColors, 64));

    expansionJS.setPaintLabels(true);
    expansionJS.setSnapToTicks(true);

    expansionJS.setLabelTable(expansionJS.createStandardLabels(32, 32));
  }

  public void initListener() {
    sampleC.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        int index = sampleC.getSelectedIndex();
        thesample = parameterfile.getSample(index);
      }
    });
    phaseC.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        Phase oldphase = thephase;
        int index = phaseC.getSelectedIndex();
        thephase = parameterfile.getActiveSample().getPhase(index);
        if (thephase != oldphase) {
          TableModel hklModel = new simplehklTableModel(thephase);
          hkltable.setModel(hklModel);
          hkltable.invalidate();
        }
      }
    });
  }

  public void wizard_action() {
    if (thephase == null || thesample == null)
      return;
    int hklnumbersel = hkltable.getSelectedRow();
    if (hklnumbersel < 0)
      hklnumbersel = 0;
    (new JWizardDisableD(this, thesample, thephase, hklnumbersel)).setVisible(true);
  }

  static String gridResString = "texturePlot.gridResolution";
  static String zoomString = "texturePlot.zoomFactor";
  static String maxAngleString = "texturePlot.maxAzimuthalAngle";
  static String logTexturePlotString = "texturePlot.logScale";
  static String numberofColors = "texturePlot.colorsNumber";
  public static int lastResolution = MaudPreferences.getInteger(gridResString, 101);
  public static double zoom = MaudPreferences.getDouble(zoomString, 1); // must be a power of 2
  public static double filterWidth = MaudPreferences.getDouble("texturePlot.gaussFilterWidth", 0.0);

  public static String minAlphaAngle = MaudPreferences.getPref("texturePlot.minODFAlphaAngle", "0.0");
  public static String maxAlphaAngle = MaudPreferences.getPref("texturePlot.maxODFAlphaAngle", "360.0");
  public static String stepAlphaAngle = MaudPreferences.getPref("texturePlot.stepODFAlphaAngle", "5.0");
  public static String minBetaAngle = MaudPreferences.getPref("texturePlot.minODFBetaAngle", "0.0");
  public static String maxBetaAngle = MaudPreferences.getPref("texturePlot.maxODFBetaAngle", "180.0");
  public static String stepBetaAngle = MaudPreferences.getPref("texturePlot.stepODFBetaAngle", "5.0");

//	static int lastContourLevel = 10;

  public void plot_ODF_action() {
    if (thephase == null || thesample == null)
      return;
    int do3DplotIndex = odf3DplotCB.getSelectedIndex();
    double alphaStart = Double.parseDouble(alphaStartTF.getText());
    double alphaEnd = Double.parseDouble(alphaEndTF.getText());
    double alphaStep = Double.parseDouble(alphaStepTF.getText());
    double betaStart = Double.parseDouble(betaStartTF.getText());
    double betaEnd = Double.parseDouble(betaEndTF.getText());
    double betaStep = Double.parseDouble(betaStepTF.getText());
    thephase.getActiveTexture().plotODFMap(alphaStart, alphaEnd, alphaStep, betaStart, betaEnd, betaStep, do3DplotIndex);
  }

  private void radialPF() {
    if (thephase == null || thesample == null)
      return;
    boolean twoDmap = plotTypeRB[0].isSelected();
    boolean coverage = plotWhatRB[0].isSelected();
    boolean reconstructed = plotWhatRB[1].isSelected();
    boolean experimental = plotWhatRB[2].isSelected();
    boolean inverse = plotWhatRB[3].isSelected();
    boolean absorptionCorrection = plotWhatRB[4].isSelected();
    boolean reconstructedStrain = plotWhatRB[5].isSelected();
    boolean experimentalStrain = plotWhatRB[6].isSelected();
    lastResolution = Math.abs(Integer.valueOf(pointsTF.getText()).intValue());
    MaudPreferences.setPref(gridResString, Integer.toString(lastResolution));
    zoom = Math.abs(Double.parseDouble(zoomTF.getText()));
    filterWidth = Math.abs(Double.valueOf(smoothTF.getText()).doubleValue());
    MaudPreferences.setPref(zoomString, Double.toString(zoom));
    MaudPreferences.setPref("texturePlot.gaussFilterWidth", Double.toString(filterWidth));
    String maxAngleS = maxAngleTF.getText();
    double maxAngle = Double.valueOf(maxAngleS).doubleValue();
    maxAngle = Constants.sqrt2 * Math.sin(maxAngle * Constants.DEGTOPI / 2.0);
    MaudPreferences.setPref(maxAngleString, maxAngleS);
    boolean logScale = logScaleCB.isSelected();
    String logValue = "false";
    if (logScale)
      logValue = "true";
    MaudPreferences.setPref(logTexturePlotString, logValue);

    int colrsNumber = expansionJS.getValue();
    if (colrsNumber == 0) {
      colrsNumber = 8;
      expansionJS.setValue(colrsNumber);
    }
    MaudPreferences.setPref(numberofColors, colrsNumber);

    int hklnumbersel = hkltable.getSelectedRow();
    if (hklnumbersel < 0)
      hklnumbersel = 0;
    final int hklnumber = hklnumbersel;

    boolean pfList = reconstructed || experimental;
    Reflection[] poleList = null;
    if ((pfList || absorptionCorrection) && twoDmap) {
      int numPoles = thephase.gethklNumber();
      Vector list = new Vector(0, 1);
      for (int i = 0; i < numPoles; i++) {
        Reflection refl = thephase.getReflex(i);
        if (refl.poleFigurePlot)
          list.addElement(refl);
      }
      int selPoles = list.size();
      if (selPoles == 0) {
        poleList = new Reflection[1];
        poleList[0] = thephase.getReflex(hklnumbersel);
      } else {
        poleList = new Reflection[selPoles];
        for (int i = 0; i < selPoles; i++)
          poleList[i] = (Reflection) list.elementAt(i);
        list.removeAllElements();
      }
    } else {
      poleList = new Reflection[1];
      poleList[0] = thephase.getReflex(hklnumbersel);
    }

    new PlotRadialPoleFigure(this, thesample, poleList, 0, lastResolution, zoom,
            filterWidth, grayShadedCB.isSelected(), maxAngle, logScale, colrsNumber);
  }

  public void plot_action() {
    if (thephase == null || thesample == null)
      return;
    boolean twoDmap = plotTypeRB[0].isSelected();
    boolean coverage = plotWhatRB[0].isSelected();
    boolean reconstructed = plotWhatRB[1].isSelected();
    boolean experimental = plotWhatRB[2].isSelected();
    boolean inverse = plotWhatRB[3].isSelected();
    boolean absorptionCorrection = plotWhatRB[4].isSelected();
    boolean reconstructedStrain = plotWhatRB[5].isSelected();
    boolean experimentalStrain = plotWhatRB[6].isSelected();
    lastResolution = Math.abs(Integer.valueOf(pointsTF.getText()).intValue());
    MaudPreferences.setPref(gridResString, Integer.toString(lastResolution));
    zoom = Math.abs(Double.parseDouble(zoomTF.getText()));
    filterWidth = Math.abs(Double.valueOf(smoothTF.getText()).doubleValue());
    MaudPreferences.setPref(zoomString, Double.toString(zoom));
    MaudPreferences.setPref("texturePlot.gaussFilterWidth", Double.toString(filterWidth));
    String maxAngleS = maxAngleTF.getText();
    double maxAngle = Double.valueOf(maxAngleS).doubleValue();
    maxAngle = Constants.sqrt2 * Math.sin(maxAngle * Constants.DEGTOPI / 2.0);
    MaudPreferences.setPref(maxAngleString, maxAngleS);
    boolean logScale = logScaleCB.isSelected();
    String logValue = "false";
    if (logScale)
      logValue = "true";
    MaudPreferences.setPref(logTexturePlotString, logValue);

    int colrsNumber = expansionJS.getValue();
    if (colrsNumber == 0) {
      colrsNumber = 8;
      expansionJS.setValue(colrsNumber);
    }
    MaudPreferences.setPref(numberofColors, colrsNumber);

    int hklnumbersel = hkltable.getSelectedRow();
    if (hklnumbersel < 0)
      hklnumbersel = 0;
    final int hklnumber = hklnumbersel;

    boolean pfList = reconstructed || experimental || reconstructedStrain || experimentalStrain;
    Reflection[] poleList = null;
    if ((pfList || absorptionCorrection) && twoDmap) {
      int numPoles = thephase.gethklNumber();
      Vector list = new Vector(0, 1);
      for (int i = 0; i < numPoles; i++) {
        Reflection refl = thephase.getReflex(i);
        if (refl.poleFigurePlot)
          list.addElement(refl);
      }
      int selPoles = list.size();
      if (selPoles == 0) {
        poleList = new Reflection[1];
        poleList[0] = thephase.getReflex(hklnumbersel);
      } else {
        poleList = new Reflection[selPoles];
        for (int i = 0; i < selPoles; i++)
          poleList[i] = (Reflection) list.elementAt(i);
        list.removeAllElements();
      }
    } else {
      poleList = new Reflection[1];
      poleList[0] = thephase.getReflex(hklnumbersel);
    }

    if (coverage && !pfList)
      (new PlotPFCoverage(TexturePlot.this, thesample, thephase, hklnumber)).setVisible(true);
    else if (absorptionCorrection && twoDmap)
      (new PlotPoleFigure(this, thesample, poleList, 2, lastResolution, zoom,
             filterWidth, grayShadedCB.isSelected(), maxAngle, logScale, colrsNumber)).setVisible(true);
    else if (reconstructed && !twoDmap)
      show3DPole(this, poleList[0], 1, lastResolution, maxAngle, logScale, colrsNumber);
    else if (reconstructed)
      (new PlotPoleFigure(this, thesample, poleList, 0, lastResolution, zoom,
              filterWidth, grayShadedCB.isSelected(), maxAngle, logScale, colrsNumber)).setVisible(true);
    else if (experimental)
      (new PlotPoleFigure(this, thesample, poleList, 1, lastResolution, zoom,
              filterWidth, grayShadedCB.isSelected(), maxAngle, logScale, colrsNumber)).setVisible(true);
    else if (inverse)
      (new PlotPoleFigure(this, thesample, poleList, 3, lastResolution, zoom,
              filterWidth, grayShadedCB.isSelected(), maxAngle, logScale, colrsNumber)).setVisible(true);
    else if (reconstructedStrain && !twoDmap)
      show3DPole(this, poleList[0], -1, lastResolution, maxAngle, false, colrsNumber);
    else if (reconstructedStrain)
      (new PlotPoleFigure(this, thesample, poleList, 4, lastResolution, zoom,
              filterWidth, grayShadedCB.isSelected(), maxAngle, false, colrsNumber)).setVisible(true);
    else if (experimentalStrain)
      (new PlotPoleFigure(this, thesample, poleList, 5, lastResolution, zoom,
              filterWidth, grayShadedCB.isSelected(), maxAngle, false, colrsNumber)).setVisible(true);
    return;
  }

  i3DCanvas pole3D = null;

  public void show3DPole(Frame parent, Reflection pole, int mode, int resolution, double maxAngle,
                         boolean logScale, int colrsNumber) {


    myJFrame poleFrame = new myJFrame(parent);

    Constants.grayShaded = grayShadedCB.isSelected();

/*    if (Constants.OpenGL) {
      try {
        pole3D = new PoleRendering3Dgl(pole, mode, resolution, maxAngle, logScale, colrsNumber);
        GLProfile glp = GLProfile.getDefault();
        GLCanvas canvas = new GLCanvas(new GLCapabilities(glp));
        canvas.addGLEventListener((PoleRendering3Dgl) pole3D);
        poleFrame.getContentPane().add(canvas, BorderLayout.CENTER);
        ((AnimatedRendering3Dgl)pole3D).setCanvas(canvas);
      } catch (Throwable e) {
        Constants.OpenGL = false;
        pole3D = new PoleRendering3Djgl(pole, mode, resolution, maxAngle, logScale, colrsNumber);
        poleFrame.getContentPane().add((Component) pole3D);
      }
    } else {*/
      pole3D = new PoleRendering3Djgl(pole, mode, resolution, maxAngle, logScale, colrsNumber);
      poleFrame.getContentPane().add((Component) pole3D);
 //   }

    poleFrame.setSize(500, 500);
    poleFrame.setVisible(true);

    pole3D.start();
  }


  public void chooseColor() {

//		PoleRendering.pole_color = JColorChooser.showDialog(
//																this, "Choose color", PoleRendering.pole_color);
  }

  public static void disableUnNecessaryFiles(Sample asample, Phase aphase, int hklnumbersel,
                                             boolean removeFiles, double resolution, double maxpolarangle) {

    Reflection reflex = aphase.getReflex(hklnumbersel);

    if (reflex != null && asample != null) {

//			asample.datasetPreparation(aphase, hklnumbersel);

      int np = asample.getNumberActiveDatafiles();

      double[] angles = null;

      double datax[] = new double[np];
      double datay[] = new double[np];
      boolean disable[] = new boolean[np];

	    int index = 0;
      for (int i = 0; i < asample.activeDatasetsNumber(); i++) {
				DataFileSet dataset = asample.getActiveDataSet(i);
	      for (int j = 0; j < dataset.activedatafilesnumber(); j++) {
		      DiffrDataFile adatafile = dataset.getActiveDataFile(j);
          angles = adatafile.getTextureAngles(adatafile.getPosition(aphase, hklnumbersel, 0));  // todo: v3.0 for all radiations?

          datax[index] = angles[0] * Math.cos(angles[1] * Constants.DEGTOPI);
          datay[index] = angles[0] * Math.sin(angles[1] * Constants.DEGTOPI);

          disable[index++] = true;
	      }
      }

      Vector pointarray = Angles.getArbitraryGridCoordinates(resolution, 6, maxpolarangle);
      int pointnumber = pointarray.size();
      double dx, dy;
      for (int i = 0; i < pointnumber; i++) {
        double pointcoord[] = (double[]) pointarray.elementAt(i);
        int firstNeigh = 0;
        dx = datax[0] - pointcoord[0];
        dy = datay[0] - pointcoord[1];
        double mindistance = dx * dx + dy * dy;
        for (int j = 1; j < np; j++) {
          dx = datax[j] - pointcoord[0];
          dy = datay[j] - pointcoord[1];
          double distance = dx * dx + dy * dy;
          if (distance < mindistance) {
            firstNeigh = j;
            mindistance = distance;
          }
        }
        disable[firstNeigh] = false;
      }
      for (int i = np - 1; i >= 0; i--) {
        if (disable[i]) {
          DiffrDataFile datafile = asample.getActiveDiffrDataFile(i);
          if (removeFiles) {
            DataFileSet dataset = (DataFileSet) datafile.getParent();
            dataset.removeDatafile(datafile);
          } else
            datafile.setCompute(false);
        }
      }
    }
  }

  public void dispose() {
    thephase = null;
    thesample = null;
    if (pole3D != null) {
      pole3D.stop();
      pole3D = null;
    }
    super.dispose();
  }

  public class JWizardDisableD extends JDialog {
    Sample asample = null;
    Phase aphase = null;
    int hklnumbersel = 0;

    JTextField resolutionTF = null;
    JTextField maxpolarangleTF = null;

    public JWizardDisableD(Frame parent, Sample asample, Phase aphase, int hklnumbersel) {

      super(parent, "Enforce hexagonal grid coverage", true);

      this.asample = asample;
      this.aphase = aphase;
      this.hklnumbersel = hklnumbersel;

      this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

      Container principalPanel = this.getContentPane();

      principalPanel.setLayout(new BorderLayout(3, 3));

      JPanel panel1 = new JPanel();
      panel1.setLayout(new GridLayout(2, 2, 3, 3));
      principalPanel.add(BorderLayout.NORTH, panel1);

      panel1.add(new JLabel("Hexagonal grid resolution:"));
      resolutionTF = new JTextField(Constants.FLOAT_FIELD);
      resolutionTF.setText("10");
      panel1.add(resolutionTF);
      panel1.add(new JLabel("Maximum polar angle:"));
      maxpolarangleTF = new JTextField(Constants.FLOAT_FIELD);
      maxpolarangleTF.setText("90");
      panel1.add(maxpolarangleTF);

      panel1 = new JPanel();
      panel1.setLayout(new GridLayout(1, 3, 3, 3));

      JButton disableB = new JButton("Disable files");
      disableB.setToolTipText("Disable all files except the closest to any grid point");
      panel1.add(disableB);
      disableB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          double res = Double.valueOf(resolutionTF.getText()).doubleValue();
          double max = Double.valueOf(maxpolarangleTF.getText()).doubleValue();
          processFiles(false, res, max);
          closeDialog();
        }
      });

      disableB = new JButton("Remove files");
      disableB.setToolTipText("Remove all files except the closest to any grid point");
      panel1.add(disableB);
      disableB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          double res = Double.valueOf(resolutionTF.getText()).doubleValue();
          double max = Double.valueOf(maxpolarangleTF.getText()).doubleValue();
          processFiles(true, res, max);
          closeDialog();
        }
      });

      disableB = new JCancelButton();
      panel1.add(disableB);
      disableB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          closeDialog();
        }
      });

      principalPanel.add(BorderLayout.SOUTH, panel1);

      this.pack();
    }

    public void processFiles(boolean removeFiles, double res, double max) {
      disableUnNecessaryFiles(asample, aphase, hklnumbersel, removeFiles, res, max);
    }

    public void closeDialog() {
      this.setVisible(false);
      dispose();
    }

    public void dispose() {
      aphase = null;
      asample = null;
      super.dispose();
    }
  }

}
