/*
 * @(#)refinementOptionsD.java created 16/09/1998 Mesiano
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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

import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MaudPreferences;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Display a refinementOptionsD, that permits to change some refinement options.
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:53 $
 * @author Luca Lutterotti
 */

public class refinementOptionsD extends myJFrame {

//	JSlider priorityJS;
//	JComboBox intensityExtractionCB;
  JComboBox textureComputationCB;
//	JComboBox positionExtractionCB;
  JComboBox strainComputationCB;
//	JComboBox structureFactorExtractionCB;
  JComboBox structureFactorComputationCB;

  JComboBox backgroundInterpolationCB;
  JComboBox computationAlgorithmCB;
  JCheckBox memoryControlCB;
  JCheckBox theoreticalWeightCB;
  JComboBox minimizeCB;
  JComboBox weightsCB;
  JCheckBox addStatisticalErrorCB;
	JCheckBox storeSpectraCB;
	JCheckBox storeStructureCB;
	JCheckBox storeTextureCB;

  public refinementOptionsD(Frame parentFrame) {
    super(parentFrame, "Analysis options");

    setOwnPosition = true;
//		createDefaultMenuBar();

    final FilePar parameterfile = getFileParent();

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout());

    JPanel principalPanel = new JPanel();
    principalPanel.setLayout(new BorderLayout());
    c1.add(BorderLayout.NORTH, principalPanel);

    JPanel p1 = new JPanel();
    p1.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    principalPanel.add("North", p1);

/*		p1 = new JPanel();
		p1.setLayout(new FlowLayout(FlowLayout.RIGHT,5,5));
		principalPanel.add("Center", p1);

		p1.add(new JLabel("Computation priority: "));
		JLabel priorityTF = new JLabel();
		priorityJS = new JSlider();
		priorityJS.setToolTipText("Set the computation priority; big values don't permit window refresh");
		listener = new SliderListener(priorityTF);
		priorityJS.addChangeListener(listener);
		p1.add(priorityTF);
		p1.add(priorityJS);*/

    JPanel lowPanel = new JPanel();
    lowPanel.setLayout(new BorderLayout(6, 6));
    c1.add(BorderLayout.CENTER, lowPanel);

    JPanel texturePanel = new JPanel();
    lowPanel.add(BorderLayout.NORTH, texturePanel);
    texturePanel.setLayout(new GridLayout(0, 1, 0, 0));

/*		JPanel jPanel9 = new JPanel();
		jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT,6,6));
		texturePanel.add(jPanel9);
		jPanel9.add(new JLabel("Enable intensity extraction at "));
		intensityExtractionCB = new JComboBox();
		for (int i = 0; i < parameterfile.ComputationOptionNumber; i++)
			intensityExtractionCB.addItem(parameterfile.COMP_STATUS[i]);
		intensityExtractionCB.setToolTipText("Specify when the intensity extraction method should be used");
		jPanel9.add(intensityExtractionCB);*/

    JPanel jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    texturePanel.add(jPanel9);
    jPanel9.add(new JLabel("Enable texture extraction/computation at "));
    textureComputationCB = new JComboBox();
    for (int i = 0; i < parameterfile.ComputationOptionNumber; i++)
      textureComputationCB.addItem(parameterfile.COMP_STATUS[i]);
    textureComputationCB.setToolTipText("Specify when the texture extraction/computation should be used");
    jPanel9.add(textureComputationCB);

/*		jPanel9 = new JPanel();
		jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT,6,6));
		texturePanel.add(jPanel9);
		jPanel9.add(new JLabel("Enable position extraction at "));
		positionExtractionCB = new JComboBox();
		for (int i = 0; i < parameterfile.ComputationOptionNumber; i++)
			positionExtractionCB.addItem(parameterfile.COMP_STATUS[i]);
		positionExtractionCB.setToolTipText("Specify when the position extraction method should be used");
		jPanel9.add(positionExtractionCB);*/

    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    texturePanel.add(jPanel9);
    jPanel9.add(new JLabel("Enable strain extraction/computation at "));
    strainComputationCB = new JComboBox();
    for (int i = 0; i < parameterfile.ComputationOptionNumber; i++)
      strainComputationCB.addItem(parameterfile.COMP_STATUS[i]);
    strainComputationCB.setToolTipText("Specify when the strain extraction/computation should be used");
    jPanel9.add(strainComputationCB);

/*		jPanel9 = new JPanel();
		jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT,6,6));
		texturePanel.add(jPanel9);
		jPanel9.add(new JLabel("Enable structure factor extraction at "));
		structureFactorExtractionCB = new JComboBox();
		for (int i = 0; i < parameterfile.ComputationOptionNumber; i++)
			structureFactorExtractionCB.addItem(parameterfile.COMP_STATUS[i]);
		structureFactorExtractionCB.setToolTipText(
                        "Specify when the structure factor extraction method should be used");
		jPanel9.add(structureFactorExtractionCB);*/

    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    texturePanel.add(jPanel9);
    jPanel9.add(new JLabel("Enable structure factor extraction/computation at "));
    structureFactorComputationCB = new JComboBox();
    for (int i = 0; i < parameterfile.ComputationOptionNumber; i++)
      structureFactorComputationCB.addItem(parameterfile.COMP_STATUS[i]);
    structureFactorComputationCB.setToolTipText(
            "Specify when the structure factor extraction/computation should be used");
    jPanel9.add(structureFactorComputationCB);

    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    texturePanel.add(jPanel9);
    jPanel9.add(new JLabel("Enable background interpolation at "));
    backgroundInterpolationCB = new JComboBox();
    for (int i = 0; i < parameterfile.ComputationOptionNumber; i++)
      backgroundInterpolationCB.addItem(parameterfile.COMP_STATUS[i]);
    backgroundInterpolationCB.setToolTipText(
            "Specify when the background interpolation should be performed");
    jPanel9.add(backgroundInterpolationCB);

    JPanel optionsP = new JPanel();
    optionsP.setLayout(new GridLayout(0, 1, 0, 0));
    lowPanel.add(BorderLayout.CENTER, optionsP);

    jPanel9 = new JPanel();
    optionsP.add(jPanel9);
    jPanel9.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
    jPanel9.add(new JLabel("Refinement model: "));
    computationAlgorithmCB = new JComboBox();
    computationAlgorithmCB.setToolTipText("Select the refinement algorithm to be used");
    for (int i = 0; i < parameterfile.getsubordClassNumber(parameterfile.optimizationAlgorithmID); i++)
      computationAlgorithmCB.addItem(parameterfile.getsubordIdentifier(parameterfile.optimizationAlgorithmID, i));
    jPanel9.add(computationAlgorithmCB);
    JButton jb = new JButton("Algorithms options");
    jb.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
          String selectedAlgorithm = computationAlgorithmCB.getSelectedItem().toString();
          if (!parameterfile.getOptimizationAlgorithm().identifier.equals(selectedAlgorithm))
            parameterfile.setOptimizationAlgorithm(selectedAlgorithm);
          parameterfile.getOptimizationAlgorithm().getOptionsDialog(refinementOptionsD.this).
                                  setVisible(true);}
    });
    jPanel9.add(jb);


    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    optionsP.add(jPanel9);
    memoryControlCB = new JCheckBox("Keep lower memory occupation");
    memoryControlCB.setToolTipText("The derivative matrix will be allocated as double instead of double");
    jPanel9.add(memoryControlCB);

    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    texturePanel.add(jPanel9);

    jPanel9.add(new JLabel("Quantity to minimize: "));
    minimizeCB = new JComboBox();
    for (int i = 0; i < parameterfile.minimizeQuantity.length; i++)
      minimizeCB.addItem(parameterfile.minimizeQuantity[i]);
    minimizeCB.setToolTipText("Specify the quantity to be minimized in the refinement procedure");
    jPanel9.add(minimizeCB);

    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    texturePanel.add(jPanel9);

    jPanel9.add(new JLabel("Weight statistic based on "));
    weightsCB = new JComboBox();
    for (int i = 0; i < parameterfile.weights.length; i++)
      weightsCB.addItem(parameterfile.weights[i]);
    weightsCB.setToolTipText("Specify the kind of weights to be used in the non linear least squares routine");
    jPanel9.add(weightsCB);

    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    optionsP.add(jPanel9);
    theoreticalWeightCB = new JCheckBox("Use theoretical weights");
    theoreticalWeightCB.setToolTipText("Use weights based on computed intensity instead of measured instensity");
    jPanel9.add(theoreticalWeightCB);

    jPanel9 = new JPanel();
    jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
    optionsP.add(jPanel9);
    addStatisticalErrorCB = new JCheckBox("Add error to output fitting files");
    addStatisticalErrorCB.setToolTipText("Add statistical error in intensity to the output fitting files");
    jPanel9.add(addStatisticalErrorCB);

		jPanel9 = new JPanel();
		jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT,6,6));
		optionsP.add(jPanel9);
		storeSpectraCB = new JCheckBox("Store spectra in the analysis file");
		storeSpectraCB.setToolTipText("This will store the spectra inside the analysis file when saving (original datafiles not needed)");
		jPanel9.add(storeSpectraCB);

	  jPanel9 = new JPanel();
	  jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT,6,6));
	  optionsP.add(jPanel9);
	  storeStructureCB = new JCheckBox("Store structure factors in the analysis file");
	  storeStructureCB.setToolTipText("This will store the structure factors inside the analysis file (if not check you loose the structure factors on reload)");
	  jPanel9.add(storeStructureCB);

	  jPanel9 = new JPanel();
	  jPanel9.setLayout(new FlowLayout(FlowLayout.RIGHT,6,6));
	  optionsP.add(jPanel9);
	  storeTextureCB = new JCheckBox("Store texture factors in the analysis file");
	  storeTextureCB.setToolTipText("This will store the texture factors inside the analysis file (if not check you loose the texture factors on reload)");
	  jPanel9.add(storeTextureCB);

	  p1 = new JPanel();
    p1.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    c1.add("South", p1);
    JButton cancelButton = new JCancelButton();
    p1.add(cancelButton);
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        setVisible(false);
        dispose();
      }
    });
    JButton closeButton = new JCloseButton();
    p1.add(closeButton);
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        retrieveParameters();
        setVisible(false);
        dispose();
      }
    });
    getRootPane().setDefaultButton(closeButton);

    initParameters();

    pack();
//		priorityJS.setValue(parameterfile.getThreadPriority());

  }

  public void initParameters() {
    FilePar parameterfile = getFileParent();
/*		priorityJS.setMaximum(Thread.MAX_PRIORITY);
		priorityJS.setMinimum(Thread.MIN_PRIORITY);
		priorityJS.setValue(Thread.MAX_PRIORITY);
		priorityJS.setPaintTicks(true);
		priorityJS.setMajorTickSpacing(4);
		priorityJS.setMinorTickSpacing(1);
		priorityJS.setPaintLabels( true );
		priorityJS.setSnapToTicks( true );
		priorityJS.setLabelTable(priorityJS.createStandardLabels(4));*/

//		intensityExtractionCB.setSelectedItem(parameterfile.getTextureFactorsExtractionStatus());
    textureComputationCB.setSelectedItem(parameterfile.getTextureComputationStatus());
//		positionExtractionCB.setSelectedItem(parameterfile.getPositionExtractionStatus());
    strainComputationCB.setSelectedItem(parameterfile.getStrainComputationStatus());
//		structureFactorExtractionCB.setSelectedItem(parameterfile.getStructureFactorExtractionStatus());
    structureFactorComputationCB.setSelectedItem(parameterfile.getStructureFactorComputationStatus());
    backgroundInterpolationCB.setSelectedItem(parameterfile.getBackgroundInterpolationStatus());
    memoryControlCB.setSelected(parameterfile.getMemoryOccupationControl());
    minimizeCB.setSelectedItem(parameterfile.getMinimizeQuantity());
    weightsCB.setSelectedItem(parameterfile.getWeightingScheme());
    theoreticalWeightCB.setSelected(parameterfile.theoreticalWeightingScheme());
    addStatisticalErrorCB.setSelected(parameterfile.addStatisticalError);
    computationAlgorithmCB.setSelectedItem(parameterfile.getOptimizationAlgorithm().identifier);
//    Removed also from preferences
//		Constants.speedUp = MaudPreferences.getBoolean(MaudPreferences.speedupComp);
    storeSpectraCB.setSelected(parameterfile.storeSpectraWithAnalysis());
	  storeStructureCB.setSelected(!parameterfile.compactSavingStructureFactors());
	  storeTextureCB.setSelected(!parameterfile.compactSavingTextureFactors());
  }

  public void retrieveParameters() {
    FilePar parameterfile = getFileParent();
    parameterfile.setOptimizationAlgorithm(computationAlgorithmCB.getSelectedItem().toString());
//		parameterfile.setThreadPriority(priorityJS.getValue());
    parameterfile.setTextureFactorsExtractionStatus(textureComputationCB.getSelectedItem().toString());
    parameterfile.setTextureComputationStatus(textureComputationCB.getSelectedItem().toString());
    parameterfile.setPositionExtractionStatus(strainComputationCB.getSelectedItem().toString());
    parameterfile.setStrainComputationStatus(strainComputationCB.getSelectedItem().toString());
    parameterfile.setStructureFactorExtractionStatus(
            structureFactorComputationCB.getSelectedItem().toString());
    parameterfile.setStructureFactorComputationStatus(
            structureFactorComputationCB.getSelectedItem().toString());
    parameterfile.setBackgroundInterpolationStatus(
            backgroundInterpolationCB.getSelectedItem().toString());
    parameterfile.setMemoryOccupationControl(memoryControlCB.isSelected());
    parameterfile.setMinimizeQuantity(minimizeCB.getSelectedItem().toString());
    parameterfile.setWeightingScheme(weightsCB.getSelectedItem().toString());
    parameterfile.setTheoreticalWeightingScheme(theoreticalWeightCB.isSelected());
    parameterfile.addStatisticalError = addStatisticalErrorCB.isSelected();
    boolean store = storeSpectraCB.isSelected();
    boolean oldstore = parameterfile.storeSpectraWithAnalysis();
    parameterfile.setStoreSpectraOption(store);
    if (oldstore != store) // only if the user makes a different choice
      MaudPreferences.setPref("analysis_default.storeSpectraWithParameters", store);
	  parameterfile.setCompactSavingStructureFactors(!storeStructureCB.isSelected());
	  parameterfile.setCompactSavingTextureFactors(!storeTextureCB.isSelected());
  }


}
