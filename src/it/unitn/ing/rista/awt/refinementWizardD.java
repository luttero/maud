/*
 * @(#)refinementWizardD.java created 20/12/1998 Riva del Garda
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.FilePar;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.jdesktop.swingx.JXTitledPanel;
import org.jdesktop.swingx.border.DropShadowBorder;

/**
 * The refinementWizardD is a dialog that permits to set up the refinement.
 *
 * @version $Revision: 1.5 $, $Date: 2004/11/18 09:30:49 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class refinementWizardD extends myJFrame {

  JRadioButton[] refineRB;

  public refinementWizardD(Frame parentFrame) {
    super(parentFrame, "Refinement wizard");

//		createDefaultMenuBar();

//		setTitle("Refinement wizard");

    setHelpFilename("wizard.txt");

//    FilePar parameterfile = (FilePar) getFileParent();

    String[] labelRB = {"Background and scale parameters", "Previous + basic phase parameters",
                        "Previous + microstructure parameters", "Previous + crystal structure parameters",
                        "All parameters for texture", "Crystal+Texture parameters",
                        "All parameters for strain", "Crystal+Strain analysis",
                        "Strain+Texture parameters", "Crystal+Texture+Strain parameters",
                        "Quantitative analysis", "Crystal structure analysis", "Texture analysis",
                        "Crystal+Texture analysis", "Strain analysis", "Crystal+Strain analysis",
                        "Strain+Texture analysis", "Crystal+Texture+Strain analysis",
                        "INEL PSD calibration"};

    refineRB = new JRadioButton[labelRB.length];

    Container c1 = getContentPane();
    c1.setLayout(new BorderLayout());

    JPanel principalPanel = new JPanel();
    principalPanel.setLayout(new BorderLayout());
    principalPanel.setBorder(new BevelBorder(BevelBorder.LOWERED));
    c1.add(BorderLayout.NORTH, principalPanel);

    JXTitledPanel refinePanel = new JXTitledPanel("Refine parameters");
    refinePanel.setBorder(new DropShadowBorder(UIManager.getColor("Control"), 1, true));
    refinePanel.setLayout(new GridLayout(0, 1, 0, 0));
//    refinePanel.setBorder(new TitledBorder("Refine"));
    principalPanel.add(BorderLayout.WEST, refinePanel);

    ButtonGroup rbg = new ButtonGroup();
    JButton customB;

    for (int i = 0; i < 10; i++) {
      final int index = i;

      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      refinePanel.add(jp);
      refineRB[i] = new JRadioButton(labelRB[i]);
      customB = new JButton("Custom");
      jp.add(refineRB[i]);
      rbg.add(refineRB[i]);
      jp.add(customB);
      customB.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          customWizard(index);
        }
      });
    }

    JXTitledPanel specialPanel = new JXTitledPanel("Complete analysis");
    specialPanel.setBorder(new DropShadowBorder(UIManager.getColor("Control"), 1, true));
//    JPanel specialPanel = new JPanel();
    specialPanel.setLayout(new GridLayout(0, 1, 0, 0));
//    specialPanel.setBorder(new TitledBorder("Special"));
    principalPanel.add(BorderLayout.EAST, specialPanel);

    for (int i = 10; i < labelRB.length; i++) {
//      final int index = i;
      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));
      specialPanel.add(jp);
      refineRB[i] = new JRadioButton(labelRB[i]);
      jp.add(refineRB[i]);
      rbg.add(refineRB[i]);
    }

/*    JPanel commandPanel = new JPanel();
    commandPanel.setLayout(new BorderLayout());
    commandPanel.setBorder(new TitledBorder("Commands"));
    c1.add(BorderLayout.CENTER, commandPanel);
    JPanel jp = new JPanel();
    jp.setLayout(new GridLayout(0, 4, 3, 3));
    commandPanel.add(jp);
    JButton cmdB = new JButton("Fix all parameters");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().fixAllParametersPreserveBound();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Free all parameters");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().freeAllParametersPreserveBound();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Free backgrounds");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().freeAllBackgroundParameters();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Free scale pars");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().freeAllScaleParameters();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Free basic pars");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().freeAllBasicParameters();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Bound B factors");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().boundBFactors();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Free microstructure");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().freeAllMicroParameters();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Free crystal struct");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        FilePar parameterfile = getFileParent();
        parameterfile.freeAllBasicParameters();
        parameterfile.freeAllCrystalParameters();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Free texture");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().freeAllTextureParameters();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Free strain");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().freeAllStrainParameters();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Fix backgrounds");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().fixAllBackgroundParametersPreserveBound();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Fix texture");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().fixAllTextureParametersPreserveBound();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Fix strain");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().fixAllStrainParametersPreserveBound();
      }
    });
    jp.add(cmdB);
    cmdB = new JButton("Bound datafiles by bank");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().boundMonitorsByBank();
      }
    });
    cmdB.setToolTipText("Use this to bound all spectra parameters by bank number (for IPNS/LANSCE/GSAS data)");
    jp.add(cmdB);
    cmdB = new JButton("Bound datafiles by angles");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        (new ChooseAnglesFrame(refinementWizardD.this)).setVisible(true);
      }
    });
    cmdB.setToolTipText("Use this to bound all spectra parameters by angles (to be specified)");
    jp.add(cmdB);
    cmdB = new JButton("Free all spectra monitors");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().freeAllCountMonitors();
      }
    });
    cmdB.setToolTipText("Use this to free all spectra count monitor parameters (eq. to a scale factor)");
    jp.add(cmdB);
    cmdB = new JButton("Fix all spectra monitors");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().fixAllCountMonitorsPreserveBound();
      }
    });
    cmdB.setToolTipText("Use this to fix all spectra count monitor parameters (eq. to a scale factor)");
    jp.add(cmdB);*/

    JPanel p1 = new JPanel();
    p1.setLayout(new FlowLayout(FlowLayout.RIGHT, 3, 3));
    c1.add(BorderLayout.CENTER, p1);
    JButton closeButton = new JIconButton("TrafficGreen.gif", "Go!");
    p1.add(closeButton);
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        refineWizard();
        setVisible(false);
        dispose();
      }
    });
    getRootPane().setDefaultButton(closeButton);
    JButton cancelButton = new JButton("Set parameters");
    p1.add(cancelButton);
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        retrieveParameters();
        setVisible(false);
        dispose();
      }
    });
    cancelButton = new JCancelButton();
    p1.add(cancelButton);
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        setVisible(false);
        dispose();
      }
    });
    setHelpButton(p1);

    initParameters();

    pack();

//    centerOnScreen();
  }

  public void initParameters() {
//    FilePar parameterfile = (FilePar) getFileParent();

    refineRB[0].setSelected(true);
  }

  public void retrieveParameters() {
    FilePar parameterfile = getFileParent();

    for (int i = 0; i < 10; i++)
      if (refineRB[i].isSelected())
        parameterfile.prepareWizard(null, i);
  }

  public void customWizard(int index) {
  }

  public void refineWizard() {
//    FilePar parameterfile = getFileParent();

    for (int i = 0; i < 18; i++)
      if (refineRB[i].isSelected())
        getmainFrame().refineWizard(i);
  }

  class ChooseAnglesFrame extends myJFrame {

    JCheckBox[] anglesCB = null;

    public ChooseAnglesFrame(Frame aframe) {

      super(aframe);

      ChooseAnglesFrame.this.setOwnPosition = true;

      Container c1 = ChooseAnglesFrame.this.getContentPane();

      c1.setLayout(new BorderLayout(3, 3));
      JPanel principalPanel = new JPanel();
      c1.add(BorderLayout.CENTER, principalPanel);

      principalPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 3, 3));

      JPanel panel3 = new JPanel();
      panel3.setLayout(new GridLayout(0, 1, 3, 3));
      principalPanel.add(panel3);

      panel3.add(new JLabel("Check the angles to be used :"));

      String[] angleNames = {"omega",
                             "chi",
                             "phi",
                             "eta"};

      JPanel jp1;
      anglesCB = new JCheckBox[angleNames.length];
      for (int i = 0; i < angleNames.length; i++) {
        jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 2));
        panel3.add(jp1);
        jp1.add(anglesCB[i] = new JCheckBox(angleNames[i]));
        anglesCB[i].setSelected(false);
      }

      panel3 = new JPanel();
      panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      c1.add(BorderLayout.SOUTH, panel3);

      JButton stopD = new JCancelButton();
      stopD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          ChooseAnglesFrame.this.setVisible(false);
          ChooseAnglesFrame.this.dispose();
        }
      });
      panel3.add(stopD);

      JButton startD = new JCloseButton();

      final FilePar parameterfile = getFileParent();

      startD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          boolean[] useAngle = new boolean[4];
          for (int i = 0; i < 4; i++) {
            useAngle[i] = anglesCB[i].isSelected();
          }
          parameterfile.boundMonitorsByAngles(useAngle);

          ChooseAnglesFrame.this.setVisible(false);
          ChooseAnglesFrame.this.dispose();
        }
      });
      panel3.add(startD);
      getRootPane().setDefaultButton(startD);

//			this.setHelpButton(panel3);

      ChooseAnglesFrame.this.setTitle("Choose angles");

      ChooseAnglesFrame.this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);

      ChooseAnglesFrame.this.pack();
      ChooseAnglesFrame.this.setResizable(false);

    }
  }


}
