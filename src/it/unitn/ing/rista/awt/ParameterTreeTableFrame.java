/*
 * @(#)ParameterTreeTableFrame.java created 09/09/1998 Trento-Firenze (Pendolino).
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

import it.unitn.ing.rista.awt.treetable.JTreeTable;
import it.unitn.ing.rista.awt.treetable.ParameterTreeModel;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.interfaces.basicObj;
import it.unitn.ing.rista.util.Constants;

import javax.swing.*;
import javax.swing.event.DocumentListener;
import javax.swing.event.DocumentEvent;
import javax.swing.border.TitledBorder;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

/**
 * Display a ParameterTreeTable, using a JTreeTable.
 *
 * @version $Revision: 1.12 $, $Date: 2006/11/20 21:36:24 $
 * @author Luca Lutterotti
 */

public class ParameterTreeTableFrame extends myJFrame {

  JTreeTable treeTable = null;
  JTextField searchField = null;
  ParameterTreeModel parameterTreeModel = null;

  public ParameterTreeTableFrame(Frame parentFrame, basicObj parameterfile) {
    super(parentFrame);

    initializeSizeAndPosition(
            true, "parTreeFrame.frameWidth", "parTreeFrame.frameHeight", 740, 700,
            true, "parTreeFrame.framePositionX", "parTreeFrame.framePositionY", 10, 20);

    createDefaultMenuBar();

    setTitle("TreeTable");
    parameterTreeModel = new ParameterTreeModel(this, parameterfile);
    treeTable = new JTreeTable(parameterTreeModel);

    JScrollPane scrollpane = new JScrollPane(treeTable);
    scrollpane.setPreferredSize(new Dimension(440, 400));

    Container principalPane = getContentPane();
    principalPane.setLayout(new BorderLayout(Constants.borderInside, Constants.borderInside));

    JPanel subPrincipal = new JPanel(new BorderLayout());
    subPrincipal.add(BorderLayout.CENTER, scrollpane);
    principalPane.add(BorderLayout.CENTER, subPrincipal);

    JPanel commandPanel = new JPanel();
    commandPanel.setLayout(new BorderLayout());
    commandPanel.setBorder(new TitledBorder("Commands"));
    subPrincipal.add(BorderLayout.SOUTH, commandPanel);
    JPanel jp = new JPanel();
    jp.setLayout(new GridLayout(0, 4, Constants.borderInside, Constants.borderInside));
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
	  cmdB = new JButton("Free selected parameters");
	  cmdB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  freeAllSelectedParameters();
		  }
	  });
	  jp.add(cmdB);
	  cmdB = new JButton("Fix selected parameters");
	  cmdB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  fixAllSelectedParameters();
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
        (new ChooseAnglesFrame2(ParameterTreeTableFrame.this)).setVisible(true);
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
    jp.add(cmdB);

	  cmdB = new JButton("Free all linear abs par");
	  cmdB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  getFileParent().freeAllLinearAbsorption();
		  }
	  });
	  cmdB.setToolTipText("Use this to free all spectra linear absorption parameters (only TOF)");
	  jp.add(cmdB);
	  cmdB = new JButton("Fix all all linear abs par");
	  cmdB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  getFileParent().fixAllLinearAbsorptionPreserveBound();
		  }
	  });
	  cmdB.setToolTipText("Use this to fix all spectra linear absorption parameters (only TOF)");
	  jp.add(cmdB);

    cmdB = new JButton("Bound all banks coeffs");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().boundAllBankCoefficients();
      }
    });
    cmdB.setToolTipText("Use this to bound togheter all banks similar coefficients");
    jp.add(cmdB);
    cmdB = new JButton("Uniform all banks");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().forceBoundAllBankCoefficients();
      }
    });
    cmdB.setToolTipText("Use this to force bound togheter all banks in an instrument");
    jp.add(cmdB);

    cmdB = new JButton("Refine all DIFC/shifts");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().refineAllZEROBankCoefficients(false);
      }
    });
    cmdB.setToolTipText("Use this to set refinable all DIFC coefficients of TOF banks or individual datafiles shifts");
    jp.add(cmdB);

    cmdB = new JButton("Refine all y-z displ");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().refineAllXYSampleDisplacements();
      }
    });
    cmdB.setToolTipText("Use this to set refinable all individual datafiles x/y sample displacement");
    jp.add(cmdB);

    cmdB = new JButton("Refine TOF Bank SF");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().refineAllTOFSFBankCoefficients();
      }
    });
    cmdB.setToolTipText("Use this to set refinable the scale factor of all TOF banks");
    jp.add(cmdB);

	  cmdB = new JButton("Set aniso B factors");
	  cmdB.setToolTipText("Only for selected objects and descendant");
	  cmdB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  setAnisotropicBfactors();
		  }
	  });
	  cmdB.setToolTipText("Use this to set refinable the scale factor of all TOF banks");
	  jp.add(cmdB);

    cmdB = new JButton("Shrink min max values");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().shrinkMinMax();
      }
    });
    cmdB.setToolTipText("Shrink all min max by 30%, but not the atom fract coord.");
    jp.add(cmdB);

    cmdB = new JButton("Enlarge min max values");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().enlargeMinMax();
      }
    });
    cmdB.setToolTipText("Enlarge all min max by 50%, but not the atom fract coord.");
    jp.add(cmdB);

    cmdB = new JButton("Center min max range");
    cmdB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        getFileParent().centerMinMax();
      }
    });
    cmdB.setToolTipText("Center the range of min max around the value, only free pars and not the atom fract coord.");
    jp.add(cmdB);

    JPanel buttonP = new JPanel(new FlowLayout(FlowLayout.RIGHT, Constants.borderInside, Constants.borderInside));
    principalPane.add(BorderLayout.SOUTH, buttonP);

    JLabel searchLabel = new JLabel("Search: ");
    buttonP.add(searchLabel);
    searchField = new JTextField(18);
    searchField.setToolTipText("Type a text here to show only parameters containing that text in the CIF label. Case sensitive!");
    buttonP.add(searchField);
    DocumentListener documentListener = new DocumentListener() {
      public void changedUpdate(DocumentEvent documentEvent) {
        updateTree();
      }
      public void insertUpdate(DocumentEvent documentEvent) {
        updateTree();
      }
      public void removeUpdate(DocumentEvent documentEvent) {
        updateTree();
      }
      private void updateTree() {
        parameterTreeModel.reload();
        expandAllRows();
      }
    };
    searchField.getDocument().addDocumentListener(documentListener);

    buttonP.add(new JLabel("        "));

    JButton prefB = new JButton("Parameter preferences");
    buttonP.add(prefB);
    prefB.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent event) {
        Utility.showParameterPrefs(ParameterTreeTableFrame.this);
      }
    });

    JButton okB = new JButton("Expand all");
    buttonP.add(okB);
    okB.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent event) {
        expandAllRows();
      }
    });

    okB = new JButton("Collapse all");
    buttonP.add(okB);
    okB.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent event) {
        collapseAllRows();
      }
    });

    JCloseButton cancelB = new JCloseButton("Close");
    buttonP.add(cancelB);
    cancelB.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent event) {
        setVisible(false);
        dispose();
      }
    });
    getRootPane().setDefaultButton(cancelB);

    pack();
    setVisible(true);

  }

	private void setAnisotropicBfactors() {
		TreePath path = treeTable.getTree().getSelectionPath();
		if (path != null) {
			Object mRow = path.getLastPathComponent();
			basicObj obj1 = parameterTreeModel.getParameter(mRow);
			if (obj1 instanceof XRDcat) {
				XRDcat obj = (XRDcat) obj1;
				Vector<basicObj> objs = new Vector<basicObj>();
				objs.add(obj);
				objs.addAll(getAllChildren(obj, getSearchString()));
				for (int i = 0; i < objs.size(); i++)
					if (objs.get(i) instanceof AtomSite)
						((AtomSite) objs.get(i)).setAnisotropicBfactors();
			}
		} else
			System.out.println("Nothing selected in the three path!");
	}

	private void freeAllSelectedParameters() {
		TreePath path = treeTable.getTree().getSelectionPath();
		if (path != null) {
			Object mRow = path.getLastPathComponent();
			basicObj obj1 = parameterTreeModel.getParameter(mRow);
			if (obj1 instanceof XRDcat) {
				XRDcat obj = (XRDcat) obj1;
				Vector<basicObj> objs = new Vector<basicObj>();
				objs.add(obj);
				objs.addAll(getAllChildren(obj, getSearchString()));
				for (int i = 0; i < objs.size(); i++)
					if (objs.get(i) instanceof Parameter)
						((Parameter) objs.get(i)).setRefinable();
					else
						((XRDcat) objs.get(i)).freeAllParameters();
			}
		} else
			System.out.println("Nothing selected in the three path!");
	}

	private void fixAllSelectedParameters() {
		TreePath path = treeTable.getTree().getSelectionPath();
		if (path != null) {
			Object mRow = path.getLastPathComponent();
			basicObj obj1 = parameterTreeModel.getParameter(mRow);
			if (obj1 instanceof XRDcat) {
				XRDcat obj = (XRDcat) obj1;
				Vector<basicObj> objs = new Vector<basicObj>();
				objs.add(obj);
				objs.addAll(getAllChildren(obj, getSearchString()));
				for (int i = 0; i < objs.size(); i++)
					if (objs.get(i) instanceof Parameter)
						((Parameter) objs.get(i)).setNotRefinable();
					else
						((XRDcat) objs.get(i)).fixAllParameters();
			}
		} else
			System.out.println("Nothing selected in the three path!");
	}
	
	public Vector<basicObj> getAllChildren(basicObj obj, String toSearch) {
		Vector<basicObj> allObjs = new Vector<basicObj>();
		basicObj[] children = obj.getChildren(toSearch);
		for (int i = 0; i < children.length; i++) {
			allObjs.add(children[i]);
			if (!(children[i] instanceof Parameter))
				allObjs.addAll(getAllChildren(children[i], toSearch));
		}
		return allObjs;
	}
	
	public String getSearchString() {
    if (searchField != null)
      return searchField.getText();
    else
      return null;
  }

  public void expandAllRows() {
    int rownumbers = 0;
    int i = 0;
    rownumbers = treeTable.getTree().getRowCount();
    while (i <= rownumbers) {
      treeTable.getTree().expandRow(i++);
      rownumbers = treeTable.getTree().getRowCount();
    }
  }

  public void collapseAllRows() {
    int rownumbers = 0;
    int i = 0;
    rownumbers = treeTable.getTree().getRowCount();
    while (i <= rownumbers) {
      treeTable.getTree().collapseRow(i++);
      rownumbers = treeTable.getTree().getRowCount();
    }
  }

  public SetEqualtoXRDcatD XRDcatDlg = null;
  public SetEqualtoD ParameterDlg = null;

  public void dispose() {
    if (XRDcatDlg != null) {
      XRDcatDlg.setVisible(false);
      XRDcatDlg.dispose();
      XRDcatDlg = null;
    }
    if (ParameterDlg != null) {
      ParameterDlg.setVisible(false);
      ParameterDlg.dispose();
      ParameterDlg = null;
    }
    super.dispose();
  }

  class ChooseAnglesFrame2 extends myJFrame {

    JCheckBox[] anglesCB = null;

    public ChooseAnglesFrame2(Frame aframe) {

      super(aframe);

      ChooseAnglesFrame2.this.setOwnPosition = true;

      Container c1 = ChooseAnglesFrame2.this.getContentPane();

      c1.setLayout(new BorderLayout(Constants.borderInside, Constants.borderInside));
      JPanel principalPanel = new JPanel();
      c1.add(BorderLayout.CENTER, principalPanel);

      principalPanel.setLayout(new FlowLayout(FlowLayout.LEFT, Constants.borderInside, Constants.borderInside));

      JPanel panel3 = new JPanel();
      panel3.setLayout(new GridLayout(0, 1, Constants.borderInside, Constants.borderInside));
      principalPanel.add(panel3);

      panel3.add(new JLabel("Check the angles to be used :"));

      String[] angleNames = {"omega",
                             "chi",
                             "phi",
                             "eta"};

      JPanel jp1;
      anglesCB = new JCheckBox[angleNames.length];
      for (int i = 0; i < angleNames.length; i++) {
        jp1 = new JPanel(new FlowLayout(FlowLayout.LEFT, Constants.borderInside, Constants.borderInside));
        panel3.add(jp1);
        jp1.add(anglesCB[i] = new JCheckBox(angleNames[i]));
        anglesCB[i].setSelected(false);
      }

      panel3 = new JPanel();
      panel3.setLayout(new FlowLayout(FlowLayout.RIGHT, Constants.borderInside, Constants.borderInside));
      c1.add(BorderLayout.SOUTH, panel3);

      JButton stopD = new JCancelButton();
      stopD.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          ChooseAnglesFrame2.this.setVisible(false);
          ChooseAnglesFrame2.this.dispose();
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

          ChooseAnglesFrame2.this.setVisible(false);
          ChooseAnglesFrame2.this.dispose();
        }
      });
      panel3.add(startD);

      getRootPane().setDefaultButton(startD);
//			this.setHelpButton(panel3);

      ChooseAnglesFrame2.this.setTitle("Choose angles");

      ChooseAnglesFrame2.this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);

      ChooseAnglesFrame2.this.pack();
      ChooseAnglesFrame2.this.setResizable(false);

    }
  }


}
