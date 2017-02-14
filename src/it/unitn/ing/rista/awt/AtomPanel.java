/*
 * @(#)AtomPanel.java created Aug 29, 2004 Pergine Valsugana
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.diffr.AtomScatterer;
import it.unitn.ing.rista.diffr.AtomSite;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.models.xyzTableModel;
import it.unitn.ing.rista.interfaces.AtomsStructureI;

import javax.swing.*;
import javax.swing.table.TableModel;
import javax.swing.event.*;
import javax.swing.border.TitledBorder;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;


/**
 * The AtomPanel - the AtomSite List editing panel
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:52 $
 * @since JDK1.1
 */

public class AtomPanel extends JPanel {

  protected int siteselected = -1;
  JList sitelabellist = null;
	JAtomTypeListPane atomLabelList;
  JTextField atomquantity_incell = null, atomquantity = null,
  xcoord = null, ycoord = null, zcoord = null, Bfactor = null;
  JCheckBox quantityFromOccCB = null;
	JCheckBox useUinsteadOfB = null;
  JCheckBox useThisAtomCB = null;
  AtomsStructureI m_Struct;
  myJFrame parentD;

  /**
   * Creates new form atomPanel
   */
  public AtomPanel(myJFrame parentD, AtomsStructureI m_Struct) {
    this.parentD = parentD;
    this.m_Struct = m_Struct;
    initComponents();
    initListeners();
  }

  public void initComponents() {
    JPanel tmpPanel = null, jp1 = null, jPanel16 = null, jPanel13 = null, jPanel18 = null,
        jPanel19 = null, jPanel17 = null, jPanel20 = null;
    JButton addSiteB = null, jb1 = null;

    JPanel borderPanel1 = new JPanel(new FlowLayout());
    add(borderPanel1);
    borderPanel1.setBorder(new TitledBorder(new BevelBorder(BevelBorder.LOWERED), "Atoms"));
    borderPanel1.add(tmpPanel = new JPanel(new BorderLayout()));
    tmpPanel.add(jp1 = new JPanel(new BorderLayout()), BorderLayout.SOUTH);
    jp1.add(jPanel16 = new JPanel(new GridLayout(0, 2)), BorderLayout.CENTER);

    jPanel16.add(addSiteB = new JButton("Add site"));
    addSiteB.setToolTipText("Add a new site to the list");
    addSiteB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        addNewSiteAction();
      }
    });

    jPanel16.add(addSiteB = new JButton("Duplicate"));
    addSiteB.setToolTipText("Duplicate the selected sites in the list");
    addSiteB.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        duplicateAtoms();
      }
    });

    final JRemoveButton button2 = new JRemoveButton();
    button2.setToolTipText("Remove the selected sites from the list");
    jPanel16.add(button2);
    button2.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        if (!Constants.confirmation || Utility.areYouSureToRemove("Remove the selected sites?"))
          removeSiteAction();
      }
    });
	  jPanel16.add(jb1 = new JIconButton("NewSheet.gif", "Positions"));
	  jb1.setToolTipText("Shows a list of the equivalent positions for this site");
	  jb1.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent event) {
			  show_xyz();
		  }
	  });

    JPanel jp3 = new JPanel(new FlowLayout());
    jp1.add(jp3, BorderLayout.SOUTH);
    jp3.add(jPanel16 = new JPanel(new GridLayout(0, 1, 3, 3)));
	  jPanel16.add(useUinsteadOfB = new JCheckBox("Use U instead of B for thermal factors"));
	  useUinsteadOfB.setToolTipText("Select this to use Biso factor instead of dimensionless Uiso (also for anisotropic)");
	  useUinsteadOfB.setSelected(m_Struct.isDebyeWallerModelDimensionLess());
	  useUinsteadOfB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  m_Struct.setDebyeWallerModelDimensionLess(useUinsteadOfB.isSelected());
		  }
	  });
	  jPanel16.add(quantityFromOccCB = new JCheckBox("Compute quantity from occupancy"));
	  quantityFromOccCB.setToolTipText("Uncheck this to compute occupancy from quantity");
	  quantityFromOccCB.setSelected(m_Struct.getQuantityFromOccupancy());
	  quantityFromOccCB.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  m_Struct.setQuantityFromOccupancy(quantityFromOccCB.isSelected());
		  }
	  });
	  jPanel16.add(useThisAtomCB = new JCheckBox("Use it in the computation"));
	  useThisAtomCB.setToolTipText("Uncheck this to set this atom as dummy");

	  sitelabellist = new JList();
	  sitelabellist.setVisibleRowCount(6);
	  sitelabellist.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
	  JScrollPane sp = new JScrollPane();
//		sp.setBorder(new LineBorder(Color.black));
	  sp.getViewport().add(sitelabellist);
	  tmpPanel.add(sp, BorderLayout.CENTER);

	  tmpPanel.add(jPanel13 = new JPanel(new BorderLayout(0, 0)), BorderLayout.NORTH);
	  jPanel13.add(new JLabel("Atom site list:"), BorderLayout.WEST);

	  borderPanel1.add(jPanel20 = new JPanel(new BorderLayout()));

	  atomLabelList = new JAtomTypeListPane(parentD, false);
	  atomLabelList.setBorder(new TitledBorder(
			  new BevelBorder(BevelBorder.LOWERED), "Atom types in the site:"));

	  jPanel20.add(atomLabelList, BorderLayout.NORTH);

	  jPanel20.add(jPanel19 = new JPanel(new BorderLayout()), BorderLayout.CENTER);

    jPanel19.add(jPanel17 = new JPanel(new GridLayout(0, 1, 3, 3)), BorderLayout.WEST);
    jPanel17.add(new JLabel(" Total quantity:"));
    jPanel17.add(new JLabel("Total occupancy:"));
    jPanel17.add(new JLabel("              x:"));
    jPanel17.add(new JLabel("              y:"));
    jPanel17.add(new JLabel("              z:"));
	  String Bstring =       "    Biso factor:";
	  if (m_Struct.isDebyeWallerModelDimensionLess())
		  Bstring =           "    Uiso factor:";
    jPanel17.add(new JLabel(Bstring));

    jPanel19.add(jPanel18 = new JPanel(new GridLayout(0, 1, 3, 3)), BorderLayout.CENTER);
    jPanel18.add(atomquantity_incell = new JTextField("1", 6));
    atomquantity_incell.setToolTipText("Set the number of atoms in the cell");
    jPanel18.add(atomquantity = new JTextField("1", 6));
    jPanel18.add(xcoord = new JTextField("0", Constants.FLOAT_FIELD));
    jPanel18.add(ycoord = new JTextField("0", Constants.FLOAT_FIELD));
    jPanel18.add(zcoord = new JTextField("0", Constants.FLOAT_FIELD));
    jPanel18.add(Bfactor = new JTextField("0", Constants.FLOAT_FIELD));

    initAtomList();

  }

	public void initListeners() {
    sitelabellist.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent event) {
        sitelabellist_ListSelect();
      }
    });
    if (m_Struct.getAtomList().size() > 0)
      siteselected = 0;
  }

  void retrieveAtom() {
    if (siteselected >= 0) {
      AtomSite anatom = getSelectedSite();
      if (anatom != null) {
	      atomLabelList.retrieveparlist();
        anatom.setQuantity(atomquantity_incell.getText());
        anatom.getOccupancy().setValue(atomquantity.getText());
        anatom.getBfactor().setValue(Bfactor.getText());
        anatom.getLocalCoordX().setValue(xcoord.getText());
        anatom.getLocalCoordY().setValue(ycoord.getText());
        anatom.getLocalCoordZ().setValue(zcoord.getText());
        anatom.setDummy(!useThisAtomCB.isSelected());
      }
      anatom.refreshPositions(false);
      anatom.refreshOccupancyAndQuantity();
    }
  }

  void retrieveParameters() {
    retrieveAtom();
  }

  public void setAtomComponent(AtomSite anatom) {
    if (anatom != null) {
	    String labels[] = {"Partial occupancy: "};
	    atomLabelList.setList(anatom, 0, labels.length, labels);
	    parentD.addComponenttolist(atomquantity, anatom.getOccupancy());
      parentD.addComponenttolist(xcoord, anatom.getLocalCoordX());
      parentD.addComponenttolist(ycoord, anatom.getLocalCoordY());
      parentD.addComponenttolist(zcoord, anatom.getLocalCoordZ());
      parentD.addComponenttolist(Bfactor, anatom.getBfactor());
    } else {
      parentD.removeComponentfromlist(atomquantity);
      parentD.removeComponentfromlist(xcoord);
      parentD.removeComponentfromlist(ycoord);
      parentD.removeComponentfromlist(zcoord);
      parentD.removeComponentfromlist(Bfactor);
	    atomquantity.setText("0");
      xcoord.setText("0");
      ycoord.setText("0");
      zcoord.setText("0");
      Bfactor.setText("0");
    }
  }

  public AtomSite getSelectedAtom() {
    return (AtomSite) m_Struct.getAtomList().selectedElement();
  }

  public AtomSite getSelectedSite() {
//		return getSelectedAtom();
    if (siteselected >= 0 && siteselected < m_Struct.getAtomList().size())
      return (AtomSite) m_Struct.getAtomList().elementAt(siteselected);
    else
      return null;
  }

  public void initAtomList() {
    int sitenumb = m_Struct.getAtomList().setList(sitelabellist);
//		atomnumber = loadatomtable();
//		totalatomnumber.setText(String.valueOf(sitenumb));
    if (sitenumb > 0)
      setatomsite(0);
  }

  public void setatomsite() {
    AtomSite anatom = getSelectedAtom();
    if (anatom != null) {
      anatom.refreshPositions(false);
      anatom.refreshOccupancyAndQuantity();
      atomquantity_incell.setText(anatom.getQuantity());
      atomquantity.setText(anatom.getOccupancy().getValue());
      xcoord.setText(anatom.getLocalCoordX().getValue());
      ycoord.setText(anatom.getLocalCoordY().getValue());
      zcoord.setText(anatom.getLocalCoordZ().getValue());
      Bfactor.setText(anatom.getBfactor().getValue());
      useThisAtomCB.setSelected(!anatom.isDummyAtom());
    }
    setAtomComponent(anatom);
  }

  public void setatomsite(int numb) {
    sitelabellist.setSelectedIndex(numb);
    setatomsite();
  }

/*    public String gettheLabel() {
      return "Site label:";
    }*/

  void addNewSiteAction() {
    // add a new atom site
    retrieveAtom();
    siteselected = -1;
    m_Struct.addAtom();
    setAtomComponent(null);

//			update3dStructure();

//		int sitenumb = thephase.getAtomNumber();
//		totalatomnumber.setText(String.valueOf(sitenumb));
//		setatomsite(sitenumb-1);
  }

  void duplicateAtoms() {
    AtomSite[] selAtom = getSelectedAtoms();
    if (selAtom != null) {
      siteselected = -1;
      setAtomComponent(null);
      for (int i = 0; i < selAtom.length; i++) {
        AtomSite newAtom = (AtomSite) selAtom[i].getCopy(selAtom[i].getParent());
        newAtom.setLabel(selAtom[i].getLabel() + " copy");
        newAtom.setParent(selAtom[i].getParent());
        m_Struct.addAtom(newAtom);
      }
    }
  }

  public AtomSite[] getSelectedAtoms() {
    int[] selAtom = sitelabellist.getSelectedIndices();
    AtomSite[] atomlist = null;
    if (selAtom != null) {
      atomlist = new AtomSite[selAtom.length];
      for (int i = 0; i < selAtom.length; i++) {
        atomlist[i] = m_Struct.getAtom(selAtom[i]);
      }
    }
    return atomlist;
  }

  public void removeSiteAction() {
    // remove selected atom
    int[] selAtom = sitelabellist.getSelectedIndices();
    if (selAtom != null) {
      siteselected = -1;
      setAtomComponent(null);
      for (int i = selAtom.length - 1; i >= 0; i--)
        m_Struct.removeAtomAt(selAtom[i]);
    }
  }

  void sitelabellist_ListSelect() {
    retrieveAtom();
    if (sitelabellist != null) {
      siteselected = sitelabellist.getSelectedIndex();
      setatomsite();
    }
  }

/*    void sitelabellist_DblClicked() {
      final LabelD labD = new LabelD(parentD, "Edit site label", true);
      labD.setTextField(getField());
      labD.jbok.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setField(labD.getTextField());
          labD.dispose();
        }
      });
    }

    public void setField(String label) {
      m_Struct.getAtomList().setLabelAt(label, sitelabellist.getSelectedIndex());
    }

    public String getField() {
      return sitelabellist.getSelectedValue().toXRDcatString();
    } */

  public void show_xyz() {
    AtomSite anatom = getSelectedAtom();
    if (anatom != null) {
      final myJFrame jf = new myJFrame(parentD, "xyz list of: " + anatom.toXRDcatString());
      jf.createDefaultMenuBar();
      TableModel xyzModel = new xyzTableModel(anatom);
      JTable xyztable = new JTable(xyzModel);
      JScrollPane scrollpane = new JScrollPane(xyztable);
//			scrollpane.setBorder(new LineBorder(Color.black));
      Container c1 = jf.getContentPane();
      c1.setLayout(new BorderLayout());
      c1.add(scrollpane, BorderLayout.CENTER);
      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      JCloseButton jb = new JCloseButton();
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          jf.setVisible(false);
          jf.dispose();
        }
      });
      jp.add(jb);
      c1.add(jp, BorderLayout.SOUTH);
      getRootPane().setDefaultButton(jb);
      jf.setSize(300, 250);
      jf.setVisible(true);
    } else
      System.out.println("No atom or site selected");
  }

	public void showInfoPanel() {
		AtomSite atomSite = getSelectedAtom();
		if (atomSite == null)
			return;
		AtomScatterer atomScat = (AtomScatterer) atomSite.subordinateloopField[AtomSite.scattererLoopID].selectedElement();
		if (atomScat == null)
			return;
		int atomicNumber = atomScat.getAtomicNumber();

		int plotCounts = 3000;
		double[] x = new double[plotCounts];
		double[] y = new double[plotCounts];
		double xstart = 1.01;
		double xstep = 0.01;
		for (int i = 0; i < plotCounts; i++) {
			x[i] = xstart + i * xstep;
			y[i] = XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(atomicNumber, x[i]);
//			x[i] = MoreMath.log10(x[i]);
			if (y[i] > 0)
				y[i] = MoreMath.log10(y[i]);
			else
				y[i] = 0;
		}
		(new PlotSimpleData(this.parentD, x, y, false)).setVisible(true);

	}

  public void dispose() {
    sitelabellist = null;
	  atomLabelList = null;
    siteselected = -1;
  }

}

