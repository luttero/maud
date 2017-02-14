/*
* @(#)ForceFieldSimpleRepulsion.java created 18/10/2002 Mesiano
*
* Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.forcefield;

import it.unitn.ing.rista.awt.JOptionsDialog;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.structure.StructureAtomic;
import it.unitn.ing.rista.util.Coordinates;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.chemistry.AtomInfo;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.*;
import java.io.*;


/**
 *  The ForceFieldSimpleRepulsion determines potential energy based on a electrostatic force field
 *
 *
 * @version $Revision: 1.11 $, $Date: 2006/01/19 14:45:56 $
 * @author Mauro Bortolotti
 * @since JDK1.1
 */


public class ForceFieldSimpleRepulsion extends ForceField {

	protected static String[] diclistc = {
      "_geom_bond_distance_max",
		"_geom_bond_distance", "_geom_bond_angle"
	};
  protected static String[] diclistcrm = {
      "Maximum distance between atoms",
    "Minimum bond distance (Angstrom)", "Minimum bond angle (degrees)"
  };

	protected static String[] classlistc = {};

	protected static String[] classlistcs = {};

  private Vector anglesVector = null;

  protected double maxDistance = 2.0;

	public ForceFieldSimpleRepulsion(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		initXRD();
		identifier = "Simple Repulsion Force Field";
		IDlabel = "Simple Repulsion Force Field";
		description = "Simple Repulsion Force Field";
	}

	public ForceFieldSimpleRepulsion(XRDcat aobj) {
		this(aobj, "Simple Repulsion Force Field");
	}

	public ForceFieldSimpleRepulsion() {
		identifier = "Simple Repulsion Force Field";
		IDlabel = "Simple Repulsion Force Field";
		description = "Simple Repulsion Force Field";
	}

	public void initConstant() {
		Nstring = 1;
		Nstringloop = 2;
		Nparameter = 0;
		Nparameterloop = 0;
		Nsubordinate = 0;
		Nsubordinateloop = 0;
	}

	public void initDictionary() {
		for (int i = 0; i < totsubordinateloop; i++)
			diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
		for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
			classlist[i] = classlistc[i];
		for (int i = 0; i < totsubordinate - totparameterloop; i++)
			classlists[i] = classlistcs[i];
	}

	public void initParameters() {
		super.initParameters();
    setMaximumDistance(MaudPreferences.getPref("angle&BondRestraint.maximumDistance", "2.0"));
	}

  public void setMaximumDistance(String s) {
    stringField[0] = s;
  }

  public String getMaximumDistance() {
    return stringField[0];
  }

  public double getMaxDistance() {
    maxDistance = Double.parseDouble(getMaximumDistance());
    return maxDistance;
  }

  private boolean refreshAtoms = true;

	public RepulsionDistance getRepulsionDistance(int index) {
		if ((index < 0) || (index > stringloopField[0].size()))
			return null;
		String rij_str = (String) stringloopField[0].elementAt(index);
		return string2RepulsionDist(rij_str);
	}

	public RepulsionDistance getRepulsionDistance(String a_symb1, String a_symb2) {
		RepulsionDistance r12 = new RepulsionDistance(a_symb1, a_symb2);
		for (int nr = 0; nr < stringloopField[0].size(); nr++) {
			RepulsionDistance rij = getRepulsionDistance(nr);
			if ((rij != null) && (r12.equals(rij)))
				return rij;
		}
		return null;
	}

	public double getRepulsionRadius(String a_symb1, String a_symb2) {
		RepulsionDistance r12 = getRepulsionDistance(a_symb1, a_symb2);
		if (r12 == null)
			return AtomInfo.retrieveAtomRadius(a_symb1) + AtomInfo.retrieveAtomRadius(a_symb2);;
		return r12.dist;
	}

	public void setRepulsionDistance(RepulsionDistance r12, int index) {
		if ((index < 0) || (index > getPairNumber()))
			return;
		stringloopField[0].setItemAt(RepulsionDist2string(r12), index);
	}

	public void setRepulsionDistance(String atm1, String atm2, double dist) {

		RepulsionDistance r12 = new RepulsionDistance(atm1, atm2, dist);
		//System.out.println("setRepulsionDistance(String atm1, String atm2, double dist)" + r12.dist);
		for (int nd = 0; nd < getPairNumber(); nd++) {
			RepulsionDistance rij = string2RepulsionDist((String)stringloopField[0].elementAt(nd));
			if (r12.equals(rij))
				setRepulsionDistance(r12, nd);
		}
	}

	public int getPairNumber() {
		return stringloopField[0].size();
	}

	public RepulsionDistance string2RepulsionDist(String str) {
		StringTokenizer st_tkn = new StringTokenizer(str);
		String atm1 = st_tkn.nextToken();
		String atm2 = st_tkn.nextToken();
		double dist = Double.parseDouble(st_tkn.nextToken());
		return new RepulsionDistance(atm1, atm2, dist);
	}

	public String RepulsionDist2string(RepulsionDistance dst) {
		return dst.atm1 + " " + dst.atm2 + " " + String.valueOf(dst.dist);
	}

  public AtomSite getAtom(String label) {
    StructureAtomic m_Struct = getParentStructure();
    Phase aphase = m_Struct.getPhaseParent();
    Vector atoms = aphase.getFullAtomList();

    int na = atoms.size();
    for (int i = 0; i < na; i++) {
      AtomSite atom1 = (AtomSite) atoms.get(i);
      if (atom1.getLabel().equalsIgnoreCase(label))
        return atom1;
    }
    return null;
  }

	public void refreshAtomPairs() {
		StructureAtomic m_Struct = getParentStructure();
    Phase aphase = m_Struct.getPhaseParent();
    Vector atoms = aphase.getFullAtomList();

		ArrayList atomPairs = new ArrayList();
		int na = atoms.size();
		for (int i = 0; i < na; i++) {
      AtomSite atm1 = ((AtomSite) atoms.get(i));
      atm1.computeCartesianCoords(false);
			for (int j = 0; j < na; j++) {
				String atom1 = atm1.getFirstAtomSymbol();
				String atom2 = ((AtomSite) atoms.get(j)).getFirstAtomSymbol();
				RepulsionDistance rij_tmp = new RepulsionDistance(atom1, atom2);
				if (!atomPairs.contains(rij_tmp)) {
					atomPairs.add(rij_tmp);
				}
			}
		}

		int np_new, np_old;

		//check for removed atompairs in Structure

		for (np_old = 0; np_old < stringloopField[0].size(); np_old++) {
			boolean removed = true;
			String str_tmp = (String) stringloopField[0].elementAt(np_old);
			for (np_new = 0; np_new < atomPairs.size(); np_new++) {
				RepulsionDistance rij_tmp = (RepulsionDistance) atomPairs.get(np_new);
				if (rij_tmp.equals(string2RepulsionDist(str_tmp)))
					removed = false;
			}
			if (removed)
				stringloopField[0].removeItemAt(np_old);
		}

		//check for added atompairs in Structure

		for (np_new = 0; np_new < atomPairs.size(); np_new++) {
			boolean newpair = true;
			RepulsionDistance rij_tmp = (RepulsionDistance) atomPairs.get(np_new);
			for (np_old = 0; np_old < stringloopField[0].size(); np_old++) {
				String str_tmp = (String) stringloopField[0].elementAt(np_old);
				if (rij_tmp.equals(string2RepulsionDist(str_tmp)))
					newpair = false;
			}
			if (newpair)
				stringloopField[0].addItem(RepulsionDist2string(rij_tmp));
		}
		refreshAtoms = false;

// Angles
    if (anglesVector == null)
      updateVectors();

    ArrayList atomT = new ArrayList();
    na = atoms.size();
    for (int i = 0; i < na; i++) {
      AtomSite atom1 = (AtomSite) atoms.get(i);
      if (!atom1.isDummyAtom()) {
        for (int k = 0; k < na; k++) {
          AtomSite atom3 = (AtomSite) atoms.get(k);
          if (!atom3.isDummyAtom()) {
            for (int j = k + 1; j < na; j++) {
              if (i != j && i != k) {
                AtomSite atom2 = (AtomSite) atoms.get(j);
                if (!atom2.isDummyAtom()) {
                  double dist1 = DistanceAngleBondRestraints.getDistance(atom1, atom2);
                  double dist2 = DistanceAngleBondRestraints.getDistance(atom1, atom3);
//                  System.out.println(atom1.getLabel() + " " + atom2.getLabel() + " " + atom3.getLabel());
                  if (dist1 < maxDistance && dist2 < maxDistance) {
                    OptimumAngle rij_tmp = new OptimumAngle(atom1, atom2, atom3);
                    System.out.println(atom1.getLabel() + " " + atom2.getLabel() + " " + atom3.getLabel());
                    atomT.add(rij_tmp);
                  }
                }
              }
            }
          }
        }
      }
    }

    //check for removed atompairs in Structure

    for (np_old = 0; np_old < getAnglesNumber(); np_old++) {
      boolean removed = true;
      OptimumAngle rij = (OptimumAngle) getAngles().elementAt(np_old);
      for (np_new = 0; np_new < atomT.size(); np_new++) {
        OptimumAngle rij_tmp = (OptimumAngle) atomT.get(np_new);
        if (rij.equals(rij_tmp)) {
          removed = false;
          break;
        }
      }
      if (removed) {
        getAngles().removeElementAt(np_old);
        np_old--;
      }
    }

    //check for added atompairs in Structure

    for (np_new = 0; np_new < atomT.size(); np_new++) {
      boolean newpair = true;
      OptimumAngle rij_tmp = (OptimumAngle) atomT.get(np_new);
      for (np_old = 0; np_old < getAnglesNumber(); np_old++) {
        if (rij_tmp.equals(getAngles().elementAt(np_old))) {
          newpair = false;
          break;
        }
      }
      if (newpair)
        getAngles().addElement(rij_tmp);
    }

    refreshComputation = false;
	}

  public void refreshAllVectors() {

    if (anglesVector == null)
      updateVectors();
  }

  private void updateVectors() {
    anglesVector = new Vector(0, 100);
    for (int i = 0; i < stringloopField[1].size(); i++)
      anglesVector.addElement(string2OptimumAngle((String) stringloopField[1].elementAt(i)));
  }

  private void updateLoopFields() {
    if (anglesVector != null) {
      stringloopField[1].removeAllItems();
      for (int i = 0; i < anglesVector.size(); i++)
        stringloopField[1].addItem(OptimumAngle2string((OptimumAngle) anglesVector.elementAt(i)));
    }
  }

  public void copyCat(XRDcat thecopy) {
    updateLoopFields();
    super.copyCat(thecopy);
  }

  public void writeAllLoopFields(BufferedWriter out) {
    updateLoopFields();
    super.writeAllLoopFields(out);
  }

  public void printStringLoopInformations(OutputStream out) throws IOException {
    updateLoopFields();
    super.printStringLoopInformations(out);
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
    updateLoopFields();
  }

  public Vector getAngles() {
    return anglesVector; // stringloopField[1];
  }

  public int getAnglesNumber() {
    return getAngles().size();
  }

  public OptimumAngle getOptimumAngle(int index) {
    if ((index < 0) || (index >= getAnglesNumber()))
      return null;
    return (OptimumAngle) getAngles().elementAt(index);
  }

  public void setOptimumAngle(OptimumAngle r12, int index) {
    if ((index < 0) || (index >= getAnglesNumber()))
      return;
    getAngles().setElementAt(r12, index);
  }

  public void setOptimumAngle(int index, AtomSite atm1, AtomSite atm2, AtomSite atm3, double dist, double kappa) {
    OptimumAngle r12 = new OptimumAngle(atm1, atm2, atm3, dist, kappa);
    OptimumAngle rij = (OptimumAngle) getAngles().elementAt(index);
    if (r12.equals(rij))
      setOptimumAngle(r12, index);
  }

  public OptimumAngle string2OptimumAngle(String str) {
    StringTokenizer st_tkn = new StringTokenizer(str);
    String atm1 = st_tkn.nextToken();
    String atm2 = st_tkn.nextToken();
    String atm3 = st_tkn.nextToken();
    double dist = Double.parseDouble(st_tkn.nextToken());
    double kappa = Double.parseDouble(st_tkn.nextToken());
    return new OptimumAngle(getAtom(atm1), getAtom(atm2), getAtom(atm3), dist, kappa);
  }

  public String OptimumAngle2string(OptimumAngle dst) {
    return new StringBuffer().append(dst.atm1.getLabel()).append(" ").append(dst.atm2.getLabel()).append(" ")
        .append(dst.atm3.getLabel()).append(" ").append(String.valueOf(dst.angle))
        .append(" ").append(String.valueOf(dst.kappa)).toString();
  }

	public double computeEnergy() {
		if (refreshAtoms) refreshAtomPairs();
    StructureAtomic m_Struct = getParentStructure();
    Phase aphase = m_Struct.getPhaseParent();
    Vector atoms = aphase.getFullAtomList();
		double PEnergy = 0.0;
		for (int na1 = 0; na1 < atoms.size(); na1++) {
      AtomSite atm1 = (AtomSite) atoms.get(na1);
      if (!atm1.isDummyAtom()) {
      for (int na2 = 0; na2 < atoms.size(); na2++) {
				if (na1 != na2) {
				AtomSite atm2 = (AtomSite) atoms.get(na2);
        if (!atm2.isDummyAtom()) {
				for (int np1 = 0; np1 < atm1.getCartesianCoords().size(); np1++) {
					for (int np2 = 0; np2 < atm2.getCartesianCoords().size(); np2++) {
						Coordinates acoord1 = atm1.getCartesianCoords(np1);
						Coordinates acoord2 = atm2.getCartesianCoords(np2);
						double rmin = getRepulsionRadius(atm1.getFirstAtomSymbol(), atm2.getFirstAtomSymbol());
						double rij = Math.sqrt(Math.pow(acoord1.x - acoord2.x, 2) + Math.pow(acoord1.y - acoord2.y, 2) +
                Math.pow(acoord1.z - acoord2.z, 2));

						if (rij < rmin && (atm1.getOccupancyValue() + atm2.getOccupancyValue()) > 1.0) {
              double E = Math.pow(rmin / rij, 2);
//              System.out.println("Energy: " + atm1.getLabel() + " " + atm2.getLabel() + " " +
//                          rmin + " " + rij + " " + E);
              PEnergy += E;
            }
					}
				}
        }
        }
      }
      }
    }
    for (int nd = 0; nd < getAnglesNumber(); nd++) {
      OptimumAngle rij = (OptimumAngle) getAngles().elementAt(nd);
      if (rij.kappa > 0 && rij.angle != 0) {
        double actualAngle = DistanceAngleBondRestraints.getAngle(rij.atm1, rij.atm2, rij.atm3);
        if (rij.angle > actualAngle)
          PEnergy += Math.pow((rij.angle - actualAngle) / rij.angle, 2) * rij.kappa;
      }
    }
		return PEnergy; //Math.log(PEnergy + 1);
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JSRFFOptionsD(parent, this);
		return adialog;
	}

	public class JSRFFOptionsD extends JOptionsDialog {

    private JTable distTable;
    private JTable angTable;
    String[] columnNames;
    Object[][] data;
    String[] angleColumnNames;
    Object[][] angleData;
    JTextField maxDistTF;

    public JSRFFOptionsD(Frame parent, XRDcat obj) {

      super("OK", parent, obj);
      refreshAtomPairs();

      principalPanel.setLayout(new BorderLayout(3, 3));

      JPanel tablePanel = new JPanel();
      tablePanel.setLayout(new BorderLayout(6, 6));

      distTable = new JTable(new srTableModel());
      JScrollPane tablescrollPane = new JScrollPane(distTable);
      distTable.setPreferredScrollableViewportSize(new Dimension(350, 400));

      tablePanel.add(BorderLayout.CENTER, tablescrollPane);

      angTable = new JTable(new anTableModel());
      JScrollPane tablescrollPaneA = new JScrollPane(angTable);
      angTable.setPreferredScrollableViewportSize(new Dimension(400, 400));

      tablePanel.add(BorderLayout.EAST, tablescrollPaneA);

      principalPanel.add(BorderLayout.CENTER, tablePanel);

      JPanel topPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 3, 3));

      principalPanel.add(BorderLayout.NORTH, topPanel);

      maxDistTF = new JTextField(8);
      topPanel.add(new JLabel("   Max bond length to consider: "));
      topPanel.add(maxDistTF);

      JButton jb;
      topPanel.add(jb = new JButton("Save changes"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          retrieveParameters();
        }
      });
      jb.setToolTipText("Save all modifications done. Use it before refresh.");

      topPanel.add(jb = new JButton("Refresh atoms"));
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setMaximumDistance(maxDistTF.getText());
          refreshAllVectors();
          distTable.setModel(new srTableModel());
          angTable.setModel(new anTableModel());
        }
      });
      jb.setToolTipText("Refresh for new or removed atoms. Some restraints may disappears!");

      setTitle("Simple Repulsion Force Field");
//      setHelpFilename("DistanceAngleBondRestraints.txt");

      initParameters();
      pack();
    }

    public void initParameters() {
      super.initParameters();
      maxDistTF.setText(getMaximumDistance());
    }

    public void retrieveParameters() {
//      stringloopField[0].removeAllItems();
      for (int nd = 0; nd < data.length; nd++) {
        String atm1 = (String) data[nd][0];
        String atm2 = (String) data[nd][1];
        Double dist = (Double) data[nd][2];
        setRepulsionDistance(atm1, atm2, dist.doubleValue());
      }
      for (int nd = 0; nd < angleData.length; nd++) {
        AtomSite atm1 = (AtomSite) angleData[nd][0];
        AtomSite atm2 = (AtomSite) angleData[nd][1];
        AtomSite atm3 = (AtomSite) angleData[nd][2];
        Double dist = (Double) angleData[nd][4];
        Double kappa = (Double) angleData[nd][5];
        setOptimumAngle(nd, atm1, atm2, atm3, dist.doubleValue(), kappa.doubleValue());
      }
      setMaximumDistance(maxDistTF.getText());
    }

    class srTableModel extends AbstractTableModel {

      public srTableModel() {
        columnNames = new String[]{"AtomSite 1", "AtomSite 2", "Min. Distance"};
        data = new Object[getPairNumber()][3];
        for (int nd = 0; nd < getPairNumber(); nd++) {
          RepulsionDistance r12 = getRepulsionDistance(nd);
          data[nd][0] = r12.atm1;
          data[nd][1] = r12.atm2;
          data[nd][2] = new Double(r12.dist);
        }
      }

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return data.length;
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
        return data[row][col];
      }

      public Class getColumnClass(int c) {
        return getValueAt(0, c).getClass();
      }

      public boolean isCellEditable(int row, int col) {
        if (col < 2) {
          return false;
        } else {
          return true;
        }
      }

      public void setValueAt(Object value, int row, int col) {
        data[row][col] = value;
        fireTableCellUpdated(row, col);
      }
    }
    class anTableModel extends AbstractTableModel {

      public anTableModel() {
        angleColumnNames = new String[]{"Pivot atom", "AtomSite 1", "AtomSite 2", "Act. Angle", "Min. Angle", "Strength"};
        angleData = new Object[getAnglesNumber()][6];
        for (int nd = 0; nd < getAnglesNumber(); nd++) {
          OptimumAngle r12 = getOptimumAngle(nd);
          angleData[nd][0] = r12.atm1;
          angleData[nd][1] = r12.atm2;
          angleData[nd][2] = r12.atm3;
          angleData[nd][3] = DistanceAngleBondRestraints.getAngle(r12.atm1, r12.atm2, r12.atm3);
          angleData[nd][4] = new Double(r12.angle);
          angleData[nd][5] = new Double(r12.kappa);
        }
      }

      public int getColumnCount() {
        return angleColumnNames.length;
      }

      public int getRowCount() {
        return angleData.length;
      }

      public String getColumnName(int col) {
        return angleColumnNames[col];
      }

      public Object getValueAt(int row, int col) {
        return angleData[row][col];
      }

      public Class getColumnClass(int c) {
        return getValueAt(0, c).getClass();
      }

      public boolean isCellEditable(int row, int col) {
        if (col < 4) {
          return false;
        } else {
          return true;
        }
      }

      public void setValueAt(Object value, int row, int col) {
        angleData[row][col] = value;
        fireTableCellUpdated(row, col);
      }
    }
  }

/*


		private JTable distTable;
		String[] columnNames;
		Object[][] data;

		public JSRFFOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);
			refreshAtomPairs();

      principalPanel.setLayout(new BorderLayout(3, 3));

      distTable = new JTable(new srTableModel());
			JScrollPane tablescrollPane = new JScrollPane(distTable);
			distTable.setPreferredScrollableViewportSize(new Dimension(400, 200));

			principalPanel.add(BorderLayout.CENTER, tablescrollPane);


			setTitle("Simple Repulsion Force Field");
			initParameters();
			pack();
		}

		public void initParameters() {
			super.initParameters();
		}

		public void retrieveParameters() {
      for (int nd = 0; nd < data.length; nd++) {
        String atm1 = (String) data[nd][0];
        String atm2 = (String) data[nd][1];
        Double dist = (Double) data[nd][2];
        setRepulsionDistance(atm1, atm2, dist.doubleValue());
      }
		}

		class srTableModel extends AbstractTableModel {

			public srTableModel() {
				columnNames = new String[]{"AtomSite 1", "AtomSite 2", "Min. Distance"};
				data = new Object[getPairNumber()][3];
				for (int nd = 0; nd < getPairNumber(); nd++) {
					RepulsionDistance r12 = getRepulsionDistance(nd);
					data[nd][0] = r12.atm1;
					data[nd][1] = r12.atm2;
					data[nd][2] = new Double(r12.dist);
				}
			}

			public int getColumnCount() {
				return columnNames.length;
			}

			public int getRowCount() {
				return data.length;
			}

			public String getColumnName(int col) {
				return columnNames[col];
			}

			public Object getValueAt(int row, int col) {
				return data[row][col];
			}

			public Class getColumnClass(int c) {
				return getValueAt(0, c).getClass();
			}

			public boolean isCellEditable(int row, int col) {
				if (col < 2) {
					return false;
				} else {
					return true;
				}
			}

			public void setValueAt(Object value, int row, int col) {
				data[row][col] = value;
				fireTableCellUpdated(row, col);
			}
		}
	}
	*/

	public class RepulsionDistance {
		public String atm1;
		public String atm2;
		public double dist;

		public RepulsionDistance() {
		}

		public RepulsionDistance(String a1, String a2) {
			atm1 = a1;
			atm2 = a2;
			dist = AtomInfo.retrieveAtomRadius(a1) + AtomInfo.retrieveAtomRadius(a2);
		}

		public RepulsionDistance(String a1, String a2, double d) {
			atm1 = a1;
			atm2 = a2;
			dist = d;
		}

		public boolean equals(Object o) {
			RepulsionDistance r12 = (RepulsionDistance) o;
			if (((atm1.equals(r12.atm1)) && (atm2.equals(r12.atm2))) || ((atm1.equals(r12.atm2)) && (atm2.equals(r12.atm1))))
				return true;
			return false;
		}

		public int hashCode() {
			return (int) dist;
		}
	}

  public class OptimumAngle {
    public AtomSite atm1;
    public AtomSite atm2;
    public AtomSite atm3;
    public double angle;
    public double kappa;

    public OptimumAngle() {
    }

    public OptimumAngle(AtomSite a1, AtomSite a2, AtomSite a3) {
      atm1 = a1;
      atm2 = a2;
      atm3 = a3;
      angle = DistanceAngleBondRestraints.getAngle(a1, a2, a3);
      kappa = 0.0;
    }

    public OptimumAngle(AtomSite a1, AtomSite a2, AtomSite a3, double d) {
      atm1 = a1;
      atm2 = a2;
      atm3 = a3;
      angle = d;
      kappa = 0.0;
    }

    public OptimumAngle(AtomSite a1, AtomSite a2, AtomSite a3, double d, double kappaSpring) {
      atm1 = a1;
      atm2 = a2;
      atm3 = a3;
      angle = d;
      kappa = kappaSpring;
    }

    public boolean equals(Object o) {
      OptimumAngle r12 = (OptimumAngle) o;
      if (((atm2.equalsByDistance(r12.atm2) && atm3.equalsByDistance(r12.atm3)) ||
          (atm2.equalsByDistance(r12.atm3) && atm3.equalsByDistance(r12.atm2))) && (atm1.equalsByDistance(r12.atm1)))
        return true;
      return false;
    }

  }

}
