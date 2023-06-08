/*
* @(#)ForceFieldLennardJones.java created 18/10/2002 Mesiano
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
import it.unitn.ing.rista.diffr.AtomSite;
import it.unitn.ing.rista.diffr.structure.StructureAtomic;
import it.unitn.ing.rista.util.Coordinates;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.awt.*;
import java.util.ArrayList;
import java.util.StringTokenizer;


/**
 *  The ForceFieldLennardJones determines potential energy based on a Lennard-Jones force field
 *
 *
 * @version $Revision: 1.11 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class ForceFieldLennardJones extends ForceField {

	protected static String[] diclistc = {
		"_force_field_lennardjones_params"
	};
  protected static String[] diclistcrm = {
    "_force_field_lennardjones_params"
  };

	protected static String[] classlistc = {};

	protected static String[] classlistcs = {};

	public ForceFieldLennardJones(XRDcat aobj, String alabel) {
		super(aobj, alabel);
		identifier = "Lennard-Jones Force Field";
		IDlabel = "Lennard-Jones Force Field";
		description = "Lennard-Jones Force Field";
		initBaseObject();
	}

	public ForceFieldLennardJones(XRDcat aobj) {
		this(aobj, "Lennard-Jones Force Field");
	}

	public ForceFieldLennardJones() {
		identifier = "Lennard-Jones Force Field";
		IDlabel = "Lennard-Jones Force Field";
		description = "Lennard-Jones Force Field";
	}

	public void initConstant() {
		Nstring = 0;
		Nstringloop = 1;
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
	}

  private boolean refreshAtoms = true;
	static final double defaultC1 = 2.0;
	static final double defaultC2 = 2.0;


	public LennardJonesParams getLennardJonesParams(int index) {
		if ((index < 0) || (index > stringloopField[0].size()))
			return null;
		String rij_str = (String) stringloopField[0].elementAt(index);
		return string2LJParams(rij_str);
	}

	public LennardJonesParams getLennardJonesParams(String a_symb1, String a_symb2) {
		LennardJonesParams p12 = new LennardJonesParams(a_symb1, a_symb2);
		for (int np = 0; np < stringloopField[0].size(); np++) {
			LennardJonesParams pij = getLennardJonesParams(np);
			if ((pij != null) && (p12.equals(pij)))
				return pij;
		}
		return null;
	}

	public double getLennardJonesConst1(String a_symb1, String a_symb2) {
		LennardJonesParams p12 = getLennardJonesParams(a_symb1, a_symb2);
		if (p12 == null)
			return defaultC1;
		return p12.const1;
	}

	public double getLennardJonesConst2(String a_symb1, String a_symb2) {
		LennardJonesParams p12 = getLennardJonesParams(a_symb1, a_symb2);
		if (p12 == null)
			return defaultC2;
		return p12.const2;
	}

	public void setLennardJonesParams(LennardJonesParams p12, int index) {
		if ((index < 0) || (index > stringloopField[0].size()))
			return;
		stringloopField[0].setItemAt(LJParams2string(p12), index);
	}

	public void setLennardJonesParams(String atm1, String atm2, double c1, double c2) {
		LennardJonesParams p12 = new LennardJonesParams(atm1, atm2, c1, c2);
		//System.out.println("setRepulsionDistance(String atm1, String atm2, double dist)" + p12.const1 + p12.const2);
		for (int np = 0; np < stringloopField[0].size(); np++) {
			LennardJonesParams pij = string2LJParams((String)stringloopField[0].elementAt(np));
			if (p12.equals(pij))
				setLennardJonesParams(p12, np);
		}
	}

	public int getPairNumber() {
		return stringloopField[0].size();
	}

	public LennardJonesParams string2LJParams(String str) {
		StringTokenizer st_tkn = new StringTokenizer(str);
		String atm1 = st_tkn.nextToken();
		String atm2 = st_tkn.nextToken();
		double c1 = Double.parseDouble(st_tkn.nextToken());
		double c2 = Double.parseDouble(st_tkn.nextToken());
		return new LennardJonesParams(atm1, atm2, c1, c2);
	}

	public String LJParams2string(LennardJonesParams ljp) {
		return ljp.atm1 + " " + ljp.atm2 + " " + String.valueOf(ljp.const1) + " " + String.valueOf(ljp.const2);
	}

	public void refreshAtomPairs() {
		StructureAtomic m_Struct = getParentStructure();

		ArrayList atomPairs = new ArrayList();
		int na = m_Struct.getAtomList().size();
		for (int i = 0; i < na; i++) {
			for (int j = 0; j < na; j++) {
				String atom1 = ((AtomSite) m_Struct.getAtomList().get(i)).getSiteLabel();
				String atom2 = ((AtomSite) m_Struct.getAtomList().get(j)).getSiteLabel();
				LennardJonesParams pij_tmp = new LennardJonesParams(atom1, atom2);
				if (!atomPairs.contains(pij_tmp)) {
					atomPairs.add(pij_tmp);
				}
			}
		}

		int np_new, np_old;

		//check for removed atompairs in Structure

		for (np_old = 0; np_old < stringloopField[0].size(); np_old++) {
			boolean removed = true;
			String str_tmp = (String) stringloopField[0].elementAt(np_old);
			for (np_new = 0; np_new < atomPairs.size(); np_new++) {
				LennardJonesParams pij_tmp = (LennardJonesParams) atomPairs.get(np_new);
				if (pij_tmp.equals(string2LJParams(str_tmp)))
					removed = false;
			}
			if (removed)
				stringloopField[0].removeItemAt(np_old);
		}

		//check for added atompairs in Structure

		for (np_new = 0; np_new < atomPairs.size(); np_new++) {
			boolean newpair = true;
			LennardJonesParams rij_tmp = (LennardJonesParams) atomPairs.get(np_new);
			for (np_old = 0; np_old < stringloopField[0].size(); np_old++) {
				String str_tmp = (String) stringloopField[0].elementAt(np_old);
				if (rij_tmp.equals(string2LJParams(str_tmp)))
					newpair = false;
			}
			if (newpair)
				stringloopField[0].addItem(LJParams2string(rij_tmp));
		}
		refreshAtoms = false;
	}

	public double computeEnergy() {
		if (refreshAtoms) refreshAtomPairs();
		StructureAtomic m_Struct = getParentStructure();
		double PEnergy = 0.0;
		for (int na1 = 0; na1 < m_Struct.getAtomList().size(); na1++) {
			for (int na2 = 0; na2 < m_Struct.getAtomList().size(); na2++) {
				if (na1 == na2) continue;
				AtomSite atm1 = (AtomSite) m_Struct.getAtomList().get(na1);
				AtomSite atm2 = (AtomSite) m_Struct.getAtomList().get(na2);
				for (int np1 = 0; np1 < atm1.getCartesianCoords().size(); np1++) {
					for (int np2 = 0; np2 < atm2.getCartesianCoords().size(); np2++) {
						Coordinates acoord1 = atm1.getCartesianCoords(np1);
						Coordinates acoord2 = atm2.getCartesianCoords(np2);

						double c1 = getLennardJonesConst1(atm1.getFirstAtomSymbol(), atm2.getFirstAtomSymbol());
						double c2 = getLennardJonesConst2(atm1.getFirstAtomSymbol(), atm2.getFirstAtomSymbol());

						double rij = Math.sqrt(Math.pow(acoord1.x - acoord2.x, 2) + Math.pow(acoord1.y - acoord2.y, 2) + Math.pow(acoord1.z - acoord2.z, 2));
						double r_6 = Math.pow(1/rij, 6);
						double r_12 = Math.pow(r_6, 2);

						PEnergy += c1/r_12 - c2/r_6;
					}
				}
			}
		}
		return Math.log(PEnergy + 1);
	}

	public JOptionsDialog getOptionsDialog(Frame parent) {
		JOptionsDialog adialog = new JLJFFOptionsD(parent, this);
		return adialog;
	}

	public class JLJFFOptionsD extends JOptionsDialog {

		private JTable distTable;
		String[] columnNames;
		Object[][] data;

		public JLJFFOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);
			refreshAtomPairs();

			GridBagLayout gridbag = new GridBagLayout();
			GridBagConstraints c = new GridBagConstraints();
			c.fill = GridBagConstraints.HORIZONTAL;
			c.insets = new Insets(5, 5, 5, 5);
			//c.weightx = 1.0;
			//c.weighty = 1.0;
			principalPanel.setLayout(gridbag);

			distTable = new JTable(new ljTableModel());
			JScrollPane tablescrollPane = new JScrollPane(distTable);
			distTable.setPreferredScrollableViewportSize(new Dimension(300, 70));


			gridbag.setConstraints(tablescrollPane, c);
			principalPanel.add(tablescrollPane);


			setTitle("Lennard Jones Force Field");
			initParameters();
			pack();
		}

		public void SaveData() {
			for (int nd = 0; nd < data.length; nd++) {
				String atm1 = (String) data[nd][0];
				String atm2 = (String) data[nd][1];
				Double c1 = (Double) data[nd][2];
				Double c2 = (Double) data[nd][3];
				setLennardJonesParams(atm1, atm2, c1.doubleValue(), c2.doubleValue());
			}
		}

		public void initParameters() {
			super.initParameters();
		}

		public void retrieveParameters() {
			SaveData();
		}

		class ljTableModel extends AbstractTableModel {

			public ljTableModel() {
				columnNames = new String[]{"AtomSite 1", "AtomSite 2", "Const 1", "Const 2"};
				data = new Object[getPairNumber()][4];
				for (int nd = 0; nd < getPairNumber(); nd++) {
					LennardJonesParams ljp12 = getLennardJonesParams(nd);
					data[nd][0] = ljp12.atm1;
					data[nd][1] = ljp12.atm2;
					data[nd][2] = new Double(ljp12.const1);
					data[nd][3] = new Double(ljp12.const2);
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

	public class LennardJonesParams {
		public String atm1;
		public String atm2;
		public double const1;
		public double const2;

		public LennardJonesParams() {
		}

		public LennardJonesParams(String a1, String a2) {
			atm1 = a1;
			atm2 = a2;
			const1 = defaultC1;
			const2 = defaultC2;
		}

		public LennardJonesParams(String a1, String a2, double c1, double c2) {
			atm1 = a1;
			atm2 = a2;
			const1 = c1;
			const2 = c2;
		}

		public boolean equals(Object o) {
			LennardJonesParams r12 = (LennardJonesParams) o;
			if (((atm1.equals(r12.atm1)) && (atm2.equals(r12.atm2))) || ((atm1.equals(r12.atm2)) && (atm2.equals(r12.atm1))))
				return true;
			return false;
		}

		public int hashCode() {
			return (int) const1;
		}
	}
}


