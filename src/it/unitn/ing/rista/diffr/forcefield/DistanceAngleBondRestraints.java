/*
 * @(#)DistanceAngleBondRestraints.java created Apr 21, 2011 Caen
 *
 * Copyright (c) 2011 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.forcefield;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.diffr.structure.StructureAtomic;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.JOptionsDialog;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import java.util.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.io.*;

/**
 * The DistanceAngleBondRestraints is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Apr 21, 2011 1:56:22 PM $
 * @since JDK1.1
 */
public class DistanceAngleBondRestraints extends ForceField {

  protected static String[] diclistc = {
      "_geom_bond_distance_max",
      "_geom_bond_distance",
      "_geom_angle"
  };

  protected static String[] diclistcrm = {
      "Maximum distance between atoms",
      "Optimal bond distance (Angstrom)",
      "Optimal bond angle (degrees)"
  };

  protected static String[] classlistc = {};

  protected static String[] classlistcs = {};

  private Vector bondsVector = null;
  private Vector anglesVector = null;

  protected double maxDistance = 2.0;

  public DistanceAngleBondRestraints(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Distance & Angles Restraints";
    IDlabel = "Distance & Angles Restraints";
    description = "Distance & Angles Restraints";
  }

  public DistanceAngleBondRestraints(XRDcat aobj) {
    this(aobj, "Distance & Angles Restraints");
  }

  public DistanceAngleBondRestraints() {
    identifier = "Distance & Angles Restraints";
    IDlabel = "Distance & Angles Restraints";
    description = "Distance & Angles Restraints";
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

  public Vector getDistancePair() {
    return bondsVector; //stringloopField[0];
  }

  public int getDistancePairNumber() {
    return getDistancePair().size();
  }

  public RepulsionDistance getRepulsionDistance(int index) {
    if ((index < 0) || (index >= getDistancePairNumber()))
      return null;
    return (RepulsionDistance) getDistancePair().elementAt(index);
  }

  public void setRepulsionDistance(RepulsionDistance r12, int index) {
    if ((index < 0) || (index >= getDistancePairNumber()))
      return;
    getDistancePair().setElementAt(r12, index);
  }

  public void setRepulsionDistance(int index, AtomSite atm1, AtomSite atm2, double dist, double kappa) {
    RepulsionDistance r12 = new RepulsionDistance(atm1, atm2, dist, kappa);
    RepulsionDistance rij = (RepulsionDistance) getDistancePair().elementAt(index);
    if (r12.equals(rij))
        setRepulsionDistance(r12, index);
  }

  public RepulsionDistance string2RepulsionDist(String str) {
    StringTokenizer st_tkn = new StringTokenizer(str);
    String atm1 = st_tkn.nextToken();
    String atm2 = st_tkn.nextToken();
    double dist = Double.parseDouble(st_tkn.nextToken());
    double kappa = Double.parseDouble(st_tkn.nextToken());
    return new RepulsionDistance(getAtom(atm1), getAtom(atm2), dist, kappa);
  }

  public String RepulsionDist2string(RepulsionDistance dst) {
    return new StringBuffer().append(dst.atm1.getLabel()).append(" ").append(dst.atm2.getLabel()).append(" ")
        .append(String.valueOf(dst.dist)).append(" ").append(String.valueOf(dst.kappa)).toString();
  }

  public static double getDistance(AtomSite atm1, AtomSite atm2) {
	  if (atm1 == atm2)
		  return getDistanceFromSelf(atm1);
    double distance = 1E9;
    for (int np1 = 0; np1 < atm1.getCartesianCoords().size(); np1++) {
      Coordinates c1 = atm1.getCartesianCoords(np1);
      for (int np2 = 0; np2 < atm2.getCartesianCoords().size(); np2++) {
        Coordinates c2 = atm2.getCartesianCoords(np2);
        double dist = Math.sqrt(Math.pow(c1.x - c2.x, 2) + Math.pow(c1.y - c2.y, 2) +
                      Math.pow(c1.z - c2.z, 2));
        if (dist < distance)
          distance = dist;
      }
    }
    return distance;
  }

	public static double getDistanceFromSelf(AtomSite atm1) {
		double distance = 1E9;
		for (int np1 = 0; np1 < atm1.getCartesianCoords().size(); np1++) {
			Coordinates c1 = atm1.getCartesianCoords(np1);
			for (int np2 = np1 + 1; np2 < atm1.getCartesianCoords().size(); np2++) {
				Coordinates c2 = atm1.getCartesianCoords(np2);
				double dist = Math.sqrt(Math.pow(c1.x - c2.x, 2) + Math.pow(c1.y - c2.y, 2) +
						Math.pow(c1.z - c2.z, 2));
				if (dist < distance)
					distance = dist;
			}
		}
		return distance;
	}

	public static int[] getPairWithShorterDistance(AtomSite atm1, AtomSite atm2, AtomSite atm3) {
		int[] pair = new int[3];
		int[] pair1 = getPairWithShorterDistance(atm1, atm2);
		int pair2 = getPairWithShorterDistance(atm1, atm3, pair1[0]);
		pair[0] = pair1[0];
		pair[1] = pair1[1];
		pair[2] = pair2;
		return pair;
	}

	public static int[] getPairWithShorterDistance(AtomSite atm1, AtomSite atm2) {
    double distance = 1E9;
    int[] pair = new int[2];
    for (int np1 = 0; np1 < atm1.getCartesianCoords().size(); np1++) {
      Coordinates c1 = atm1.getCartesianCoords(np1);
      for (int np2 = 0; np2 < atm2.getCartesianCoords().size(); np2++) {
        Coordinates c2 = atm2.getCartesianCoords(np2);
        double dist = Math.sqrt(Math.pow(c1.x - c2.x, 2) + Math.pow(c1.y - c2.y, 2) +
                      Math.pow(c1.z - c2.z, 2));
        if (dist < distance) {
          distance = dist;
          pair[0] = np1;
          pair[1] = np2;
        }
      }
    }
    return pair;
  }

	public static int[] getPairWithShorterDistanceFromSelf(AtomSite atm1, AtomSite atm2) {
		int[] pair = new int[3];
		int[] pair1 = getPairWithShorterDistance(atm1, atm2);
		int pair2 = getPairWithShorterDistance(atm1, atm2, pair1);
		pair[0] = pair1[0];
		pair[1] = pair1[1];
		pair[2] = pair2;
		return pair;
	}

	public static int getPairWithShorterDistance(AtomSite atm1, AtomSite atm2, int np1) {
    double distance = 1E9;
    int pair = 0;
    Coordinates c1 = atm1.getCartesianCoords(np1);
      for (int np2 = 0; np2 < atm2.getCartesianCoords().size(); np2++) {
        Coordinates c2 = atm2.getCartesianCoords(np2);
        double dist = Math.sqrt(Math.pow(c1.x - c2.x, 2) + Math.pow(c1.y - c2.y, 2) +
                      Math.pow(c1.z - c2.z, 2));
        if (dist < distance) {
          distance = dist;
          pair = np2;
        }
      }
    return pair;
  }

	public static int getPairWithShorterDistance(AtomSite atm1, AtomSite atm2, int[] np1) {
		double distance = 1E9;
		int pair = 0;
		Coordinates c1 = atm1.getCartesianCoords(np1[0]);
		for (int np2 = 0; np2 < atm2.getCartesianCoords().size(); np2++) {
			if (np2 != np1[1]) {
			Coordinates c2 = atm2.getCartesianCoords(np2);
			double dist = Math.sqrt(Math.pow(c1.x - c2.x, 2) + Math.pow(c1.y - c2.y, 2) +
					Math.pow(c1.z - c2.z, 2));
			if (dist < distance) {
				distance = dist;
				pair = np2;
			}
			}
		}
		return pair;
	}

	public static double getAngle(AtomSite a1, AtomSite a2, AtomSite a3) {
	  int[] pair1;
	  if (a2 == a3)
		  pair1 = getPairWithShorterDistanceFromSelf(a1, a2);
	  else
		  pair1 = getPairWithShorterDistance(a1, a2, a3);
    Coordinates c1 = a1.getCartesianCoords(pair1[0]);
    Coordinates c2 = a2.getCartesianCoords(pair1[1]);
    Coordinates c3 = a3.getCartesianCoords(pair1[2]);
    double ax = c2.x - c1.x;
    double ay = c2.y - c1.y;
    double az = c2.z - c1.z;
    double bx = c3.x - c1.x;
    double by = c3.y - c1.y;
    double bz = c3.z - c1.z;
    double cost = (ax * bx + ay * by + az * bz) /
        (Math.sqrt(ax * ax + ay * ay + az * az) * Math.sqrt(bx * bx + by * by + bz * bz));
    return MoreMath.acosd(cost);
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

    if (bondsVector == null || anglesVector == null)
      refreshAllVectors();
    else {
      StructureAtomic m_Struct = getParentStructure();
      Phase aphase = m_Struct.getPhaseParent();
      Vector atoms = aphase.getFullAtomList();

      int na = atoms.size();
      for (int i = 0; i < na; i++) {
        AtomSite atom1 = (AtomSite) atoms.get(i);
	      atom1.refreshComputation = true;
        atom1.refreshPositions(true);
      }
    }
    refreshComputation = false;
  }

  public void refreshAllVectors() {

    if (bondsVector == null || anglesVector == null)
      updateVectors();

	  int plus1 = 1;
	  if (MaudPreferences.getBoolean("distanceAngleAtom.useSelf", true))
		  plus1 = 0;

    StructureAtomic m_Struct = getParentStructure();
    Phase aphase = m_Struct.getPhaseParent();
    Vector atoms = aphase.getFullAtomList();

    double maximumDistance = getMaxDistance();

    ArrayList atomPairs = new ArrayList();
    int na = atoms.size();
    for (int i = 0; i < na; i++) {
      AtomSite atom1 = (AtomSite) atoms.get(i);
      atom1.refreshPositions(true);
      if (!atom1.isDummyAtom()) {
        for (int j = i + plus1; j < na; j++) {
          AtomSite atom2 = (AtomSite) atoms.get(j);
          if (!atom2.isDummyAtom()) {
            double dist = getDistance(atom1, atom2);
//	          System.out.println("Checking distance for atoms (" + atom1.getLabel() + ", " + atom2.getLabel() + "): " + dist);
            if (dist < maximumDistance) {
              RepulsionDistance rij_tmp = new RepulsionDistance(atom1, atom2, dist);
              atomPairs.add(rij_tmp);
            }
          }
        }
      }
    }

    int np_new, np_old;

    //check for removed atompairs in Structure

    for (np_old = 0; np_old < getDistancePairNumber(); np_old++) {
      boolean removed = true;
//      String str_tmp = (String) getDistancePair().elementAt(np_old);
      RepulsionDistance rij = (RepulsionDistance) getDistancePair().elementAt(np_old); //string2RepulsionDist(str_tmp);
      for (np_new = 0; np_new < atomPairs.size(); np_new++) {
        RepulsionDistance rij_tmp = (RepulsionDistance) atomPairs.get(np_new);
        if (rij.equals(rij_tmp)) {
          removed = false;
          break;
        }
      }
      if (removed) {
        getDistancePair().removeElementAt(np_old);
        np_old--;
      }
    }

    //check for added atompairs in Structure

    for (np_new = 0; np_new < atomPairs.size(); np_new++) {
      boolean newpair = true;
      RepulsionDistance rij_tmp = (RepulsionDistance) atomPairs.get(np_new);
      for (np_old = 0; np_old < getDistancePairNumber(); np_old++) {
        if (rij_tmp.equals(getDistancePair().elementAt(np_old))) {
          newpair = false;
          break;
        }
      }
      if (newpair)
        getDistancePair().addElement(rij_tmp);
    }

// Angles

    ArrayList atomT = new ArrayList();
    na = atoms.size();
    for (int i = 0; i < na; i++) {
      AtomSite atom1 = (AtomSite) atoms.get(i);
      if (!atom1.isDummyAtom()) {
        for (int k = 0; k < na; k++) {
          AtomSite atom3 = (AtomSite) atoms.get(k);
          if (!atom3.isDummyAtom()) {
            for (int j = k + plus1; j < na; j++) {
              if (i != j && i != k) {
                AtomSite atom2 = (AtomSite) atoms.get(j);
                if (!atom2.isDummyAtom()) {
                  double dist1 = getDistance(atom1, atom2);
                  double dist2 = getDistance(atom1, atom3);
                  if (dist1 < maxDistance && dist2 < maxDistance) {
                    OptimumAngle rij_tmp = new OptimumAngle(atom1, atom2, atom3);
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

  private void updateVectors() {
    bondsVector = new Vector(0, 100);
    anglesVector = new Vector(0, 100);
    for (int i = 0; i < stringloopField[0].size(); i++)
      bondsVector.addElement(string2RepulsionDist((String) stringloopField[0].elementAt(i)));
    for (int i = 0; i < stringloopField[1].size(); i++)
      anglesVector.addElement(string2OptimumAngle((String) stringloopField[1].elementAt(i)));
  }

  private void updateLoopFields() {
    if (bondsVector != null) {
      stringloopField[0].removeAllItems();
      for (int i = 0; i < bondsVector.size(); i++)
        stringloopField[0].addItem(RepulsionDist2string((RepulsionDistance) bondsVector.elementAt(i)));
    }
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
    if (refreshComputation) refreshAtomPairs();
    double PEnergy = 0.0;
    for (int nd = 0; nd < getDistancePairNumber(); nd++) {
      double energy = 0.0;
      RepulsionDistance rij = (RepulsionDistance) getDistancePair().elementAt(nd);
      if (rij.kappa > 0 && rij.dist > 0 && (rij.atm1.getOccupancyValue() + rij.atm2.getOccupancyValue()) > 1.0)
        energy = Math.pow((rij.dist - getDistance(rij.atm1, rij.atm2)) / rij.dist, 2) * rij.kappa;
//      if (Constants.testing)
//        System.out.println("Energy: " + rij.atm1.getLabel() + " " + rij.atm2.getLabel() + " " + rij.dist + " " + getDistance(rij.atm1, rij.atm2) + " " + energy);
      PEnergy += energy;
    }

    for (int nd = 0; nd < getAnglesNumber(); nd++) {
      OptimumAngle rij = (OptimumAngle) getAngles().elementAt(nd);
      if (rij.kappa > 0 && rij.angle != 0)
        PEnergy += Math.pow((rij.angle - getAngle(rij.atm1, rij.atm2, rij.atm3)) / rij.angle, 2) * rij.kappa;
    }
//    if (Constants.testing)
//      System.out.println("Energy restraint: " + PEnergy);
    return PEnergy; //Math.log(PEnergy + 1);
  }

  public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new JSRDAOptionsD(parent, this);
    return adialog;
  }

  public class JSRDAOptionsD extends JOptionsDialog {

    private JTable distTable;
    private JTable angTable;
    String[] columnNames;
    Object[][] data;
    String[] angleColumnNames;
    Object[][] angleData;
    JTextField maxDistTF;

    public JSRDAOptionsD(Frame parent, XRDcat obj) {

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

      setTitle("Distance & Angles Restraints");
      setHelpFilename("DistanceAngleBondRestraints.txt");

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
        AtomSite atm1 = (AtomSite) data[nd][0];
        AtomSite atm2 = (AtomSite) data[nd][1];
        Double dist = (Double) data[nd][3];
        Double kappa = (Double) data[nd][4];
        setRepulsionDistance(nd, atm1, atm2, dist.doubleValue(), kappa.doubleValue());
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
        columnNames = new String[]{"AtomSite 1", "AtomSite 2", "Act. bond", "Opt. bond", "Strength"};
        data = new Object[getDistancePairNumber()][5];
        for (int nd = 0; nd < getDistancePairNumber(); nd++) {
          RepulsionDistance r12 = getRepulsionDistance(nd);
          data[nd][0] = r12.atm1;
          data[nd][1] = r12.atm2;
          data[nd][2] = getDistance(r12.atm1, r12.atm2);
          data[nd][3] = new Double(r12.dist);
          data[nd][4] = new Double(r12.kappa);
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
        if (col < 3) {
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
        angleColumnNames = new String[]{"Pivot atom", "AtomSite 1", "AtomSite 2", "Act. Angle", "Opt. Angle", "Strength"};
        angleData = new Object[getAnglesNumber()][6];
        for (int nd = 0; nd < getAnglesNumber(); nd++) {
          OptimumAngle r12 = getOptimumAngle(nd);
          angleData[nd][0] = r12.atm1;
          angleData[nd][1] = r12.atm2;
          angleData[nd][2] = r12.atm3;
          angleData[nd][3] = getAngle(r12.atm1, r12.atm2, r12.atm3);
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

  public class RepulsionDistance {
    public AtomSite atm1;
    public AtomSite atm2;
    public double dist;
    public double kappa;

    public RepulsionDistance() {
    }

    public RepulsionDistance(AtomSite a1, AtomSite a2) {
      atm1 = a1;
      atm2 = a2;
      dist = a1.getMeanRadius() + a2.getMeanRadius();
      kappa = 0.0;
    }

    public RepulsionDistance(AtomSite a1, AtomSite a2, double d) {
      atm1 = a1;
      atm2 = a2;
      dist = d;
      kappa = 0.0;
    }

    public RepulsionDistance(AtomSite a1, AtomSite a2, double d, double kappaSpring) {
      atm1 = a1;
      atm2 = a2;
      dist = d;
      kappa = kappaSpring;
    }

    public boolean equals(Object o) {
      RepulsionDistance r12 = (RepulsionDistance) o;
      if (((atm1.equalsByDistance(r12.atm1)) && (atm2.equalsByDistance(r12.atm2))) ||
          ((atm1.equalsByDistance(r12.atm2)) && (atm2.equalsByDistance(r12.atm1))))
        return true;
      return false;
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
      angle = getAngle(a1, a2, a3);
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
