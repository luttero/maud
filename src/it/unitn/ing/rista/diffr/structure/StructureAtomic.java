/*
 * @(#)StructureAtomic.java created March 1, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.structure;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.ListVector;
import it.unitn.ing.rista.interfaces.AtomsStructureI;
import it.unitn.ing.rista.util.MaudPreferences;

import java.awt.*;
import java.util.Vector;

/**
 *  The StructureAtomic is a
 *
 *
 * @version $Revision: 1.19 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureAtomic extends StructureModel implements AtomsStructureI {


  public static final int AtomListID = 0;
  public static final int FragmentListID = 1;
  public static final int BondListID = 2;
  boolean quantityFromOccupancy = true;
//  public static final int structureSolutionMethodID = 0;
  public static final int forceFieldID = 0;
  public double energyWeight = 1.0;

	/**
	 * DebyeWaller model switch, default Uiso
	 */
	public static boolean DebyeWallerModelDefault = MaudPreferences.getBoolean("atomicDisplacements.useDimensionlessFactors", false);
	public boolean DebyeWallerModelDimensionLess = true;

	protected static String[] diclistc = {
    "_riet_structure_quantity_from_occupancy", "_refine_ls_energy_weight",
			"_riet_structure_use_U_dimensionless",

//		"_riet_structure_solution_method",
		"_riet_structure_force_field",

		"_atom_site_label",
		"_fragment_label",
		"_bond_label"

	};

  protected static String[] diclistcrm = {
    "_riet_structure_quantity_from_occupancy", "_refine_ls_energy_weight",
		  "_riet_structure_use_U_dimensionless",

//    "_riet_structure_solution_method",
    "_riet_structure_force_field",

    "_atom_site_label",
    "_fragment_label",
    "_bond_label"

  };

  protected static String[] classlistc = {

		"it.unitn.ing.rista.diffr.AtomSite",
		"it.unitn.ing.rista.diffr.Fragment",
		"it.unitn.ing.rista.diffr.Bond"

	};

  protected static String[] classlistcs = {

//		"superclass:it.unitn.ing.rista.diffr.StructureSolutionMethod",
		"superclass:it.unitn.ing.rista.diffr.ForceField"

	};

  public StructureAtomic(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "Atomic Structure";
    IDlabel = "Atomic Structure";
    description = "Atomic Structure";
  }

  public StructureAtomic(XRDcat aobj) {
    this(aobj, "Atomic Structure");
  }

  public StructureAtomic() {
    identifier = "Atomic Structure";
    IDlabel = "Atomic Structure";
    description = "Atomic Structure";
  }

	public void initConstant() {
    Nstring = 3;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = classlistcs.length;
    Nsubordinateloop = classlistc.length;
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
    setQuantityFromOccupancy(true);
    stringField[1] = new String("1.0");
//    setSubordinateModel(structureSolutionMethodID, "Genetic Algorithm SDPD");
    setSubordinateModel(forceFieldID, "No force field");
	  setDebyeWallerModelDimensionLess(DebyeWallerModelDefault = MaudPreferences.getBoolean("atomicDisplacements.useDimensionlessFactors", false));
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(false);
//    System.out.println("Refreshing occupancy");
    quantityFromOccupancy = getQuantityFromOccupancy();
    energyWeight = Double.parseDouble(stringField[1]);
	  DebyeWallerModelDimensionLess = getDebyeWallerModelDimensionLess();
  }

  public boolean getQuantityFromOccupancy() {
    return stringField[0].equalsIgnoreCase("true");
  }

  public void setQuantityFromOccupancy(boolean status) {
    if (status)
      stringField[0] = new String("true");
    else
      stringField[0] = new String("false");
    quantityFromOccupancy = status;
  }

  public void setQuantityFromOccupancy(String value) {
    stringField[0] = new String(value);
  }

  public boolean quantityFromOccupancy() {
    return quantityFromOccupancy;
  }

	public boolean isDebyeWallerModelDimensionLess() {
		return DebyeWallerModelDimensionLess;
	}

	public boolean getDebyeWallerModelDimensionLess() {
		return stringField[2].equalsIgnoreCase("true");
	}

	public void setDebyeWallerModelDimensionLess(boolean value) {
		if (value != getDebyeWallerModelDimensionLess() && stringField[2].length() > 0) {
			Vector atomList = getFullAtomList();
			if (value)
				for (int i = 0; i < atomList.size(); i++)
					((AtomSite) atomList.elementAt(i)).convertAtomDisplacementsToDimensionless();
			else
				for (int i = 0; i < atomList.size(); i++)
					((AtomSite) atomList.elementAt(i)).convertAtomDisplacementsFromDimensionless();
		}
		if (value)
			stringField[2] = "true";
		else
			stringField[2] = "false";
	}

	public void setEnergyWeight(String value) {
    stringField[1] = new String(value);
  }

  public String getEnergyWeightS() {
    return stringField[1];
  }

  public double getEnergyWeight() {
    return energyWeight;
  }

	public ListVector getAtomList() {
		return subordinateloopField[AtomListID];
	}

  public Vector getFullAtomList() {
    Vector fullAtomList = new Vector(0, 1);
    fullAtomList.addAll(getAtomList());
    for (int nf = 0; nf < getFragmentList().size(); nf++) {
      fullAtomList.addAll(((Fragment) getFragmentList().get(nf)).getFullAtomList());
    }
    return fullAtomList;
  }

  public void deleteAtomFromFullList(int i) {
    if (i >= 0 && i < getAtomList().size())
      getAtomList().removeItemAt(i);
    else {
      int index = i - getAtomList().size();
      for (int nf = 0; nf < getFragmentList().size(); nf++) {
        Fragment frag = ((Fragment) getFragmentList().get(nf));
        if (index >= 0 && index < frag.getFullAtomList().size()) {
          frag.deleteAtomFromFullList(index);
          break;
        }
        index -= frag.getFullAtomList().size();
      }
    }
  }

  public int getAtomNumber() {
    return getAtomList().size();
  }

  public AtomSite getAtom(int index) {
    return (AtomSite) getAtomList().elementAt(index);
  }

  public void addAtom() {
    AtomSite newatom = new AtomSite(this);
    addAtom(newatom);
    newatom.addAtomWithSymbol("Ca");
  }

  public void addAtom(AtomSite newatom) {
    addsubordinateloopField(AtomListID, newatom);
    getPhaseParent().refreshAtoms = true;
    getPhaseParent().fullAtomList = null;
  }

  public boolean removeSelectedAtom() {
    return removeselSubLField(AtomListID);
  }

  public void removeAtomAt(int number) {
    removeSubLFieldAt(AtomListID, number);
  }

  public ListVector getFragmentList() {
    return subordinateloopField[FragmentListID];
  }

  public int getFragmentNumber() {
    return getFragmentList().size();
  }

  public Fragment getFragment(int index) {
    return (Fragment) getFragmentList().elementAt(index);
  }

	public void addFragment(Fragment frg) {
    addsubordinateloopField(FragmentListID, frg);
    getPhaseParent().refreshAtoms = true;
    getPhaseParent().fullAtomList = null;
	}

	public void addFragment() {
    Fragment newFragment = new Fragment(this, "fragment_x");
    addFragment(newFragment);
  }

  public boolean removeSelectedFragment() {
    return removeselSubLField(FragmentListID);
  }

  public void removeFragmentAt(int number) {
    removeSubLFieldAt(FragmentListID, number);
  }

  public ListVector getBondList() {
    return subordinateloopField[BondListID];
  }

  public int getBondNumber() {
    return getBondList().size();
  }

  public void addBond(Bond bnd) {
    addsubordinateloopField(BondListID, bnd);
    getPhaseParent().refreshAtoms = true;
    getPhaseParent().fullAtomList = null;
  }

	public void addBond() {
		Bond newBond = new Bond(this);
		addBond(newBond);
  }

  public boolean removeSelectedBond() {
    return removeselSubLField(BondListID);
  }

  public void removeBondAt(int number) {
    removeSubLFieldAt(BondListID, number);
  }

  public ForceField getForceFieldModel() {
    return (ForceField) getActiveSubordinateModel(forceFieldID);
  }

  public double computeEnergy() {
    return getForceFieldModel().computeEnergy();
  }

  public void freeAllCrystalParameters() {
    AtomSite anatom;
    int atomNumbers = getAtomNumber();
    boolean share;

    for (int i = 0; i < atomNumbers; i++) {
      anatom = getAtom(i);
      share = false;
      for (int j = 0; j < i; j++)
        if (anatom.shareSiteWith(getAtom(j))) {
          share = true;
          anatom.boundAllParametersTo(getAtom(j));
        }
      if (!share)
        anatom.freeAllCrystalParameters();
    }
    for (int i = 0; i < getFragmentNumber(); i++)
      getFragment(i).freeAllCrystalParameters();
  }

  public void convertAtomsForSG(double traslx, double trasly, double traslz) {
    int numbAtomn = getAtomNumber();
    for (int i = 0; i < numbAtomn; i++) {
      AtomSite oldatom = getAtom(i);
      AtomSite newatom = new AtomSite(this);
      newatom.setSiteLabel(oldatom.getSiteLabel() + "a");
	    for (int j = 0; j < oldatom.subordinateloopField[AtomSite.scattererLoopID].size(); j++) {
		    newatom.addsubordinateloopField(AtomSite.scattererLoopID, ((XRDcat)
				    oldatom.subordinateloopField[AtomSite.scattererLoopID].elementAt(j)).getCopy(this));
	    }
      newatom.getLocalCoordX().setValue(oldatom.getLocalCoordX().getValueD() + traslx);
      newatom.getLocalCoordX().setEqualTo(oldatom.getLocalCoordX(), 1.0, traslx);
      newatom.getLocalCoordY().setValue(oldatom.getLocalCoordY().getValueD() + trasly);
      newatom.getLocalCoordY().setEqualTo(oldatom.getLocalCoordY(), 1.0, trasly);
      newatom.getLocalCoordZ().setValue(oldatom.getLocalCoordZ().getValueD() + traslz);
      newatom.getLocalCoordZ().setEqualTo(oldatom.getLocalCoordZ(), 1.0, traslz);
      newatom.getOccupancy().setValue(oldatom.getOccupancy().getValueD());
      newatom.getOccupancy().setEqualTo(oldatom.getOccupancy(), 1.0, 0.0);
      newatom.getBfactor().setValue(oldatom.getBfactor().getValueD());
      newatom.getBfactor().setEqualTo(oldatom.getBfactor(), 1.0, 0.0);
      for (int j = 0; j < 6; j++) {
        newatom.getAnisoBfactor(j).setValue(oldatom.getAnisoBfactor(j).getValueD());
        newatom.getAnisoBfactor(j).setEqualTo(oldatom.getAnisoBfactor(j), 1.0, 0.0);
      }
      newatom.setDummy(oldatom.isDummyAtom());
      addAtom(newatom);
    }
    for (int i = 0; i < getFragmentNumber(); i++) {
      // here we should create a new fragment for each one already existing
      // and translate it by traslx, trasly, traslz
    }
  }

  public iJPanel getEditPanel(myJFrame aframe) {
    return new StructureAtomicEditPanel(aframe, this);
  }

	public JOptionsDialog getOptionsDialog(Frame parent) {
    JOptionsDialog adialog = new StructureAtomicEditD(parent, this);
    return adialog;
  }

  public class StructureAtomicEditD extends JOptionsDialog {

    StructureAtomicEditPanel structPanel = null;

    public StructureAtomicEditD(Frame parentFrame, StructureAtomic tStruct) {
      super(parentFrame, tStruct, "Edit Structure");

      initComponents(tStruct);
//    setResizable(false);
    }

    public void initComponents(StructureAtomic m_Struct) {

      structPanel = new StructureAtomicEditPanel(this, m_Struct);
      principalPanel.add(structPanel);

      pack();
    }

    public void retrieveParameters() {
      structPanel.retrieveParameters();
      super.retrieveParameters();
    }

    public void dispose() {
      structPanel.dispose();
      super.dispose();
    }

  }

}
