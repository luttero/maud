/*
 * @(#)AtomScatterer.java created 04/06/16 Casalino
 *
 * Copyright (c) 1996-2016 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is 
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.chemistry.AtomInfo;
import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.util.AtomQuantity;
import it.unitn.ing.rista.util.Constants;

/**
 * The AtomScatterer is a class ....
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: 04/06/16 12:14 $
 * @since JDK1.1
 */

public class AtomScatterer extends Scatterer {

	/**
	 * The atomic weight
	 */
	private double weight = 0;

	/**
	 * The atomic radius
	 */
	private double radius = 1.0;

	/**
	 * The atomic number
	 */
	private int atomNumber = 1;

	/**
	 * The corresponding number of this atom in the atominfo tables
	 */
	private int atomListNumber = 1;

	private int oxidationNumber = 0;

	/**
	 * The corresponding number of this isotope in the atominfo tables
	 */
	private int isotopeListNumber = 1;

	/**
	 * The atom quantity in the cell = occupancy * site multeplicity
	 */
	private double quantity;

	private static int occupancyID = 0;

	protected static String[] diclistc = {"_atom_type_symbol", "_atom_site_occupancy"};
	protected static String[] diclistcrm = {"_atom_type_symbol", "_atom_site_occupancy"};

	protected static String[] classlistc = {};

		public AtomScatterer(XRDcat obj, String alabel) {
			super(obj, alabel);
			initXRD();
			if (alabel != null && alabel.length() > 0)
				stringField[0] = alabel;
			loadAtomProperties();
		}

		public AtomScatterer(XRDcat afile) {
			this(afile, "Atom scatterer x");
		}

		public void initConstant() {
			Nstring = 1;
			Nstringloop = 0;
			Nparameter = 1;
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
		}

		public void initParameters() {
			super.initParameters();
			stringField[0] = "Ca";
			initializeParameter(0, 1.0, 0.0, 1.0);
//			System.out.println("1 Occupancy: " + getOccupancy() + " " + getParameter(occupancyID).getValueD());
		}

	public void notifyStringChanged(String source) {
		notifyStringChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
	}

	public void notifyObjectChanged(XRDcat source) {
		notifyUpObjectChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
	}

	public void notifyParameterChanged(Parameter source) {
			FilePar filepar = getFilePar();
//		System.out.println("2 Occupancy: " + getOccupancy() + " " + getParameter(occupancyID).getValueD());
			if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
				if (parameterField != null)
					for (int i = 0; i < parameterField.length; i++) {
						if (parameterField[i] == source) {
							notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
							return;
						}
					}
				super.notifyParameterChanged(source);
			}
//		System.out.println("3 Occupancy: " + getOccupancy() + " " + getParameter(occupancyID).getValueD());

	}

	/**
	 * Overwrite the setField of XRDcat; necessary because we need to check the atom symbol for
	 * consistency with the Maud convention. So in case the field is the atom symbol the check is performed.
	 * In any case the setField() method of XRDcat is performed.
	 *
	 * @param cif
	 * @param astring
	 * @param astringerror
	 * @param min
	 * @param max
	 * @param free
	 * @param refName
	 * @param refBound
	 * @param constant
	 * @param ratio
	 * @return the number of the field set up
	 * @see XRDcat#setField
	 */
	public int setField(String cif, String astring, String astringerror, String min, String max, boolean free,
	                    String refName, String refBound, String constant, String ratio, String expression,
	                    boolean autoTrace, boolean positive) {
		int index = super.setField(cif, astring, astringerror, min, max, free, refName, refBound, constant, ratio, expression,
				autoTrace, positive);
		if (index == 0) {
//      System.out.println("SetField, starting symbol: " + getAtomSymbol());
			stringField[0] = AtomSite.checkAtomSymbol(AtomSite.checkOxidationNumber(getAtomSymbol()), "");
			setLabel(stringField[0]);
//         System.out.println("SetField, final symbol: " + stringField[0]);
			loadAtomProperties();
		}
//		refreshQuantityD();  // to be sure will be refreshed at a certain point
		return index;
	}

	public void updateStringtoDoubleBuffering(boolean firstLoading) {
		super.updateStringtoDoubleBuffering(firstLoading);
		String oldSymbol = getAtomSymbol();
//    System.out.println("Update starting symbol: " + oldSymbol);
		stringField[0] = AtomSite.checkAtomSymbol(AtomSite.checkOxidationNumber(oldSymbol), "");
		setLabel(stringField[0]);
//    System.out.println("Update final symbol: " + stringField[0]);
//    if (!stringField[0].equalsIgnoreCase(oldSymbol))
		loadAtomProperties();
//		useThisAtom = !isDummyAtom();
	}

	private String loadedSymbol = null;

	public void loadAtomProperties() {
		String symbol = getAtomSymbol();
		if (loadedSymbol == null || !symbol.equalsIgnoreCase(loadedSymbol)) {
//    System.out.println("Loading properties for atom: " + stringField[0]);
			atomNumber = AtomInfo.retrieveAtomNumber(AtomSite.stripIsotopeNumber(symbol));
			atomListNumber = AtomInfo.getAtomNumber(AtomSite.stripIsotopeNumber(symbol));
			oxidationNumber = AtomSite.getOxidationNumber(symbol);
			isotopeListNumber = AtomInfo.getIsotopeNumber(AtomSite.stripOxidation(symbol));
			weight = AtomInfo.retrieveAtomWeight(AtomSite.stripIsotopeNumber(symbol));
			radius = Math.abs(AtomInfo.retrieveAtomRadius(AtomSite.stripIsotopeNumber(symbol)));
			//		System.out.println(magneticSF[0]);
			loadedSymbol = symbol;
		}
	}

	/**
	 * Use this to get the actual complete symbol of this atom in Maud convention. E.g. AG2+ etc.
	 *
	 * @return the atom symbol
	 */
	public String getAtomSymbol() {
		return stringField[0];
	}

	public void setAtomSymbol(String alabel) {
		if (!alabel.equals(stringField[0])) {
//      System.out.println("Starting symbol as label: " + alabel);
			stringField[0] = AtomSite.checkAtomSymbol(AtomSite.checkOxidationNumber(alabel), getLabel());
			setLabel(getAtomSymbol());
//      System.out.println("Final symbol as label: " + stringField[0]);
//      stringField[0] = alabel;
			((AtomSite) getParent()).getPhaseParent().refreshAtoms = true;
			loadedSymbol = null;
			loadAtomProperties();
		}
	}
	/**
	 * To be used to get the weight of this atom.
	 *
	 * @return the actual weight of the atom
	 */
	public double getAtomWeight() {
		return weight;
	}

	public double getAtomRadius() {
		return radius;
	}

	public void setOccupancy(double value) {
		parameterField[occupancyID].setValue(value);
	}

	public double getOccupancy() {
		return getParameter(occupancyID).getValueD();
	}

	public double getSiteWeight() {
//		refreshPositions(false);
		return getOccupancy() * getAtomWeight();
	}

	public double getSiteNumber() {
		return ((AtomSite) getParent()).getSiteMultiplicity() * getOccupancy();
	}

	public AtomQuantity getAtomQuantity() {
//		System.out.println(getAtomSymbol() + " " + getAtomWeight() + " " + getSiteNumber());
		return new AtomQuantity(AtomInfo.cutOxidationNumber(getAtomSymbol()), getAtomWeight(), getSiteNumber());
	}

	/**
	 * Gets the total absorption of this atom site. Proportional to the quantity.
	 *
	 * @param rad the Radiation kind for which the absorption must be computed.
	 * @return the total absorption of this atom site.
	 */
	public double getSiteAbsorption(RadiationType rad) {
		if (rad.isNeutron())
			return getSiteWeight() * rad.getRadiation(0).neutronAbs[getAtomicListNumber()];
		else if (rad.isElectron())
			return getSiteWeight() * rad.getRadiation(0).electronAbs[getAtomicListNumber()];
		else
			return 0.0;
	}

	public double getSiteAbsorption(double energyInKeV) {
//	  System.out.println("Atom: " + getAtomicNumber() + ", absorption: " + XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(getAtomicNumber(), energyInKeV));
		return getSiteWeight() * XRayDataSqLite.getTotalAbsorptionForAtomAndEnergy(getAtomicNumber(), energyInKeV);
	}

	/**
	 * Gets the index of this atom symbol in the AtomInfo tables.
	 *
	 * @return the corresponding number of this atom in the atominfo tables
	 */
	public int getAtomicListNumber() {
//    System.out.println("AtomSite " + getAtomSymbol() + ", atomic number: " + atomListNumber);
		return atomListNumber;
	}

	/**
	 * Gets the atomic number of the atom
	 *
	 * @return the atomic number
	 */
	public int getAtomicNumber() {
//    System.out.println("AtomSite " + getAtomSymbol() + ", atom number: " + atomNumber);
		return atomNumber;
	}

	/**
	 * Gets the index of this atom symbol in the AtomInfo tables.
	 *
	 * @return the corresponding number of this atom in the atominfo tables
	 */
	public int getOxidationNumber() {
//    System.out.println("AtomSite " + getAtomSymbol() + ", atomic number: " + atomListNumber);
		return oxidationNumber;
	}

	/**
	 * Gets the index of this isotope in the AtomInfo tables.
	 *
	 * @return the corresponding number of this isotope in the atominfo tables
	 */
	public int getIsotopicListNumber() {
//    System.out.println("AtomSite " + getAtomSymbol() + ", isotopic number: " + isotopeListNumber);
		return isotopeListNumber;
	}

	public double[] scatfactor(double dspacing, Radiation rad) {

		double[] fu = new double[2];
		int atomicListNumber = getAtomicListNumber();
		if (rad.isElectron()) {
			// electron radiation
			fu[0] = 0.023934 * (getOxidationNumber()) * (4 * dspacing * dspacing);
			fu[1] = 0;
			if (dspacing == 0)
				for (int j = 0; j < 5; j++)
					fu[0] += Radiation.electronSF[atomicListNumber][j];
			else
				for (int j = 0; j < 5; j++)
					fu[0] += Radiation.electronSF[atomicListNumber][j] * Math.exp(-Radiation.electronSF[atomicListNumber][j + 4] /
							(4 * dspacing * dspacing));
		} else if (rad.isNeutron()) {
			// neutron radiation
			fu[0] = Radiation.neutronSF[getIsotopicListNumber()];
			fu[1] = 0;
		}
		return fu;
	}
  
  public double[] scatfactor(double dspacing, double energyInKeV) {
    
    int atomicListNumber = getAtomicListNumber();
      // x-ray radiation
      int atomNumber = getAtomicNumber();
    double[] fu = XRayDataSqLite.getF1F2FromHenkeForAtomAndEnergy(atomNumber, energyInKeV);
      fu[0] -= atomNumber;
      fu[0] += Radiation.xraySF[atomicListNumber][8];
      if (dspacing == 0)
        for (int j = 0; j < 4; j++)
          fu[0] += Radiation.xraySF[atomicListNumber][j];
      else
        for (int j = 0; j < 4; j++)
          fu[0] += Radiation.xraySF[atomicListNumber][j] * Math.exp(-Radiation.xraySF[atomicListNumber][j + 4] /
              (4 * dspacing * dspacing));
    return fu;
  }
  
  /**
	 * Gets the X-ray scattering factor at zero deg or the number of electrons of this atom
	 * equal to: atomNumber - oxidation number
	 *
	 * @return the number of electrons of this atom
	 */
	public double xrayscatfactor() {
		return atomNumber - getOxidationNumber();
	}

	/**
	 * Gets the magnetic scattering factor for this atom.
	 *
	 * @param dspacing the d-spacing of the reflex for which the magnetic scattering factor is returned
	 * @param radType  the Radiation object type
	 * @return the magnetic scattering factor
	 */
	public double magneticScatfactor(double dspacing, int radType) {

		double fu = 0;
		switch (radType) {
			case 0:
			case 1:
			case 2:
			case 5:
			case 6:
				break;
			case 3:
			case 4:
				fu = Radiation.magneticSF[getAtomicListNumber()][6];
				for (int j = 0; j < 3; j++)
					fu += Radiation.magneticSF[getAtomicListNumber()][j * 2] * Math.exp(-Radiation.magneticSF[getAtomicListNumber()][j * 2 + 1] /
							(4 * dspacing * dspacing));
				break;
			default: {
			}
		}
		return fu;
	}

}
