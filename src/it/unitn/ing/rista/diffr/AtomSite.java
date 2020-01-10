/*
* @(#)Atom.java created 01/01/1997 Mesiano
*
* Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr;

import it.unitn.ing.rista.chemistry.AtomInfo;
//import it.unitn.ing.rista.chemistry.XRayDataSqLite;
import it.unitn.ing.rista.util.*;

import java.util.ArrayList;
import java.util.Vector;

/**
 * The AtomSite is a class that implements an atom and perform all the task the
 * atom need to do, store scattering factors and coordinates.
 * Each atom belong to a <code>Phase</code>.
 * See the atominfo.cif for all atom (and symbols) recognized by Maud and their default values.
 * There is a copy in the database and preferences files folder created by Maud at the first run.
 * See also the <code>AtomInfo</code> class responsible of loading the atomic tables from atominfo.cif
 * (either the one in the files.jar archive by default or the alternate one in the user files
 * preferences and databases folder of Maud based on the specific preference).
 * For some properties multiple tables exist in the file, but only one is loaded.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.34 $, $Date: 2006/12/04 14:30:03 $
 * @see AtomInfo
 * @see Phase
 * @since JDK1.1
 */

public class AtomSite extends XRDcat {

  private static final int xcoordNumber = 1, ycoordNumber = 2, zcoordNumber = 3;

  public boolean trowException = false;
  public boolean useThisAtom = true;
	private boolean refreshPositions = true;

  /**
   * CIF strings recognized by this object as fields, parameters or subordinate objects
   */
  protected static String[] diclistc = {
		  "_atom_site_type_symbol", "_atom_site_constraints", "_atom_type_number_in_cell",
		  "_atom_site_calc_flag",

		  "_atom_site_occupancy",
		  "_atom_site_fract_x", "_atom_site_fract_y", "_atom_site_fract_z",
		  "_atom_site_B_iso_or_equiv",
		  "_atom_site_aniso_B_11", "_atom_site_aniso_B_22", "_atom_site_aniso_B_33",
		  "_atom_site_aniso_B_12", "_atom_site_aniso_B_13", "_atom_site_aniso_B_23",

		  "_rg_site_scatterer"
  };
  protected static String[] diclistcrm = {
		  "_atom_site_type_symbol", "_atom_site_constraints", "_atom_type_number_in_cell",
		  "_atom_site_calc_flag",

		  "site occupancy (fraction)",
		  "x position (fraction)", "y position (fraction)", "z position (fraction)",
		  "B or U factor (isotropic, angstrom^2)",
		  "B or U 11 (anisotropic)", "B or U 22 (anisotropic)", "B or U 33 (anisotropic)",
		  "B or U 12 (anisotropic)", "B or U 13 (anisotropic)", "B or U 23 (anisotropic)",

		  "scatterer element"
      };

  /**
   * Classnames recognized by this object as subordinate objects
   */
  protected static String[] classlistc = {"superclass:it.unitn.ing.rista.diffr.AtomScatterer"};

  //	boolean anisoatom = false;
	public static int scattererLoopID = 0;
	public static String dummyPlaceHolder = "dummy";

  /**
   * Vector containing all the coordinates for all site equivalent positions
   */
  private Vector<Coordinates> atomcoordinates = new Vector<Coordinates>(10, 10);
	private Vector<SitePosition> generalSitePositions = new Vector<SitePosition>(10, 10);
	private Vector<SitePosition> equivalentSitePositions = new Vector<SitePosition>(10, 10);
  private double[][] atomCoordQuick = null;
  private int atomCoordNumber = 0;
	public int symmetryMatrixIndex = -1;

  /**
   * List of equivalent positions in cartesian coordinates
   */
  private ArrayList cartesianCoords;

  /**
   * The atomic weight
   */
  private double meanWeight = 0;

  /**
   * The atomic radius
   */
  private double meanRadius = 1.0;

  /**
   * The atomic number
   */
//  private int atomNumber = 1;

  /**
   * The corresponding number of this atom in the atominfo tables
   */
//  private int atomListNumber = 1;

//  private int oxidationNumber = 0;

  /**
   * The corresponding number of this isotope in the atominfo tables
   */
//  private int isotopeListNumber = 1;

  /**
   * The atom quantity in the cell = occupancy * site multeplicity
   */
  private double quantity;

  /**
   * The special coordinates number, used to recognized special positions
   */
  public static final double[] coordSpecial = {.5, .25, .75, .125, .375, .625, .875,
      .3333333333333, .666666666667, .16666666666667, 8.33333333333E-01,
      8.33333333333E-02, 4.16666666667E-01, 5.83333333333E-01, 9.16666666667E-01};

  /**
   * The number of principal special coordinates numbers
   */
  public static final int numberSpecialPosition = 7;

  /**
   * Number of all special coordinates numbers
   */
  public static final int numberSpecialPositionTH = coordSpecial.length;

  /**
   * The constructor to be used for this class when passing a specific atom site label
   *
   * @param obj    the parent object that store the atoms list
   * @param alabel the site label for this atom
   */
  public AtomSite(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
  }

  /**
   * The constructor for this class to be used when a site label is not know, a default
   * one will be set.
   *
   * @param parent the parent object creating and storing this object
   */
  public AtomSite(XRDcat parent) {
    this(parent, "Site_x");
  }

  public AtomSite() {}

  /**
   * Init the default fields of this object, see XRDcat for this.
   */
  public void initConstant() {
    Nstring = 4;
    Nstringloop = 0;
    Nparameter = 11;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = classlistc.length;
  }

  /**
   * Init the CIF dictionary of this object, see XRDcat for this.
   */
  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  /**
   * Init the default parameters/fileds/subordinate objects when the object is created first time,
   * see XRDcat.
   */
  public void initParameters() {
    super.initParameters();
    getOccupancy().setValue(1.0);
	  setDummy(true);
	  stringField[0] = dummyPlaceHolder;
    setQuantity("1.0");
    parameterField[0].setPositiveOnly();
    for (int i = 0; i < 4; i++) {
      parameterField[i].setValueMin(ParameterPreferences.getDouble(parameterField[i].getLabel() + ".min",
          0.0));
      parameterField[i].setValueMax(ParameterPreferences.getDouble(parameterField[i].getLabel() + ".max",
          1.0));
    }
    for (int i = 4; i < 11; i++) {
      parameterField[i].setValueMin(ParameterPreferences.getDouble(parameterField[i].getLabel() + ".min",
          -1.0));
      parameterField[i].setValueMax(ParameterPreferences.getDouble(parameterField[i].getLabel() + ".max",
          10.0));
    }
    stringField[3] = ".";
  }

/*  public XRDcat getCopy(XRDcat parent) {
    AtomSite atom_copy = (AtomSite) super.getCopy(parent);
    if (atom_copy != null)
      atom_copy.loadAtomProperties();
    return atom_copy;
  }*/

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
//      System.out.println("SetField, starting symbol: " + stringField[0]);
	    // now is dummy, the atom symbol should go in a subordinate loop objects list
	    if (!stringField[0].toLowerCase().startsWith("dum")) {
//		    System.out.println(stringField[0]);
		    addsubordinateloopField(scattererLoopID, new AtomScatterer(this, checkAtomSymbol(checkOxidationNumber(
				    stringField[0]), getSiteLabel())));
		    stringField[0] = dummyPlaceHolder;
	    }
//      System.out.println("SetField, final symbol: " + stringField[0]);
//      loadAtomProperties();
    }
    refreshQuantityD();  // to be sure will be refreshed at a certain point
    return index;
  }

  /**
   * Executed when a Parameter of this object is changed. Overwrite the same method in XRDcat.
   * In this case add to the message that an atom position has changed flag if it is the case.
   *
   * @param source the Parameter that has changed its value
   */
  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < parameterField.length; i++) {
        if (parameterField[i] == source) {
          if (i > 0 && i < 4) {
            notifyParameterChanged(source, Constants.ATOM_POSITION_CHANGED);
	          refreshPositions = true;
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
            return;
          } else {
            notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
            return;
          }
        }
      }
      super.notifyParameterChanged(source);
    }
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    super.updateStringtoDoubleBuffering(firstLoading);
	  if (getNumberOfScatterers() == 0)
	  	addAtomWithSymbol(checkAtomSymbol("", getSiteLabel()));
/*	  String oldSymbol = getAtomSymbol();
//    System.out.println("Update starting symbol: " + oldSymbol);
    stringField[0] = checkAtomSymbol(checkOxidationNumber(oldSymbol), getSiteLabel());
//    System.out.println("Update final symbol: " + stringField[0]);
//    if (!stringField[0].equalsIgnoreCase(oldSymbol))
    loadAtomProperties();*/
    useThisAtom = !isDummyAtom();
  }

  public boolean isDummyAtom() {
    return stringField[3].toLowerCase().startsWith("dum");  //To change body of created methods use File | Settings | File Templates.
  }

  public void setDummy(boolean dummy) {
    if (dummy)
      stringField[3] = "dummy";
    else
      stringField[3] = ".";
  }

  public void updateParametertoDoubleBuffering(boolean firstLoading) {
    // to be implemented by subclasses

    if (getFilePar().isLoadingFile() || !isAbilitatetoRefresh)
      return;
    super.updateParametertoDoubleBuffering(false);

    parameterField[0].setPositiveOnly();

	 getTotalOccupancy();
  }


  /**
   * Use this to get the actual complete symbol of this atom in Maud convention. E.g. AG2+ etc.
   *
   * @return the atom symbol
	*/

  public String getFirstAtomSymbol() {
    return getAtomScatterer(0).getAtomSymbol();
  }

	public int getNumberOfScatterers() {
		return subordinateloopField[scattererLoopID].size();
	}

	public AtomScatterer getAtomScatterer(int index) {
		return (AtomScatterer) subordinateloopField[scattererLoopID].elementAt(index);
	}

  /**
   * Set the atom symbol. It is then used to retrieve all the atom characteristics, from
   * scattering factors to weight, absorption etc. The Maud convention is as in the following examples:
   * AG2+, AG, H, O2-
   * but the method can accept also different format as the symbol will be correctly converted.
   * Other format accepted:
   * Ag2+, ag+2, Ag, h, O-2
   *
   * @param alabel the symbol to be set

  public void setAtomSymbol(String alabel) {
    if (!alabel.equals(stringField[0])) {
//      System.out.println("Starting symbol as label: " + alabel);
      stringField[0] = checkAtomSymbol(checkOxidationNumber(alabel), getSiteLabel());
//      System.out.println("Final symbol as label: " + stringField[0]);
//      stringField[0] = alabel;
      getPhaseParent().refreshAtoms = true;
      loadedSymbol = null;
      loadAtomProperties();
    }
  }*/

  /**
   * Return the oxidation number of this atom. Derived from the symbol label.
   *
   * @return the actual oxidation number
   */
  public static int getOxidationNumber(String atomSymbol) {
    return retrieveOxidationNumber(atomSymbol);
  }

  public static int retrieveOxidationNumber(String atomSymbol) {
    int sign = 1;
    String oxidation = AtomInfo.getOxidationNumber(atomSymbol);
    if (oxidation.endsWith("+")) {
      oxidation = oxidation.substring(0, oxidation.length() - 1);
    } else if (oxidation.endsWith("-")) {
      oxidation = oxidation.substring(0, oxidation.length() - 1);
      sign = -1;
    }
    if (oxidation != null && !oxidation.equals(""))
      return sign * Integer.valueOf(oxidation).intValue();
    else
      return 0;
  }

  /**
   * Check the oxidation number in the symbol for correctness. In particular if
   * the -/+ sign is before or after the oxidation number and return the atom label
   * with the oxidation in the correct Maud convention (at the moment Maud uses the
   * sign after the number).
   *
   * @param symbol the symbol to be checked
   * @return the symbol with the corrected Maud convention for oxidation
   */
  public static String checkOxidationNumber(String symbol) {
    int length = symbol.length();
    if (length == 0)
      return symbol;
    int signPosition = -1;
    for (int i = 0; i < length; i++) {
      char tmp = symbol.charAt(i);
      if (tmp == '+' || tmp == '-') {
        signPosition = i;
        break;
      }
    }
    int numberPosition = -1;
    int i = 0;
    while (Character.isDigit(symbol.charAt(i))) {
      i++;
    }
    for (; i < length; i++) {
      char tmp = symbol.charAt(i);
      if (Character.isDigit(tmp)) {
        numberPosition = i;
        break;
      }
    }
    if (numberPosition == signPosition + 1) {
      int endNumberPosition = numberPosition;
      for (i = numberPosition + 1; i < length; i++) {
        char tmp = symbol.charAt(i);
        if (Character.isDigit(tmp)) {
          endNumberPosition = i;
          break;
        }
      }
      StringBuffer tmp = new StringBuffer(symbol);
      for (i = numberPosition; i < endNumberPosition + 1; i++) {
        tmp.setCharAt(i - 1, symbol.charAt(i));
      }
      tmp.setCharAt(endNumberPosition, symbol.charAt(signPosition));
      return tmp.toString();
    } else
      return symbol;
  }

  /**
   * Check the atom symbol for correctness of the upper-lowercase letters. Maud prefers
   * the symbol with all uppercase letters.
   *
   * @param symbol the symbol to be checked
   * @return the symbol with the corrected Maud convention for upper-lowercase letters
   */
  public static String checkUpperLowerCase(String symbol) {
    boolean first = true;
    StringBuffer tmp = new StringBuffer(symbol);
    for (int i = 0; i < symbol.length(); i++) {
      Character ch = symbol.charAt(i);
      if (!Character.isDigit(ch) && ch != '+' && ch != '-') {
        if (first) {
          tmp.setCharAt(i, Character.toUpperCase(ch));
          first = false;
        } else {
          tmp.setCharAt(i, Character.toLowerCase(ch));
        }
      }
    }
    return tmp.toString();
  }

  /**
   * Check if the symbol exists in the database (see atominfo.cif), if yes return it
   * in the correct Maud convention, otherwise return the default (H).
   *
   * @param symbol the symbol to be checked
   * @return the recognized atom symbol
   */
  public static String checkAtomSymbol(String symbol, String siteLabel) {
//  	System.out.println("Symbol and siteLabel: " + symbol + " " + siteLabel);
    if (symbol.length() < 1)
      return tryFromLabel(siteLabel);
    symbol = checkUpperLowerCase(symbol);
    // first check for the plain symbol
    if (!symbol.startsWith("Dy") && symbol.startsWith("D"))
      symbol = "2H" + symbol.substring(1, symbol.length());

    if (AtomInfo.getAtomNumber(symbol) != -1)
      return symbol;
    if (AtomInfo.getIsotopeNumber(stripOxidation(symbol)) != -1 && AtomInfo.getAtomNumber(stripIsotopeNumber(symbol)) != -1)
      return symbol;
    if (Character.isDigit(symbol.charAt(0))) { // probably isotope
      if (AtomInfo.getIsotopeNumber(stripOxidation(symbol)) != -1)
        return symbol;
      int posFinal = symbol.length() - 1;
      while (posFinal > 1) {
        String temp = symbol.substring(0, posFinal);
        if (AtomInfo.getIsotopeNumber(temp) != -1)
          return temp;
        posFinal--;
      }
      symbol = stripIsotopeNumber(symbol);
    }
    if (AtomInfo.getAtomNumber(symbol) != -1)
      return symbol;
    int posFinal = symbol.length() - 1;
    while (posFinal > 0) {
      String temp = symbol.substring(0, posFinal);
      if (AtomInfo.getAtomNumber(temp) != -1)
        return temp;
      posFinal--;
    }
    return tryFromLabel(siteLabel);
  }

  public static String tryFromLabel(String symbol) {
    if (symbol.length() < 1)
      return "H";
    else if (symbol.toLowerCase().startsWith("wat"))
      return "O2-";
    else if (symbol.toLowerCase().startsWith("d") && !symbol.toLowerCase().startsWith("dy"))
      return "2H";
    else
      return checkAtomSymbol(symbol, "");
  }

  /**
   * The site label is a conventional string to identify a certain site occupied
   * by a certain atom. It just a string assigned by the user.
   * Conventionally the atom symbol followed by a progressive
   * number (for more sites with the same atom) should be used.
   *
   * @return the site label
   */

  public String getSiteLabel() {
    return toXRDcatString();
  }

  /**
   * See getSiteLabel() for more.
   *
   * @param alabel the site label to be set
   */
  public void setSiteLabel(String alabel) {
    setLabel(alabel);
  }

  /**
   * The quantity is the total amount of this atom in the cell and should be equal to the
   * occupation times the site multeplicity.
   *
   * @return The total quantity of this atom in the entire cell as a String.
   */
  public String getQuantity() {
    return stringField[2];
  }

  /**
   * The quantity is the total amount of this atom in the cell and should be equal to the
   * occupation times the site multeplicity.
   *
   * @return The total quantity of this atom in the entire cell as a number.
   */
  public double getQuantityD() {
    return quantity;
  }

  /**
   * For computation speed reasons the atom quantity is converted from its field (a String)
   * to its cached number value to be used in the computation and avoid the String convertion
   * during computation.
   */

  private void refreshQuantityD() {
    quantity = Double.valueOf(getQuantity()).doubleValue();
  }

  /**
   * Set the quantity field. And refresh the cached number value.
   *
   * @param quantity the quantity to be set.
   */
  public void setQuantity(String quantity) {
    stringField[2] = quantity;
    refreshQuantityD();
  }

  /**
   * Use this to get the occupancy Parameter, not its value.
   *
   * @return the occupancy parameter of this atom
   */
  public Parameter getOccupancy() {
    return parameterField[0];
  }

  /**
   * Use this to get the occupancy value, not its Parameter.
   *
   * @return the oocupancy actual value converted from the Parameter.
   */
  public double getOccupancyValue() {
    return getParameterValue(0);
  }

  /**
   * Gets the Parameter of the x fractional coordinate of this atom.
   *
   * @return the x fractional coordinate Parameter.
   */
  public Parameter getLocalCoordX() {
    return parameterField[xcoordNumber];
  }

  /**
   * Gets the Parameter of the y fractional coordinate of this atom.
   *
   * @return the y fractional coordinate Parameter.
   */
  public Parameter getLocalCoordY() {
    return parameterField[ycoordNumber];
  }

  /**
   * Gets the Parameter of the z fractional coordinate of this atom.
   *
   * @return the z fractional coordinate Parameter.
   */
  public Parameter getLocalCoordZ() {
    return parameterField[zcoordNumber];
  }

  public Coordinates getLocalCoordinates() {
    return new Coordinates(getLocalCoordX().getValueD(), getLocalCoordY().getValueD(), getLocalCoordZ().getValueD());
  }

  /**
   * Gets the Parameter of one fractional coordinate of this atom.
   *
   * @param index index of the fractional coordinate Parameter to be retrieved, 0 for x, 1 for y and 2 for z
   * @return the fractional coordinate Parameter for the index.
   */
  protected double getLocalCoordValue(int index) {
    return parameterValues[1 + index];
  }

  /**
   * Gets the isotropic B factor Parameter.
   *
   * @return the isotropic B factor Parameter.
   */
  public Parameter getBfactor() {
    return parameterField[4];
  }

  /**
   * Gets the isotropic B factor number value from its cached number.
   *
   * @return the isotropic B factor number value.
   */
  public double getBfactorValue() {
    return parameterValues[4];
  }

  /**
   * Gets the anisotropic B factor Parameter corresponding to the index passed. The
   * index goes from 0 for the first (B11) parameter to 5 for the last one (B12). They are in the following order:
   * B11
   * B22
   * B33
   * B23
   * B13
   * B12
   *
   * @param index the number of the anisotropic B factor to be retrieved.
   * @return the anisotropic B factor Parameter corresponding to index.
   */
  public Parameter getAnisoBfactor(int index) {

    // 1 is the first index

    return parameterField[5 + index];
  }

  /**
   * Gets the anisotropic B factor number value corresponding to the index passed. The
   * index goes from 0 for the first (B11) parameter to 5 for the last one (B12). They are in the following order:
   * B11
   * B22
   * B33
   * B23
   * B13
   * B12
   *
   * @param index the number of the anisotropic B factor to be retrieved.
   * @return the anisotropic B factor cached number value corresponding to index.
   */
  public double getAnisoBfactorValue(int index) {

    // 1 is the first index

    return parameterValues[5 + index];
  }

  /**
   * Calculates real cartesian coordinates from fractional coordinates for use in force field computation
   * and structure visualization. The cartesian coordinates are stored in the cartesianCoords field.
   * All equivalent positions included.
   */
  public void computeCartesianCoords(boolean entireCell) {

    Phase aphase = getPhaseParent();
    double a = aphase.getCellValue(0);
    double b = aphase.getCellValue(1);
    double c = aphase.getCellValue(2);
    double cosalpha = Math.cos(aphase.getCellValue(3) * Constants.DEGTOPI);
    double cosbeta = Math.cos(aphase.getCellValue(4) * Constants.DEGTOPI);
    double cosgamma = Math.cos(aphase.getCellValue(5) * Constants.DEGTOPI);
    double singamma = Math.sin(aphase.getCellValue(5) * Constants.DEGTOPI);
    double V = aphase.getCellVolume();

    cartesianCoords = new ArrayList();

    if (entireCell) {
      int startPoint = 0;
      int endPoint = 1;
      if (entireCell) {
        startPoint = -1;
        endPoint = 2;
      }
      double lowx = MaudPreferences.getDouble("cellPlotting.lowerFractionalCoordinate", -0.05);
      double highx = MaudPreferences.getDouble("cellPlotting.higherFractionalCoordinate", 1.05);

      for (int i = 0; i < atomcoordinates.size(); i++) {
        Coordinates fcoord = atomcoordinates.elementAt(i);
        for (int g = startPoint; g < endPoint; g++) {
          for (int j = startPoint; j < endPoint; j++) {
            for (int k = startPoint; k < endPoint; k++) {
              double x1 = fcoord.x + g;
              double y1 = fcoord.y + j;
              double z1 = fcoord.z + k;
              if (x1 > lowx && x1 < highx)
                if (y1 > lowx && y1 < highx)
                  if (z1 > lowx && z1 < highx) {
                    double x = a * x1 + b * cosgamma * y1 + c * cosbeta * z1;
                    double y = b * singamma * y1 + c * ((cosalpha - cosbeta * cosgamma) / singamma) * z1;
                    double z = (V / (a * b * singamma)) * z1;
                    cartesianCoords.add(i, new Coordinates(x, y, z));
                  }
            }
          }
        }
      }
    } else {

      for (int i = 0; i < atomcoordinates.size(); i++) {
        Coordinates fcoord = atomcoordinates.elementAt(i);

        double x = a * (fcoord.x) + b * cosgamma * (fcoord.y) + c * cosbeta * (fcoord.z);
        double y = b * singamma * (fcoord.y) + c * ((cosalpha - cosbeta * cosgamma) / singamma) * (fcoord.z);
        double z = (V / (a * b * singamma)) * (fcoord.z);

        cartesianCoords.add(i, new Coordinates(x, y, z));
      }
    }
  }

  /**
   * Gets an ArrayList containing all the cartesian coordinates for cell plotting purpose. All
   * equivalent positions included. If the entire cell is required it computes all the atoms positions
   * comprised in the cell from min to max in fractional coordinates where min and max are retrieved from the
   * two Maud preferences:
   * cellPlotting.lowerFractionalCoordinate
   * cellPlotting.higherFractionalCoordinate
   * Default values for min, max are -0.05 and 1.05
   *
   * @param entireCell true if the entire cell is needed, false if only the repeated cell unit is required.
   * @return the list of cartesion coordinates. Each atomic position inside the list is stored as a Coordinates object.
   */
  public ArrayList getCartesianCoords(boolean entireCell) {
    computeCartesianCoords(entireCell);
    return cartesianCoords;
  }

  /**
   * Gets the Phase to which this atom belong.
   *
   * @return the Phase parent of this atom.
   */
  protected Phase getPhaseParent() {
    Object aparent = getParent();
    while (aparent != null && !(aparent instanceof Phase))
      aparent = ((XRDcat) aparent).getParent();
    return (Phase) aparent;
  }

  public void switchAandB() {
    String label1 = getLocalCoordX().getLabel();
    String label2 = getLocalCoordY().getLabel();
    Parameter temp = getLocalCoordY();
    parameterField[ycoordNumber] = getLocalCoordX();
    parameterField[ycoordNumber].setLabel(label2);
    parameterField[ycoordNumber].editingField = getLocalCoordX().editingField;
    parameterField[xcoordNumber] = temp;
    parameterField[xcoordNumber].setLabel(label1);
    parameterField[xcoordNumber].editingField = temp.editingField;
  }

  public void switchBandC() {
    String label1 = getLocalCoordZ().getLabel();
    String label2 = getLocalCoordY().getLabel();
    Parameter temp = getLocalCoordY();
    parameterField[ycoordNumber] = getLocalCoordZ();
    parameterField[ycoordNumber].setLabel(label2);
    parameterField[ycoordNumber].editingField = getLocalCoordZ().editingField;
    parameterField[zcoordNumber] = temp;
    parameterField[zcoordNumber].setLabel(label1);
    parameterField[zcoordNumber].editingField = temp.editingField;
  }

  public void switchCandA() {
    String label1 = getLocalCoordX().getLabel();
    String label2 = getLocalCoordZ().getLabel();
    Parameter temp = getLocalCoordZ();
    parameterField[zcoordNumber] = getLocalCoordX();
    parameterField[zcoordNumber].setLabel(label2);
    parameterField[zcoordNumber].editingField = getLocalCoordX().editingField;
    parameterField[xcoordNumber] = temp;
    parameterField[xcoordNumber].setLabel(label1);
    parameterField[xcoordNumber].editingField = temp.editingField;
  }

  public void invertA() {
    parameterField[xcoordNumber].setValue(1.0 - parameterField[xcoordNumber].getValueD());
  }

  public void invertB() {
    parameterField[ycoordNumber].setValue(1.0 - parameterField[ycoordNumber].getValueD());
  }

  public void invertC() {
    parameterField[zcoordNumber].setValue(1.0 - parameterField[zcoordNumber].getValueD());
  }

  public void turnForward() {
    String label1 = getLocalCoordX().getLabel();
    String label2 = getLocalCoordY().getLabel();
    String label3 = getLocalCoordZ().getLabel();
    Parameter temp = getLocalCoordZ();
    parameterField[zcoordNumber] = getLocalCoordY();
    parameterField[zcoordNumber].setLabel(label3);
    parameterField[zcoordNumber].editingField = getLocalCoordY().editingField;
    parameterField[ycoordNumber] = getLocalCoordX();
    parameterField[ycoordNumber].setLabel(label2);
    parameterField[ycoordNumber].editingField = getLocalCoordX().editingField;
    parameterField[xcoordNumber] = temp;
    parameterField[xcoordNumber].setLabel(label1);
    parameterField[xcoordNumber].editingField = temp.editingField;
  }

  public void turnBackward() {
    String label1 = getLocalCoordX().getLabel();
    String label2 = getLocalCoordY().getLabel();
    String label3 = getLocalCoordZ().getLabel();
    Parameter temp = getLocalCoordX();
    parameterField[xcoordNumber] = getLocalCoordY();
    parameterField[xcoordNumber].setLabel(label1);
    parameterField[xcoordNumber].editingField = getLocalCoordY().editingField;
    parameterField[ycoordNumber] = getLocalCoordZ();
    parameterField[ycoordNumber].setLabel(label2);
    parameterField[ycoordNumber].editingField = getLocalCoordZ().editingField;
    parameterField[zcoordNumber] = temp;
    parameterField[zcoordNumber].setLabel(label3);
    parameterField[zcoordNumber].editingField = temp.editingField;
  }

	private final static double[][] b_Inv = {
			{0.023, 0.017, 0.031},
			{0.017, 0.021, 0.027},
			{0.031, 0.027, 0.036},
	};

  /**
   * Refresh the array atomcoordinates with all the equivalent position coordinates computed from the atom coordinated
   * and the equivalent positions given by the space group. Each entry in the array is an object of the class
   * Coordinates. Also the array of cartesian coordinates is refreshed.
   */
  public void refreshPositions(boolean cartesianAlso) {
//	  System.out.println(toString() + " refresh atom positions: " + refreshPositions);
	  if (refreshPositions) {
		  refreshPositions = false;
		  double[] xf = new double[3];

		  for (int i = 0; i < 3; i++)
			  xf[i] = getLocalCoordValue(i);

		  try {
			  computePositions(xf);
		  } catch (Exception e) {
			  e.printStackTrace();
		  }
		  if (cartesianAlso)
			  computeCartesianCoords(false);

	  }
  }

	public void setAnisotropicBfactors() {
		double[] b1 = getInvariantMatrixOfBFactors(b_Inv);
		int newMatrixIndex = getSymmetryRestrictionMatrixNumberFromInvariantMatrix(b1);
		if (symmetryMatrixIndex != newMatrixIndex) {
			symmetryMatrixIndex = newMatrixIndex;
			setRulesForAnisotropicBfactors(symmetryMatrixIndex);
		}
		if (Constants.testing)
			System.out.println("AtomSite " + getSiteLabel() + ": symmetry matrix B factors: B" + (symmetryMatrixIndex + 1));
	}

	public void setRefreshPosition(boolean value) {
		refreshPositions = value;
	}

		/**
		 * Refresh all equivalent positions based on the dx, dy, dz coordinates of one generator.
		 *
		 * @param dx x fractional coordinate of the generator position
		 * @param dy y fractional coordinate of the generator position
		 * @param dz z fractional coordinate of the generator position
		 */
  public void refreshPositions(double dx, double dy, double dz) {
    double[] xf = new double[3];

    xf[0] = getLocalCoordValue(0) + dx;
    xf[1] = getLocalCoordValue(1) + dy;
    xf[2] = getLocalCoordValue(2) + dz;

    try {
      computePositions(xf);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Compute all the equivalent atomic positions starting from one and store them in atomcoordinates
   *
   * @param xf the starting position
   */
  private void computePositions(double[] xf) {

    xf = transformLocalCoordToAbsolute(xf);
    atomcoordinates.removeAllElements();
	  generalSitePositions.removeAllElements();
	  equivalentSitePositions.removeAllElements();
    Phase aphase = getPhaseParent();
    SitePosition sitepos;
    double x[] = new double[3];
    for (int i = 0; i < aphase.getPhaseInfo().getSitePositionNumber(); i++) {
      sitepos = aphase.getPhaseInfo().getSitePosition(i);
      for (int j = 0; j < 3; j++)
        x[j] = sitepos.getcoord(j, xf);
      boolean flagx = true;
	    boolean flagy = false;
      for (int kx = 0; kx < atomcoordinates.size() && flagx; kx++) {
        boolean flagx1 = true;
        Coordinates coord = atomcoordinates.elementAt(kx);
        double diff1 = Math.abs(x[0] - coord.x);
        double diff2 = Math.abs(x[1] - coord.y);
        double diff3 = Math.abs(x[2] - coord.z);
        if (diff1 > Constants.TOOLERANCE_COORD && diff1 < 1.0 - Constants.TOOLERANCE_COORD)
          flagx1 = false;
        if (diff2 > Constants.TOOLERANCE_COORD && diff2 < 1.0 - Constants.TOOLERANCE_COORD)
          flagx1 = false;
        if (diff3 > Constants.TOOLERANCE_COORD && diff3 < 1.0 - Constants.TOOLERANCE_COORD)
          flagx1 = false;
        if (flagx1) {
	        flagx = false;
	        if (kx == 0)
		        flagy = true;
        }
      }
      if (flagx) {
	      atomcoordinates.addElement(new Coordinates(x[0], x[1], x[2]));
	      generalSitePositions.addElement(sitepos);
      }
	    if ((flagy && !flagx) || i == 0)
		    equivalentSitePositions.addElement(sitepos);
    }
    atomCoordNumber = atomcoordinates.size();
    atomCoordQuick = new double[atomCoordNumber][3];
    for (int i = 0; i < atomCoordNumber; i++) {
      atomCoordQuick[i][0] = atomcoordinates.elementAt(i).x;
      atomCoordQuick[i][1] = atomcoordinates.elementAt(i).y;
      atomCoordQuick[i][2] = atomcoordinates.elementAt(i).z;
    }
  }

  public double[][] getQuickAtomCoordinates() {
    return atomCoordQuick;
  }

  public int getQuickAtomCoordinatesNumber() {
    return atomCoordNumber;
  }

  /**
   * Compute positions (call the refreshPositions() method) and check for equivalent positions
   * too close. If too close it collapse them in a more symmetric position and changes the
   * coordinates of this atom for this new position. It also compute the new occupation value mantaining
   * the same quantity.
   */
  public void collapsePositions() {
    refreshPositions(false);
    int sitePosNumber = getSiteMultiplicity();
    Coordinates coordFirst = atomcoordinates.elementAt(0);
    int wt = 1;
    for (int i = 1; i < sitePosNumber; i++) {
      Coordinates coordNext = atomcoordinates.elementAt(i);
      if (tooShortDistance(coordFirst, coordNext))
        collapse(coordFirst, wt++, coordNext, 1);
    }
    transformAbsoluteCoordToLocal(coordFirst);
    setLocalCoordinates(coordFirst);
    checkSmallerCoordinates();
    //    int newSitePosNumber = getSiteMultiplicity();
    //    double newOccupancy = sitePosNumber / newSitePosNumber;
    computeOccupancyFromQuantity();
    //    getOccupancy().setValue(getOccupancy().getValueD() * newOccupancy);
  }

  public boolean equalsByDistance(Object obj) {
    double minDistance = MaudPreferences.getDouble("atomCheckEquivalence.minDistance", 0.01);
    if (obj instanceof AtomSite) {
      AtomSite ato2 = (AtomSite) obj;
      if (equalsByAtomSymbol(ato2)) {
        for (int np1 = 0; np1 < getCartesianCoords().size(); np1++) {
          Coordinates c1 = getCartesianCoords(np1);
          for (int np2 = 0; np2 < ato2.getCartesianCoords().size(); np2++) {
            Coordinates c2 = ato2.getCartesianCoords(np2);
            double dist = Math.sqrt(Math.pow(c1.x - c2.x, 2) + Math.pow(c1.y - c2.y, 2) +
                          Math.pow(c1.z - c2.z, 2));
            if (dist < minDistance)
              return true;
          }
        }
      }
    }
    return false;
  }

	public boolean equalsByAtomSymbol(AtomSite ato) {
		if (getNumberOfScatterers() > 0 &&
				getNumberOfScatterers() == ato.getNumberOfScatterers())
			if (getAtomScatterer(0).getAtomSymbol().equalsIgnoreCase(ato.getAtomScatterer(0).getAtomSymbol()))
				return true;
		return false;
	}

  public void transformAbsoluteCoordToLocal(Coordinates coordFirst) {
    XRDcat parent = getParent();
    if (parent instanceof Fragment) {
      coordFirst = getAbsoluteCartesianFromLocalCoordinate(coordFirst);
      ((Fragment) parent).calcRelativeCoords(coordFirst);
    }
  }

  public void transformLocalCoordToAbsolute(Coordinates coord) {
    XRDcat parent = getParent();
    if (parent instanceof Fragment) {
      coord = ((Fragment) parent).calcAbsoluteCoords(coord);
      coord = getLocalFromAbsoluteCartesianCoordinate(coord);
    }
  }

  public double[] transformAbsoluteCoordToLocal(double[] coord) {
    XRDcat parent = getParent();
    if (parent instanceof Fragment) {
      coord = getAbsoluteCartesianFromLocalCoordinate(coord);
      return ((Fragment) parent).calcRelativeCoords(coord);
    }
    return coord;
  }

  public double[] transformLocalCoordToAbsolute(double[] coord) {
    XRDcat parent = getParent();
    if (parent instanceof Fragment) {
      coord = ((Fragment) parent).calcAbsoluteCoords(coord);
      return getLocalFromAbsoluteCartesianCoordinate(coord);
    }
    return coord;
  }

  /**
   * Refresh the occupancy and quantity of this atom. Based on the setting inside the parent
   * Phase (using quantityFromOccupancy() method) it computes the quantity from occupancy or
   * the occupancy from the quantity.
   */
  public void refreshOccupancyAndQuantity() {
	  Phase aphase = getPhaseParent();
//    System.out.println("Phase " + aphase.toXRDcatString() + " " + aphase.quantityFromOccupancy());
//    System.out.println("Site mult :" + getSiteMultiplicity() + ", " +
//        Double.valueOf(getQuantity()).doubleValue());
 	  if (aphase.quantityFromOccupancy())
		  computeQuantityFromOccupancy();
	  else
		  computeOccupancyFromQuantity();
  }

  /**
   * Compute the occupancy value from the quantity using the number of equivalent positions (getSiteMultiplicity()).
   */
  public void computeOccupancyFromQuantity() {
//	  getTotalOccupancy();
    int newSitePosNumber = getSiteMultiplicity();
    getOccupancy().setValue(Double.valueOf(getQuantity()).doubleValue() / newSitePosNumber);
  }

	public double getTotalOccupancy() {
		double occ = 0;
		for (int i = 0; i < getNumberOfScatterers(); i++)
			occ += getAtomScatterer(i).getOccupancy();
		double totalOcc = 1; // getOccupancy().getValueD();
		double normalization = 1.0;
		if (occ != 0) {
			normalization = totalOcc / occ;
//		System.out.println("Normalization: " + normalization + " " + occ);
			if (normalization != 1.0)
				for (int i = 0; i < getNumberOfScatterers(); i++) {
					AtomScatterer ato = getAtomScatterer(i);
					ato.setOccupancy(ato.getOccupancy() * normalization);
				}
		} else {
			double meanOcc = 1.0 / getNumberOfScatterers();
			for (int i = 0; i < getNumberOfScatterers(); i++) {
				getAtomScatterer(i).setOccupancy(meanOcc);
			}
		}
		return occ;
	}

  /**
   * Compute the quantity value from the occupancy using the number of equivalent positions (getSiteMultiplicity()).
   */
  public void computeQuantityFromOccupancy() {
//	  getTotalOccupancy();
    int newSitePosNumber = getSiteMultiplicity();
    setQuantity(Fmt.format(getTotalOccupancy() * newSitePosNumber));
  }

  /**
   * Set the local fractional coordinates of this atom (the coordinates of the generator position).
   *
   * @param coord the object containing the fractional coordinates to be set.
   */
  public void setLocalCoordinates(Coordinates coord) {
    getLocalCoordX().setValue(coord.x);
    getLocalCoordY().setValue(coord.y);
    getLocalCoordZ().setValue(coord.z);
  }

  /**
   * Set the local fractional coordinates of this atom (the coordinates of the generator position).
   *
   * @param coord the object containing the fractional coordinates to be set.
   */
  public void setAbsoluteCoordinates(Coordinates coord) {
    transformAbsoluteCoordToLocal(coord);
    getLocalCoordX().setValue(coord.x);
    getLocalCoordY().setValue(coord.y);
    getLocalCoordZ().setValue(coord.z);
  }

  public void setAbsoluteCoordinates(double[] coord) {
    coord = transformAbsoluteCoordToLocal(coord);
    getLocalCoordX().setValue(coord[0]);
    getLocalCoordY().setValue(coord[1]);
    getLocalCoordZ().setValue(coord[2]);
  }

  /**
   * Set the local coordinates of this atom (the coordinates of the generator position)
   * using absolute cartesian coordinates.
   *
   * @param abs_coord the object containing the absolute cartesian coordinates to be set.
   */
  public void setAbsoluteCartesianCoordinate(Coordinates abs_coord) {
    Phase aphase = getPhaseParent();
    double a = aphase.getCellValue(0);
    double b = aphase.getCellValue(1);
    double c = aphase.getCellValue(2);
    double cosalpha = Math.cos(aphase.getCellValue(3) * Constants.DEGTOPI);
    double cosbeta = Math.cos(aphase.getCellValue(4) * Constants.DEGTOPI);
    double cosgamma = Math.cos(aphase.getCellValue(5) * Constants.DEGTOPI);
    double singamma = Math.sin(aphase.getCellValue(5) * Constants.DEGTOPI);
    double V = aphase.getCellVolume();
    double z = ((a * b * singamma) / V) * abs_coord.z;
    double y = (abs_coord.y - c * ((cosalpha - cosbeta * cosgamma) / singamma) * z) / (b * singamma);
    double x = (abs_coord.x - (b * cosgamma * y + c * cosbeta * z)) / a;
    setLocalCoordinates(new Coordinates(abs_coord.x, abs_coord.y, abs_coord.z));
  }

  /**
   * Get the local coordinates of this atom (the coordinates of the generator position)
   * using absolute cartesian coordinates.
   *
   * @param abs_coord the object containing the absolute cartesian coordinates to be set.
   */
  public Coordinates getLocalFromAbsoluteCartesianCoordinate(Coordinates abs_coord) {
    Phase aphase = getPhaseParent();
    double a = aphase.getCellValue(0);
    double b = aphase.getCellValue(1);
    double c = aphase.getCellValue(2);
    double cosalpha = Math.cos(aphase.getCellValue(3) * Constants.DEGTOPI);
    double cosbeta = Math.cos(aphase.getCellValue(4) * Constants.DEGTOPI);
    double cosgamma = Math.cos(aphase.getCellValue(5) * Constants.DEGTOPI);
    double singamma = Math.sin(aphase.getCellValue(5) * Constants.DEGTOPI);
    double V = aphase.getCellVolume();
    double z = ((a * b * singamma) / V) * abs_coord.z;
    double y = (abs_coord.y - c * ((cosalpha - cosbeta * cosgamma) / singamma) * z) / (b * singamma);
    double x = (abs_coord.x - (b * cosgamma * y + c * cosbeta * z)) / a;
    return new Coordinates(x, y, z);
  }

  public double[] getLocalFromAbsoluteCartesianCoordinate(double[] abs_coord) {
    Phase aphase = getPhaseParent();
    double a = aphase.getCellValue(0);
    double b = aphase.getCellValue(1);
    double c = aphase.getCellValue(2);
    double cosalpha = Math.cos(aphase.getCellValue(3) * Constants.DEGTOPI);
    double cosbeta = Math.cos(aphase.getCellValue(4) * Constants.DEGTOPI);
    double cosgamma = Math.cos(aphase.getCellValue(5) * Constants.DEGTOPI);
    double singamma = Math.sin(aphase.getCellValue(5) * Constants.DEGTOPI);
    double V = aphase.getCellVolume();
    double z = ((a * b * singamma) / V) * abs_coord[2];
    double y = (abs_coord[1] - c * ((cosalpha - cosbeta * cosgamma) / singamma) * z) / (b * singamma);
    double x = (abs_coord[0] - (b * cosgamma * y + c * cosbeta * z)) / a;
    return new double[]{x, y, z};
  }

  /**
   * Get the absolute cartesian coordinates of this atom
   * using local coordinates (the coordinates of the generator position).
   *
   * @param local_coord the object containing the absolute cartesian coordinates to be set.
   */
  public Coordinates getAbsoluteCartesianFromLocalCoordinate(Coordinates local_coord) {
    Phase aphase = getPhaseParent();
    double a = aphase.getCellValue(0);
    double b = aphase.getCellValue(1);
    double c = aphase.getCellValue(2);
    double cosalpha = Math.cos(aphase.getCellValue(3) * Constants.DEGTOPI);
    double cosbeta = Math.cos(aphase.getCellValue(4) * Constants.DEGTOPI);
    double cosgamma = Math.cos(aphase.getCellValue(5) * Constants.DEGTOPI);
    double singamma = Math.sin(aphase.getCellValue(5) * Constants.DEGTOPI);
    double V = aphase.getCellVolume();
    double z = local_coord.z / ((a * b * singamma) / V);
    double y = local_coord.y * b * singamma + c * (cosalpha - cosbeta * cosgamma) / singamma * local_coord.z;
    double x = local_coord.x * a + b * cosgamma * local_coord.y + c * cosbeta * local_coord.z;
    return new Coordinates(x, y, z);
  }

  /**
   * Get the absolute cartesian coordinates of this atom
   * using local coordinates (the coordinates of the generator position).
   *
   * @param local_coord the object containing the absolute cartesian coordinates to be set.
   */
  public double[] getAbsoluteCartesianFromLocalCoordinate(double[] local_coord) {
    Phase aphase = getPhaseParent();
    double a = aphase.getCellValue(0);
    double b = aphase.getCellValue(1);
    double c = aphase.getCellValue(2);
    double cosalpha = Math.cos(aphase.getCellValue(3) * Constants.DEGTOPI);
    double cosbeta = Math.cos(aphase.getCellValue(4) * Constants.DEGTOPI);
    double cosgamma = Math.cos(aphase.getCellValue(5) * Constants.DEGTOPI);
    double singamma = Math.sin(aphase.getCellValue(5) * Constants.DEGTOPI);
    double V = aphase.getCellVolume();
    double z = local_coord[2] / ((a * b * singamma) / V);
    double y = local_coord[1] * b * singamma + c * (cosalpha - cosbeta * cosgamma) / singamma * local_coord[2];
    double x = local_coord[0] * a + b * cosgamma * local_coord[1] + c * cosbeta * local_coord[2];
    return new double[]{x, y, z};
  }


  /**
   * Check and refreshes equivalent atom positions and select as generator coordinates for this atom
   * the coordinates of the equivalent position with the smallest distance from the origin.
   */
  public void checkSmallerCoordinates() {
    refreshPositions(false);
    int sitePosNumber = getSiteMultiplicity();
    int index = 0;
    Phase aphase = getPhaseParent();
    for (int i = 0; i < sitePosNumber; i++) {
      Coordinates coordNext = atomcoordinates.elementAt(i);
      if (coordNext.x < aphase.reducedCellFactor[0] && coordNext.y < aphase.reducedCellFactor[1] &&
          coordNext.z < aphase.reducedCellFactor[2])
        index = i;
    }
    if (index > 0) {
      Coordinates coordNext = atomcoordinates.elementAt(index);
      transformAbsoluteCoordToLocal(coordNext);
      setLocalCoordinates(coordNext);
      refreshPositions(false);
    }
  }

  /**
   * Check if the distance between two coordinates positions is smaller than one atomic radius of this atom.
   *
   * @param coordFirst object containing the fractional coordinates of the first atom
   * @param coordNext  object containing the fractional coordinates of the second atom
   * @return true if the distance is smaller
   */
  public boolean tooShortDistance(Coordinates coordFirst, Coordinates coordNext) {
//		double halfRadius = radius / 2.0;
    return tooShortDistance(coordFirst, coordNext, getMeanRadius());
  }

  /**
   * Check if the distance between two coordinates positions is smaller than a certain distance passed to the method.
   *
   * @param coordFirst      object containing the fractional coordinates of the first atom
   * @param coordNext       object containing the fractional coordinates of the second atom
   * @param distanceToCheck the distance for the checking.
   * @return true if the distance between the two positions is shorter than distanceToCheck.
   */
  public boolean tooShortDistance(Coordinates coordFirst, Coordinates coordNext, double distanceToCheck) {
    Phase aphase = getPhaseParent();
    double halfRadius = distanceToCheck;
    boolean res1 = (Math.abs(coordFirst.x - coordNext.x) * aphase.getCellValue(0) < halfRadius ||
        Math.abs(coordFirst.x - coordNext.x - 1.0) * aphase.getCellValue(0) < halfRadius ||
        Math.abs(coordFirst.x - coordNext.x + 1.0) * aphase.getCellValue(0) < halfRadius);
    boolean res2 = (Math.abs(coordFirst.y - coordNext.y) * aphase.getCellValue(1) < halfRadius ||
        Math.abs(coordFirst.y - coordNext.y - 1.0) * aphase.getCellValue(1) < halfRadius ||
        Math.abs(coordFirst.y - coordNext.y + 1.0) * aphase.getCellValue(1) < halfRadius);
    boolean res3 = (Math.abs(coordFirst.z - coordNext.z) * aphase.getCellValue(2) < halfRadius ||
        Math.abs(coordFirst.z - coordNext.z - 1.0) * aphase.getCellValue(2) < halfRadius ||
        Math.abs(coordFirst.z - coordNext.z + 1.0) * aphase.getCellValue(2) < halfRadius);
    if (res1 && res2 && res3)
      return true;
    else
      return false;
  }

  /**
   * Collapse the position of two atoms into one and compute the new mean position returned in the
   * Coordinates object of the first position. The new mean position is computed weighting
   * the two position respect to their weight.
   *
   * @param coordFirst coordinates of the first position, the collapsed final position will be returned in this object
   * @param wt1        weight of the first position
   * @param coordNext  coordinates of the second position
   * @param wt2        weight of the second position
   */
  public void collapse(Coordinates coordFirst, int wt1, Coordinates coordNext, int wt2) {
    if (Math.abs(coordFirst.x - coordNext.x) > 0.5) {
      if (coordFirst.x > coordNext.x)
        coordFirst.x -= 1.0;
      else
        coordNext.x -= 1.0;
    }
    if (Math.abs(coordFirst.y - coordNext.y) > 0.5) {
      if (coordFirst.y > coordNext.y)
        coordFirst.y -= 1.0;
      else
        coordNext.y -= 1.0;
    }
    if (Math.abs(coordFirst.z - coordNext.z) > 0.5) {
      if (coordFirst.z > coordNext.z)
        coordFirst.z -= 1.0;
      else
        coordNext.z -= 1.0;
    }
    coordFirst.x = (coordFirst.x * wt1 + coordNext.x * wt2) / (wt1 + wt2);
    coordFirst.y = (coordFirst.y * wt1 + coordNext.y * wt2) / (wt1 + wt2);
    coordFirst.z = (coordFirst.z * wt1 + coordNext.z * wt2) / (wt1 + wt2);
  }

  /**
   * Gets the multeplicity of the atom site. It return simply the dimension of the atomcoordinates array.
   *
   * @return the site multeplicity.
   */
  public int getSiteMultiplicity() {
    return atomcoordinates.size();
  }

  /**
   * Gets the weight of this site equal to quantity * weight_of_the_atom.
   *
   * @return the site total weight.
   */
  public double getSiteWeight() {
    refreshPositions(false);
	  double totalWeight = 0;
	  for (int i = 0; i < getNumberOfScatterers(); i++) {
		  totalWeight += getAtomScatterer(i).getSiteWeight();
	  }
    return getSiteMultiplicity() * totalWeight * getOccupancyValue();
  }

	public Vector<AtomQuantity> getChemicalComposition() {
		Vector<AtomQuantity> chemicalComposition = new Vector<>();
		refreshPositions(false);
		for (int i = 0; i < getNumberOfScatterers(); i++) {
			AtomQuantity atomQuantity = getAtomScatterer(i).getAtomQuantity();
			atomQuantity.quantity *= getOccupancyValue();
			atomQuantity.quantity_weight *= getOccupancyValue();
			int index = atomQuantity.getPositionIn(chemicalComposition);
			if (index >= 0) {
				AtomQuantity anAtomQuantity = chemicalComposition.elementAt(index);
				anAtomQuantity.quantity += atomQuantity.quantity;
			} else
				chemicalComposition.add(atomQuantity);
		}
		return chemicalComposition;
	}

  /**
   * Gets the total absorption of this atom site. Proportional to the quantity.
   *
   * @param rad the Radiation kind for which the absorption must be computed.
   * @return the total absorption of this atom site.
   */
  public double getSiteAbsorption(RadiationType rad) {
	  double absorption = 0;
/*	  if (rad.isNeutron())
		  absorption = getSiteWeight() * rad.getRadiation(0).neutronAbs[getAtomicListNumber()];
	  else if (rad.isElectron())
		  absorption = getSiteWeight() * rad.getRadiation(0).electronAbs[getAtomicListNumber()];
	  else
		  absorption = getSiteAbsorption(rad.getRadiationEnergy());*/
	  for (int i = 0; i < getNumberOfScatterers(); i++)
		  absorption += getAtomScatterer(i).getSiteAbsorption(rad);
	  return getSiteMultiplicity() * absorption * getOccupancyValue();
  }

	public double getSiteAbsorption(double energyInKeV) {
		double absorption = 0;
		for (int i = 0; i < getNumberOfScatterers(); i++)
			absorption += getAtomScatterer(i).getSiteAbsorption(energyInKeV);
		return getSiteMultiplicity() * absorption * getOccupancyValue();
	}

	/**
   * Convenient vector created once to be used by method getCoordinates to store and return the coordinates
   * avoiding to create the vector multiple times.
   */
  double[] xc = new double[3];

  /**
   * Gets the coordinates of the equivalent position specify by index.
   *
   * @param index the index of the equivalent position to be returned.
   * @return the coordinates of the positon corresponding to index as a double[3].
   */
  public double[] getCoordinates(int index) {
    if (index >= 0 && index < atomcoordinates.size()) {
      Coordinates coord = atomcoordinates.elementAt(index);
      xc[0] = coord.x;
      xc[1] = coord.y;
      xc[2] = coord.z;
      return xc;
    }
    return null;
  }

  /**
   * Gets the coordinates of the first equivalent position.
   *
   * @return a Coordinates object containing the coordinates of the first equivalent position.
   */
  public Coordinates getCoordinates() {
    return atomcoordinates.elementAt(0);
  }

  /**
   * Gets all equivalent positions in cartesian coordinates.
   *
   * @return the array containing all equivalent positions in cartesian coordinates.
   */
  public ArrayList getCartesianCoords() {
    if (cartesianCoords == null) {
	    refreshPositions = true;
	    refreshPositions(true);
    }
    return cartesianCoords;
  }

  /**
   * Gets the cartesian coordinates for the equivalent position given by the index argument
   *
   * @param index the number of the equivalent position for which the coordinates are required
   * @return the cartesian coordinates for the equivalent position given by the index argument
   */
  public Coordinates getCartesianCoords(int index) {
    if (cartesianCoords == null) {
	    refreshPositions = true;
	    refreshPositions(true);
    }
    if (index >= 0 && index < cartesianCoords.size())
      return (Coordinates) cartesianCoords.get(index);
    return null;
  }

  /**
   * Gets the fractional coordinate x for the equivalent position given by the index argument
   *
   * @param index the number of the equivalent position for which the coordinate is required
   * @return the fractional coordinate x for the equivalent position given by the index argument
   */
  public double getx(int index) {
    if (index >= 0 && index < atomcoordinates.size())
      return atomcoordinates.elementAt(index).x;
    else
      return 999.0;
  }

  /**
   * Gets the fractional coordinate y for the equivalent position given by the index argument
   *
   * @param index the number of the equivalent position for which the coordinate is required
   * @return the fractional coordinate y for the equivalent position given by the index argument
   */
  public double gety(int index) {
    if (index >= 0 && index < atomcoordinates.size())
      return atomcoordinates.elementAt(index).y;
    else
      return 999.0;
  }

  /**
   * Gets the fractional coordinate z for the equivalent position given by the index argument
   *
   * @param index the number of the equivalent position for which the coordinate is required
   * @return the fractional coordinate z for the equivalent position given by the index argument
   */
  public double getz(int index) {
    if (index >= 0 && index < atomcoordinates.size())
      return atomcoordinates.elementAt(index).z;
    else
      return 999.0;
  }

  /**
   * Gets the index of this atom symbol in the AtomInfo tables.
   *
   * @return the corresponding number of this atom in the atominfo tables
  public int getAtomicListNumber() {
//    System.out.println("AtomSite " + getAtomSymbol() + ", atomic number: " + atomListNumber);
    return atomListNumber;
  }
   */

  /**
   * Gets the index of this atom symbol in the AtomInfo tables.
   *
   * @return the corresponding number of this atom in the atominfo tables
  public int getOxidationNumber() {
//    System.out.println("AtomSite " + getAtomSymbol() + ", atomic number: " + atomListNumber);
    return oxidationNumber;
  }
   */

  /**
   * Gets the index of this isotope in the AtomInfo tables.
   *
   * @return the corresponding number of this isotope in the atominfo tables
  public int getIsotopicListNumber() {
//    System.out.println("AtomSite " + getAtomSymbol() + ", isotopic number: " + isotopeListNumber);
    return isotopeListNumber;
  }
   */

  /**
   * Gets the atomic number of the atom
   *
   * @return the atomic number
  public int getAtomicNumber() {
//    System.out.println("AtomSite " + getAtomSymbol() + ", atom number: " + atomNumber);
    return atomNumber;
  }
   */

  /**
   * Gets the scattering factors (real and imaginary part) for a certain d-spacing a certain
   * Radiation kind.
   *
   * @param dspacing the d-space of the reflex for which the scattering factors are required
   * @param rad  the Radiation type for which the scattering factors are required
   * @return a vector containing the real and imaginary part of the scattering factor.
   */

  public double[] scatfactor(double dspacing, Radiation rad) {

	  double[] fu = new double[2]; // = localScatfactorNoDispersion(dspacing, rad);
//	  double totalOccupancy = getOccupancyValue();
	  for (int i = 0; i < getNumberOfScatterers(); i++) {
		  AtomScatterer ato = getAtomScatterer(i);
		  double[] addFu = ato.scatfactor(dspacing, rad);
		  for (int j = 0; j < fu.length; j++)
			  fu[j] += addFu[j] * ato.getOccupancy();
	  }
    return fu;
  }
  
  public double[] scatfactor(double dspacing, double energyInKeV) {
    
    double[] fu = new double[2]; // = localScatfactorNoDispersion(dspacing, rad);
//	  double totalOccupancy = getOccupancyValue();
    for (int i = 0; i < getNumberOfScatterers(); i++) {
      AtomScatterer ato = getAtomScatterer(i);
      double[] addFu = ato.scatfactor(dspacing, energyInKeV);
      for (int j = 0; j < fu.length; j++)
        fu[j] += addFu[j] * ato.getOccupancy();
    }
    return fu;
  }

/*	public double[] localScatfactorNoDispersion(double dspacing, Radiation rad) {

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
		} else {
			// x-ray radiation
			fu[0] = Radiation.xraySF[atomicListNumber][8];
			if (dspacing == 0)
				for (int j = 0; j < 4; j++)
					fu[0] += Radiation.xraySF[atomicListNumber][j];
			else
				for (int j = 0; j < 4; j++)
					fu[0] += Radiation.xraySF[atomicListNumber][j] * Math.exp(-Radiation.xraySF[atomicListNumber][j + 4] /
							(4 * dspacing * dspacing));
		}
		return fu;
	}*/

	/**
	 * Gets the X-ray scattering factor at zero deg or the number of electrons of this atom
	 * equal to: atomNumber - oxidation number
	 *
	 * @return the number of electrons of this atom
	 */
	public double xrayscatfactor() {

		double xrayScatFactor = 0; // localXrayscatfactor();
		for (int i = 0; i < getNumberOfScatterers(); i++) {
			AtomScatterer ato = getAtomScatterer(i);
			xrayScatFactor += ato.xrayscatfactor() * ato.getOccupancy();
		}
		return xrayScatFactor;// * getOccupancyValue();
	}

	/**
   * Gets the X-ray scattering factor at zero deg or the number of electrons of this atom
   * equal to: atomNumber - oxidation number
   *
   * @return the number of electrons of this atom

  public double localXrayscatfactor() {
    return atomNumber - getOxidationNumber();
  }
	 */

  /**
   * Gets the magnetic scattering factor for this atom.
   *
   * @param dspacing the d-spacing of the reflex for which the magnetic scattering factor is returned
   * @param radType  the Radiation object type
   * @return the magnetic scattering factor
   */
  public double magneticScatfactor(double dspacing, int radType) {
		double mag = 0; // localMagneticScatfactor(dspacing, radType);
	  for (int i = 0; i < getNumberOfScatterers(); i++) {
		  AtomScatterer ato = getAtomScatterer(i);
		  mag += ato.magneticScatfactor(dspacing, radType) * ato.getOccupancy();
	  }
	  return mag;
  }

/*	public double localMagneticScatfactor(double dspacing, int radType) {

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
  }*/

  /**
   * Compute Debye-Waller factor for this atom site.
   *
   * @param h        the h index of the reflex
   * @param k        the k index of the reflex
   * @param l        the l index of the reflex
   * @param i_4dspacing2 the 0.25/d-spacing^2 of the reflex
   * @return the Debye-Waller factor value.
   */
  public double DebyeWaller(int h, int k, int l, double i_4dspacing2) {

    double w11, w22, w33, w12, w13, w23, CorrectionPI, isoCorrection;

    if (getPhaseParent().isDebyeWallerModelDimensionLess()) {
      // recommended model
	      Phase aphase = getPhaseParent();
//        cellVolume2 = aphase.getFullCellVolume() / Constants.PI;
//        cellVolume2 *= (cellVolume2 / 2.);
	    double[] so = aphase.getSoVector();
        w11 = so[0];// / cellVolume2;
        w22 = so[1];// / cellVolume2;
        w33 = so[2];// / cellVolume2;
        w12 = so[6];// / cellVolume2;
        w13 = so[7];// / cellVolume2;
        w23 = so[8];// / cellVolume2;
	      CorrectionPI = 2.0 * Constants.PI * Constants.PI;
	    isoCorrection = CorrectionPI * 4.0;
    } else { // dimensionless
		    w11 = 1;
		    w22 = 1;
		    w33 = 1;
		    w12 = 1;
		    w13 = 1;
		    w23 = 1;
		    CorrectionPI = 1;
	    isoCorrection = 1;
    }

    double h2 = h * h;
    double k2 = k * k;
    double l2 = l * l;
    double muno = (getAnisoBfactorValue(0) * h2 * w11 +
        getAnisoBfactorValue(1) * k2 * w22 +
        getAnisoBfactorValue(2) * l2 * w33 +
        2. * getAnisoBfactorValue(3) * h * k * w12 +
        2. * getAnisoBfactorValue(4) * h * l * w13 +
        2. * getAnisoBfactorValue(5) * k * l * w23) * CorrectionPI;
    //		Radiation rad = radtype.getRadiation(0);

    //System.out.println(getBfactorValue() + " " + dspacing + " " + cellVolume2);
    return Math.exp(-getBfactorValue() * isoCorrection * i_4dspacing2 - muno);
  }

/*	private String loadedSymbol = null;

  *
   * Retrieve all atom properties from the AtomInfo object based on the symbol
   * of this atom.


  public void loadAtomProperties() {
    String symbol = getAtomSymbol();
    if (loadedSymbol == null || !symbol.equalsIgnoreCase(loadedSymbol)) {
      atomNumber = AtomInfo.retrieveAtomNumber(stripIsotopeNumber(symbol));
      atomListNumber = AtomInfo.getAtomNumber(stripIsotopeNumber(symbol));
      oxidationNumber = getOxidationNumber(loadedSymbol);
      isotopeListNumber = AtomInfo.getIsotopeNumber(stripOxidation(symbol));
      weight = AtomInfo.retrieveAtomWeight(stripIsotopeNumber(symbol));
      radius = Math.abs(AtomInfo.retrieveAtomRadius(stripIsotopeNumber(symbol)));
      //		System.out.println(magneticSF[0]);
      loadedSymbol = symbol;
    }
  }*/

  public static String stripOxidation(String symbol) {
    int i = symbol.length() - 1;
    while (symbol.charAt(i) == '+' || symbol.charAt(i) == '-')
      i--;
    while (Character.isDigit(symbol.charAt(i)))
      i--;
    return symbol.substring(0, i + 1);
  }

  public static String stripIsotopeNumber(String symbol) {
    int i = 0;
    while (Character.isDigit(symbol.charAt(i))) {
      i++;
    }
    return symbol.substring(i, symbol.length());
  }

  /**
   * Set refinable all the Parameters to be refined for a crystal structure refinement.
   */
  public void freeAllCrystalParameters() {
    /*		if (!isSpecialCoord(getLocalCoordX().getValueD()))
    getLocalCoordX().setRefinableCheckBound();
    if (!isSpecialCoord(getLocalCoordX().getValueD()))
    getLocalCoordY().setRefinableCheckBound();
    if (!isSpecialCoord(getLocalCoordX().getValueD()))
    getLocalCoordZ().setRefinableCheckBound();*/

    if (isDummyAtom())
      return;
    int controlCoord[] = new int[3];
    for (int i = 0; i < 3; i++)
      controlCoord[i] = -1;
    double ratio[] = new double[3];
    double constant[] = new double[3];
    isSpecialCoord(controlCoord, ratio, constant);

    if (controlCoord[0] == 0)
      getLocalCoordX().setRefinableCheckBound();
    else if (controlCoord[0] == 2)
      getLocalCoordX().setEqualToAutomatic(getLocalCoordY(),
          Fmt.format(ratio[0]), Fmt.format(constant[0]));
    else if (controlCoord[0] == 3) {
      getLocalCoordX().setEqualToAutomatic(getLocalCoordZ(),
          Fmt.format(ratio[0]), Fmt.format(constant[0]));
    }
    if (controlCoord[1] == 0)
      getLocalCoordY().setRefinableCheckBound();
    else if (controlCoord[1] == 1)
      getLocalCoordY().setEqualToAutomatic(getLocalCoordX(),
          Fmt.format(ratio[1]), Fmt.format(constant[1]));
    else if (controlCoord[1] == 3) {
      getLocalCoordY().setEqualToAutomatic(getLocalCoordZ(),
          Fmt.format(ratio[1]), Fmt.format(constant[1]));
    }
    if (controlCoord[2] == 0)
      getLocalCoordZ().setRefinableCheckBound();
    else if (controlCoord[2] == 1)
      getLocalCoordZ().setEqualToAutomatic(getLocalCoordX(),
          Fmt.format(ratio[2]), Fmt.format(constant[2]));
    else if (controlCoord[2] == 2) {
      getLocalCoordZ().setEqualToAutomatic(getLocalCoordY(),
          Fmt.format(ratio[2]), Fmt.format(constant[2]));
    }

    refreshPositions(false);

    getBfactor().setRefinableCheckBound();
  }

  /**
   * New method to check for special coordinates. After a call to this, at end, refreshPositions must
   * be called to regenerate the correct atom position table.
   *
   * @param controlCoord
   * @param ratio
   * @param constant
   */
  public void isSpecialCoord(int[] controlCoord, double[] ratio, double[] constant) {
    // new method better but slower

    refreshPositions(false);
    int numberPos = atomcoordinates.size();

    Phase aphase = getPhaseParent();
    int numberGeneralPos = aphase.getPhaseInfo().getSitePositionNumber();

    if (numberGeneralPos == numberPos) {
      for (int i = 0; i < 3; i++)
        controlCoord[i] = 0;
      return;
    }

    refreshPositions(.03, 0.0, 0.0);
    if (numberPos == atomcoordinates.size())
      controlCoord[0] = 0;
    refreshPositions(0.0, .03, 0.0);
    if (numberPos == atomcoordinates.size())
      controlCoord[1] = 0;
    refreshPositions(0.0, 0.0, .03);
    if (numberPos == atomcoordinates.size())
      controlCoord[2] = 0;

    if (controlCoord[0] != 0 && controlCoord[1] != 0) {
      refreshPositions(.03, .03, 0.0);
      if (numberPos == atomcoordinates.size()) {
        controlCoord[0] = 0;
        controlCoord[1] = 1;
        ratio[1] = 1.0;
        constant[1] = 0.0;
      } else {
        refreshPositions(-.03, .03, 0.0);
        if (numberPos == atomcoordinates.size()) {
          controlCoord[0] = 0;
          controlCoord[1] = 1;
          ratio[1] = -1.0;
          constant[1] = 0.0;
        }
      }
    }
    if (controlCoord[0] != 0 && controlCoord[2] != 0) {
      refreshPositions(.03, 0.0, .03);
      if (numberPos == atomcoordinates.size()) {
        controlCoord[0] = 0;
        controlCoord[2] = 1;
        ratio[2] = 1.0;
        constant[2] = 0.0;
      } else {
        refreshPositions(-.03, 0.0, .03);
        if (numberPos == atomcoordinates.size()) {
          controlCoord[0] = 0;
          controlCoord[2] = 1;
          ratio[2] = -1.0;
          constant[2] = 0.0;
        }
      }
    }
    if (controlCoord[2] != 0 && controlCoord[1] != 0) {
      refreshPositions(0.0, .03, .03);
      if (numberPos == atomcoordinates.size()) {
        controlCoord[1] = 0;
        controlCoord[2] = 1;
        ratio[2] = 1.0;
        constant[2] = 0.0;
      } else {
        refreshPositions(0.0, -.03, .03);
        if (numberPos == atomcoordinates.size()) {
          controlCoord[1] = 0;
          controlCoord[2] = 1;
          ratio[2] = -1.0;
          constant[2] = 0.0;
        }
      }
    }

    if (controlCoord[0] != 0 && controlCoord[1] != 0 && controlCoord[2] != 0) {
      refreshPositions(.03, .03, .03);
      if (numberPos == atomcoordinates.size()) {
        controlCoord[0] = 0;
        controlCoord[1] = 1;
        ratio[1] = 1.0;
        constant[1] = 0.0;
        controlCoord[2] = 1;
        ratio[2] = 1.0;
        constant[2] = 0.0;
      } else {
        refreshPositions(-.03, .03, .03);
        if (numberPos == atomcoordinates.size()) {
          controlCoord[0] = 0;
          controlCoord[1] = 1;
          ratio[1] = -1.0;
          constant[1] = 0.0;
          controlCoord[2] = 1;
          ratio[2] = -1.0;
          constant[2] = 0.0;
        } else {
          refreshPositions(-.03, -.03, .03);
          if (numberPos == atomcoordinates.size()) {
            controlCoord[0] = 0;
            controlCoord[1] = 1;
            ratio[1] = 1.0;
            constant[1] = 0.0;
            controlCoord[2] = 1;
            ratio[2] = -1.0;
            constant[2] = 0.0;
          } else {
            refreshPositions(-.03, .03, -.03);
            if (numberPos == atomcoordinates.size()) {
              controlCoord[0] = 0;
              controlCoord[1] = 1;
              ratio[1] = -1.0;
              constant[1] = 0.0;
              controlCoord[2] = 1;
              ratio[2] = 1.0;
              constant[2] = 0.0;
            } else {
            }
          }
        }
      }
    }
  }

  /**
   * Check if this atom site is shared with the atom site in argument.
   *
   * @param anatom the atom site to check
   * @return true if this atom and anatom occupy the same position in the cell, false otherwise
   */
  public boolean shareSiteWith(AtomSite anatom) {
    boolean share = false;
    boolean flagx1;
    double x[] = new double[3];

    refreshPositions(false);

    x[0] = anatom.getLocalCoordX().getValueD();
    x[1] = anatom.getLocalCoordY().getValueD();
    x[2] = anatom.getLocalCoordZ().getValueD();

    for (int kx = 0; kx < atomcoordinates.size(); kx++) {
      Coordinates coord = atomcoordinates.elementAt(kx);
      double diff1 = Math.abs(x[0] - coord.x);
      double diff2 = Math.abs(x[1] - coord.y);
      double diff3 = Math.abs(x[2] - coord.z);
      flagx1 = !(diff1 > Constants.TOOLERANCE_COORD && diff1 < 1.0 - Constants.TOOLERANCE_COORD);
      if (diff2 > Constants.TOOLERANCE_COORD && diff2 < 1.0 - Constants.TOOLERANCE_COORD)
        flagx1 = false;
      if (diff3 > Constants.TOOLERANCE_COORD && diff3 < 1.0 - Constants.TOOLERANCE_COORD)
        flagx1 = false;
      if (flagx1)
        share = true;
    }
    return share;
  }

  /**
   * Check if this site position is a face centered cell position.
   *
   * @return true if it is a fcc position, false otherwise.
   */
  public boolean fccPosition() {
    boolean share = false;
    boolean flagx1;
    double x[] = new double[3];

    refreshPositions(false);

    x[0] = 0.5;
    x[1] = 0.5;
    x[2] = 0.0;

    for (int kx = 0; kx < atomcoordinates.size(); kx++) {
      Coordinates coord = atomcoordinates.elementAt(kx);
      double diff1 = Math.abs(x[0] - coord.x);
      double diff2 = Math.abs(x[1] - coord.y);
      double diff3 = Math.abs(x[2] - coord.z);
      flagx1 = !(diff1 > Constants.TOOLERANCE_COORD && diff1 < 1.0 - Constants.TOOLERANCE_COORD);
      if (diff2 > Constants.TOOLERANCE_COORD && diff2 < 1.0 - Constants.TOOLERANCE_COORD)
        flagx1 = false;
      if (diff3 > Constants.TOOLERANCE_COORD && diff3 < 1.0 - Constants.TOOLERANCE_COORD)
        flagx1 = false;
      if (flagx1)
        share = true;
    }
    return share;
  }

  /**
   * Bounds all parameters of this atom to the one passed in argument
   *
   * @param anatom the atom which all parameters must be bounded to.
   */
  public void boundAllParametersTo(AtomSite anatom) {
    getLocalCoordX().setRefparameterAutomatic(anatom.getLocalCoordX());
    getLocalCoordY().setRefparameterAutomatic(anatom.getLocalCoordY());
    getLocalCoordZ().setRefparameterAutomatic(anatom.getLocalCoordZ());
    getBfactor().setRefparameterAutomatic(anatom.getBfactor());
    for (int i = 0; i < 6; i++)
      getAnisoBfactor(i).setRefparameterAutomatic(anatom.getAnisoBfactor(i));
  }

  /**
   * Gets the atomic weight of this atom
   *
   * @return the atomic weight
   */
  public double getMeanWeight() {
	  meanWeight = 0;
	  for (int i = 0; i < getNumberOfScatterers(); i++)
		  meanWeight += getAtomScatterer(i).getSiteWeight();
//	  meanWeight /= getOccupancyValue();
    return meanWeight;
  }

  /**
   * Gets the atomic radius of this atom in Angstrom.
   *
   * @return the atomic radius
   */
  public double getMeanRadius() {
	  meanRadius = 0;
	  for (int i = 0; i < getNumberOfScatterers(); i++) {
		  AtomScatterer ato = getAtomScatterer(i);
		  meanRadius += ato.getAtomRadius() * ato.getOccupancy();
	  }
//	  meanRadius /= getOccupancyValue();
    return meanRadius;
  }

  /**
   * Gets a String with all informations for this atom:
   * label + symbol + quantity + occupancy +x + y + z + site_multiplicity + B_factor + radius + weight
   *
   * @return the information String
   */
  public String getInformationString() {
    StringBuffer astring = new StringBuffer("");
    astring.append(" ");
    astring.append(getLabel());
//    astring.append(" ");
//    astring.append(getAllAtomSymbol());
    astring.append(" ");
    astring.append(getQuantity());
    astring.append(" ");
    astring.append(getOccupancyValue());
    astring.append(" ");
    astring.append(getLocalCoordX().getValue());
    astring.append(" ");
    astring.append(getLocalCoordY().getValue());
    astring.append(" ");
    astring.append(getLocalCoordZ().getValue());
    astring.append(" ");
    astring.append(getSiteMultiplicity());
    astring.append(" ");
    astring.append(getBfactorValue());
    astring.append(" ");
    astring.append(getMeanRadius());
    astring.append(" ");
    astring.append(getMeanWeight());
    astring.append(Constants.lineSeparator);
//    if (loadedSymbol == null)
//      loadAtomProperties();
/*    astring.append("Atomic #, atom #, isotope #, weight, radius, symbol for tables: ");
    astring.append(atomNumber + ", ");
    astring.append(atomListNumber + ", ");
    astring.append(isotopeListNumber + ", ");
    astring.append(weight + ", ");
    astring.append(radius + ", ");
    astring.append(loadedSymbol);
    astring.append(Constants.lineSeparator);
    Radiation.loadAtomTables();
    astring.append("Neutron sf: ");
    astring.append(Radiation.neutronSF[getIsotopicListNumber()] + " ");
    astring.append(Constants.lineSeparator);
    astring.append("Magnetic sf: ");
    for (int j = 0; j < 7; j++)
      astring.append(Radiation.magneticSF[getAtomicListNumber()][j] + " ");
    astring.append(Constants.lineSeparator);
    astring.append("Electron sf: ");
    for (int j = 0; j < 9; j++)
      astring.append(Radiation.electronSF[getAtomicListNumber()][j] + " ");
    astring.append(Constants.lineSeparator);
    astring.append("Neutron abs: ");
    astring.append(Radiation.neutronAbs[getAtomicListNumber()]);
    astring.append(Constants.lineSeparator);
    astring.append("Electron abs: ");
    astring.append(Radiation.electronAbs[getAtomicListNumber()]);
    astring.append(Constants.lineSeparator);*/
	  // todo, fix this
/*    astring.append("X-ray sf: ");
    for (int i = 0; i < Radiation.xraySF.length; i++)
      for (int j = 0; j < Radiation.xraySF[i].length; j++) {
        astring.append(Radiation.xraySF[i][j] + " ");
      }
    astring.append(Constants.lineSeparator);

    astring.append("X-ray disp and abs: ");
    for (int i = 0; i < Radiation.xraySFA.length; i++)
      for (int j = 0; j < Radiation.xraySFA[i].length; j++)
        for (int k = 0; k < Radiation.xraySFA[i][j].length; k++)
          astring.append(Radiation.xraySFA[i][j][k] + " ");*/

    return astring.toString();
  }

/*	private String getAllAtomSymbol() {
		return "";
	}*/

	public static double conversionUToB = 2.0 * Math.PI * Math.PI;

	public void convertAtomDisplacementsToDimensionless() {
		getBfactor().setValue(getBfactorValue() / (4.0 * conversionUToB));
		Phase aphase = getPhaseParent();
		aphase.getCellVolume();
		double[] so = aphase.getSoVector();
		for (int i = 0; i < 6; i++)
			getAnisoBfactor(i).setValue(getAnisoBfactorValue(i) / (so[i] * conversionUToB));
	}

	public void convertAtomDisplacementsFromDimensionless() {
		getBfactor().setValue(getBfactorValue() * (4.0 * conversionUToB));
		Phase aphase = getPhaseParent();
		aphase.getCellVolume();
		double[] so = aphase.getSoVector();
		for (int i = 0; i < 6; i++)
			getAnisoBfactor(i).setValue(getAnisoBfactorValue(i) * so[i] * conversionUToB);
	}

	// section for anisotropic B factors
	// References: W. F. Kuhs, Atomic diplacement parameters, Vol. D, Chapter 1.9, pp. 228-242,
	// International Tables for Crystallography, 2006.
	// H. Levy, Acta Cryst. 9, 679, 1956.
	// W. J. A. M.Peterse and J. H. Palm, Acta Cryst. 20, 147, 1966.

	public double[] getInvariantMatrixOfBFactors(double[][] b) {
		// using the Wigner theorem as described by Peterse and Palm, J. Appl. Cryst. 20, 147, 1965

		double[][] bInv = new double[3][3];

		if (refreshPositions)  // in case needed, refresh the equivalentSitePositions Vector
			refreshPositions(true);

		for (int s = 0; s < equivalentSitePositions.size(); s++) {
			SitePosition sitepos = equivalentSitePositions.elementAt(s);
			double[][] Rx = sitepos.getRotationMatrix();
			for (int i = 0; i < 3; i++)
				for (int j = i; j < 3; j++)
					for (int k = 0; k < 3; k++)
						for (int l = 0; l < 3; l++)
							bInv[i][j] += Rx[i][k] * Rx[j][l] * b[k][l];
		}

		double[] bRet = new double[6];
		int index = 0;
		bRet[index++] = bInv[0][0];
		bRet[index++] = bInv[1][1];
		bRet[index++] = bInv[2][2];
		bRet[index++] = bInv[0][1];
		bRet[index++] = bInv[0][2];
		bRet[index++] = bInv[1][2];
		return bRet;
	}

	public static int[][] symmetryRestrictionB = {
			{1, 1, 1, 0, 0, 0}, // B1, ITC, vol. D, chapter 1.9, 2006
			{1, 1, 3, 0, 0, 0}, // B2
			{1, 2, 1, 0, 0, 0}, // B3
			{1, 2, 2, 0, 0, 0}, // B4
			{1, 1, 1, 4, 4, 4}, // B5
			{1, 1, 1, 4, 4, 4}, // B6
			{1, 1, 1, 4, 4, 4}, // B7
			{1, 1, 1, 4, 4, 4}, // B8
			{1, 1, 3, 1, 0, 0}, // B9
			{1, 2, 3, 0, 0, 0}, // B10
			{1, 1, 3, 4, 0, 0}, // B11
			{1, 2, 1, 0, 5, 0}, // B12
			{1, 2, 2, 0, 0, 6}, // B13
			{1, 2, 3, 2, 0, 0}, // B14
			{1, 2, 3, 1, 0, 0}, // B15
			{1, 2, 3, 4, 0, 0}, // B16
			{1, 2, 3, 0, 5, 0}, // B17
			{1, 2, 3, 0, 0, 6}, // B18
			{1, 1, 3, 4, 5, 5}, // B19
			{1, 1, 3, 4, 5, 5}, // B20
			{1, 2, 1, 4, 5, 4}, // B21
			{1, 2, 1, 4, 5, 4}, // B22
			{1, 2, 2, 4, 4, 6}, // B23
			{1, 2, 2, 4, 4, 6}, // B24
			{1, 2, 3, 2, 5, 5}, // B25
			{1, 2, 3, 1, 0, 6}, // B26
			{1, 2, 3, 2, 5, 0}, // B27
			{1, 2, 3, 1, 5, 5}, // B28
			{1, 2, 3, 4, 5, 6}  // B29
	};

	public static double[][] symmetryRestrictionRules = {
			{0, 1, 1, 0, 0, 0}, // B1, ITC, vol. D, chapter 1.9, 2006
			{0, 1, 0, 0, 0, 0}, // B2
			{0, 0, 1, 0, 0, 0}, // B3
			{0, 0, 2, 0, 0, 0}, // B4
			{0, 1, 1, 0, 1, 1}, // B5
			{0, 1, 1, 0, -1, -1}, // B6
			{0, 1, 1, 0, -1, 1}, // B7
			{0, 1, 1, 0, 1, -1}, // B8
			{0, 1, 0, 0.5, 0, 0}, // B9
			{0, 0, 0, 0, 0, 0}, // B10
			{0, 1, 0, 0, 0, 0}, // B11
			{0, 0, 1, 0, 0, 0}, // B12
			{0, 0, 2, 0, 0, 0}, // B13
			{0, 0, 0, 0.5, 0, 0}, // B14
			{0, 0, 0, 0.5, 0, 0}, // B15
			{0, 0, 0, 0, 0, 0}, // B16
			{0, 0, 0, 0, 0, 0}, // B17
			{0, 0, 0, 0, 0, 0}, // B18
			{0, 1, 0, 0, 0, -1}, // B19
			{0, 1, 0, 0, 0, 1}, // B20
			{0, 0, 1, 0, 0, -1}, // B21
			{0, 0, 1, 0, 0, 1}, // B22
			{0, 0, 2, 0, -1, 0}, // B23
			{0, 0, 2, 0, 1, 0}, // B24
			{0, 0, 0, 0.5, 0, 2}, // B25
			{0, 0, 0, 0.5, 0, 0}, // B26
			{0, 0, 0, 0.5, 0, 0}, // B27
			{0, 0, 0, 0.5, 0, 0.5}, // B28
			{0, 0, 0, 0, 0, 0}  // B29
	};

	public static int getSymmetryRestrictionMatrixNumberFromInvariantMatrix(double[] bInv) {
		int index;

		boolean found = false;
		for (index = 0; !found && index < symmetryRestrictionB.length; index++) {
			double[] b = imposeRestrictions(index, bInv);
			found = areSimilar(b, bInv);
//			Misc.printMatrix("Check: " + found + " ", b);
//			System.out.println();
		}

		return --index;
	}

	private static boolean areSimilar(double[] b, double[] bInv) {
		boolean similar = true;
		for (int i = 0; similar && i < bInv.length; i++)
			similar = Math.abs(b[i] - bInv[i]) < 1.0E-6;
		return similar;
	}

	private static double[] imposeRestrictions(int index, double[] bInv) {
		// we suppose the matrix is symmetrical and compute only upper half

		double[] b = new double[bInv.length];
		for (int i = 0; i < bInv.length; i++) {
			if (symmetryRestrictionB[index][i] == i + 1) {
				b[i] = bInv[i];
			} else if (symmetryRestrictionB[index][i] == 0) {
				b[i] = 0;
			} else {
				int j = symmetryRestrictionB[index][i] - 1;
				b[i] = b[j] * symmetryRestrictionRules[index][i];
			}
		}

		return b;
	}

	private void setRulesForAnisotropicBfactors(int index) {
		for (int i = 0; i < 6; i++) {
			if (symmetryRestrictionB[index][i] == i + 1) {
				getAnisoBfactor(i).resetParameterBound();
				getAnisoBfactor(i).setRefinable();
			} else if (symmetryRestrictionB[index][i] == 0) {
				getAnisoBfactor(i).resetParameterBound();
				getAnisoBfactor(i).setValue(0);
				getAnisoBfactor(i).setNotRefinable();
			} else {
				getAnisoBfactor(i).setEqualTo(getAnisoBfactor(symmetryRestrictionB[index][i] - 1),
						symmetryRestrictionRules[index][i], 0);
			}
		}

	}

	public void addAtomWithSymbol(String ca) {
//		System.out.println("Adding: " + ca);
		AtomScatterer atomScatterer = new AtomScatterer(this, ca);
		addsubordinateloopField(scattererLoopID, atomScatterer);
	}
}

