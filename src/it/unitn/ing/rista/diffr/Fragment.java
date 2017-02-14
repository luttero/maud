/*
 * @(#)Fragment.java created Feb 21, 2003 Mesiano
 *
 * Copyright (c) 2003 Mauro Bortolotti, Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the authors.
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

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.AtomsStructureI;
import it.unitn.ing.rista.diffr.structure.StructureAtomic;

import java.util.Vector;

/**
 *  The Fragment is a
 *
 *
 * @version $Revision: 1.33 $, $Date: 2006/01/19 14:45:54 $
 * @author Mauro Bortolotti
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Fragment extends XRDcat implements AtomsStructureI {

// Luca  private Vector atomRelativeCoordinates;
// Luca  private Vector fragmentRelativeCoordinates;
// Luca  private Vector fragmentRelativeOrientation;
  //private Coordinates centerOfMass;
  //private double mass;
  //private boolean recalcCenterOfMass = true;

  static final int xcoordNumber = 0, ycoordNumber = 1, zcoordNumber = 2;
  static final int aNumber = 3, bNumber = 4, cNumber = 5;

  protected static String[] diclistc = {

    //"_fragment_fixed_pivot",

    "_fragment_pivot_x",
    "_fragment_pivot_y",
    "_fragment_pivot_z",
    "_fragment_angle_a",
    "_fragment_angle_b",
    "_fragment_angle_c",

    "_atom_site_label",
    "_fragment_label",
    "_bond_label"

  };
  protected static String[] diclistcrm = {

    //"_fragment_fixed_pivot",

    "pivot position x (fraction)",
    "pivot position y (fraction)",
    "pivot position z (fraction)",
    "rotation angle alpha (deg)",
    "rotation angle beta (deg)",
    "rotation angle gamma (deg)",

    "_atom_site_label",
    "_fragment_label",
    "_bond_label"

  };

  protected static String[] classlistc = {

    "it.unitn.ing.rista.diffr.AtomSite",
    "it.unitn.ing.rista.diffr.Fragment",
    "it.unitn.ing.rista.diffr.Bond"

  };

  protected static String[] classlistcs = {};

  static final int AtomListID = 0;          // I made it similar to StructureAtomic, so we can sync the two
  static final int FragmentListID = 1;      // Unluckily we cannot made one the subclass of the other, so
  static final int BondListID = 2;          // the methods are duplicated

  public Fragment(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    //atomRelativeCoordinates = new Vector();
    //fragmentRelativeCoordinates = new Vector();
    //fragmentRelativeOrientation = new Vector();
//    updateRelativeCoords();
  }

  public Fragment(XRDcat aobj) {
    this(aobj, "Fragment x");
  }

  public void initConstant() {
    //Nstring = 1;
    Nstringloop = 0;
    Nparameter = 6;
    Nparameterloop = 0;
    Nsubordinate = classlistcs.length;
    Nsubordinateloop = classlistc.length;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
    for (int i = 0; i < totsubordinate - totparameterloop; i++)
      classlists[i] = classlistcs[i];
  }

  public void initParameters() {
    super.initParameters();
  }

  /*public boolean isFixedPivot() {
    return Boolean.valueOf(stringField[0]).booleanValue();
    //return Boolean.getBoolean(stringField[0]);
  }

  public void setFixedPivot(boolean isfixed) {
    stringField[0] = String.valueOf(isfixed);
  }*/

  public Parameter getX() {
    return parameterField[0];
  }

  public Parameter getY() {
    return parameterField[1];
  }

  public Parameter getZ() {
    return parameterField[2];
  }

  public Parameter getA() {
    return parameterField[3];
  }

  public Parameter getB() {
    return parameterField[4];
  }

  public Parameter getC() {
    return parameterField[5];
  }

  public double getPivotX() {
    return getParameterValue(0); // this is valid only after updateParameterToDoubleBuffering
    // (normally called after a refreshAll before spectra computation)
    // but it is faster as there is no String to double conversion
//    return parameterField[0].getValueD(); // safer but slower
  }

  public double getPivotY() {
    return getParameterValue(1); // this is valid only after updateParameterToDoubleBuffering
    // (normally called after a refreshAll before spectra computation)
    // but it is faster as there is no String to double conversion
//    return parameterField[1].getValueD(); // safer but slower
  }

  public double getPivotZ() {
    return getParameterValue(2); // this is valid only after updateParameterToDoubleBuffering
    // (normally called after a refreshAll before spectra computation)
    // but it is faster as there is no String to double conversion
//    return parameterField[2].getValueD(); // safer but slower
  }

  public double getAngleA() {
    return Constants.DEGTOPI * getParameterValue(3); // this is valid only after updateParameterToDoubleBuffering
    // (normally called after a refreshAll before spectra computation)
    // but it is faster as there is no String to double conversion
//    return parameterField[3].getValueD(); // safer but slower
  }

  public double getAngleB() {
    return Constants.DEGTOPI * getParameterValue(4); // this is valid only after updateParameterToDoubleBuffering
    // (normally called after a refreshAll before spectra computation)
    // but it is faster as there is no String to double conversion
//    return parameterField[4].getValueD(); // safer but slower
  }

  public double getAngleC() {
    return Constants.DEGTOPI * getParameterValue(5); // this is valid only after updateParameterToDoubleBuffering
    // (normally called after a refreshAll before spectra computation)
    // but it is faster as there is no String to double conversion
//    return parameterField[5].getValueD(); // safer but slower
  }

  public void setAngleA(double a) {
    while (a > Constants.PI2)
      a -= Constants.PI2;
    while (a < 0.0)
      a += Constants.PI2;
    parameterField[3].setValue(a * Constants.PITODEG);
  }

  public void setAngleB(double b) {
    while (b > Constants.PI2)
      b -= Constants.PI2;
    while (b < 0.0)
      b += Constants.PI2;
    parameterField[3].setValue(b * Constants.PITODEG);
  }

  public void setAngleC(double c) {
    while (c > Constants.PI2)
      c -= Constants.PI2;
    while (c < 0.0)
      c += Constants.PI2;
    parameterField[3].setValue(c * Constants.PITODEG);
  }

  /** Set pivot center coordinates and updates, if specified, relative coordinates of subordinate atoms and fragments.
   *  Absolute coordinates of subordinate atoms and fragments remains unchanged.
   *
   * @param x new X coordinate of pivot center
   * @param y new Y coordinate of pivot center
   * @param z new Z coordinate of pivot center
   * @param updateRelCoords if set to true, recalculate all relative coordinates of subordinate objects
   */

  private void setPivotCoords(double x, double y, double z, boolean updateRelCoords) {
    double dx = parameterField[0].getValueD() - x;
    double dy = parameterField[1].getValueD() - y;
    double dz = parameterField[2].getValueD() - z;
    parameterField[0].setValue(x);
    parameterField[1].setValue(y);
    parameterField[2].setValue(z);
    if (updateRelCoords)
      updateRelativeCoords(dx, dy, dz);
  }

  /** Set pivot center coordinates and updates, if specified, relative coordinates of subordinate atoms and fragments.
   *  Absolute coordinates of subordinate atoms and fragments remains unchanged.
   *
   * @param new_pivot fixed pivot coordinates
   * @param updateRelCoords if set to true, recalculate all relative coordinates of subordinate objects
   */

  public void setPivotCoords(Coordinates new_pivot, boolean updateRelCoords) {
    setPivotCoords(new_pivot.x, new_pivot.y, new_pivot.z, updateRelCoords);
  }

  /** Set fixed pivot center coordinates and updates relative coordinates of subordinate atoms and fragments.
   *  Absolute coordinates of subordinate atoms and fragments remains unchanged
   *
   * @param new_pivot new fixed pivot coordinates
   */

  public void setPivotCoords(Coordinates new_pivot) {
    setPivotCoords(new_pivot.x, new_pivot.y, new_pivot.z, true);
  }

  public void addToPivot(double dx, double dy, double dz, boolean updateRelCoords) {
    setPivotCoords(parameterField[0].getValueD() + dx, parameterField[1].getValueD() + dy, 
        parameterField[2].getValueD() + dz, updateRelCoords);
  }

  /** Set fragment orientation and updates, if specified, relative coordinates of subordinate atoms and fragments.
   *  Absolute coordinates of subordinate atoms and fragments remains unchanged
   *
   * @param a A angle
   * @param b B angle
   * @param c C angle
   * @param updateRelCoords if set to true, recalculate all relative coordinates of subordinate objects
   */

  private void setOrientation(double a, double b, double c, boolean updateRelCoords) {
    while (a > Constants.PI2)
      a -= Constants.PI2;
    while (a < 0.0)
      a += Constants.PI2;
    while (b > Constants.PI2)
      b -= Constants.PI2;
    while (b < 0.0)
      b += Constants.PI2;
    while (c > Constants.PI2)
      c -= Constants.PI2;
    while (c < 0.0)
      c += Constants.PI2;
    parameterField[3].setValue(a * Constants.PITODEG);
    parameterField[4].setValue(b * Constants.PITODEG);
    parameterField[5].setValue(c * Constants.PITODEG);
//    if (updateRelCoords)
//      updateRelativeCoords();
  }

  /** Set fragment orientation and updates, if specified, relative coordinates of subordinate atoms and fragments.
   *  Absolute coordinates of subordinate atoms and fragments remains unchanged
   *
   * @param orient new orientation coordinates
   * @param updateRelCoords if set to true, recalculate all relative coordinates of subordinate objects
   */

  public void setOrientation(Coordinates orient, boolean updateRelCoords) {
    setOrientation(orient.x, orient.y, orient.z, updateRelCoords);
  }

  /** Set fragment orientation and updates relative coordinates of subordinate atoms and fragments.
   *  Absolute coordinates of subordinate atoms and fragments remains unchanged
   *
   * @param orient new orientation coordinates
   */

  public void setOrientation(Coordinates orient) {
    setOrientation(orient.x, orient.y, orient.z, true);
  }

  /** Return pivot point coordinates
   *
   * @return pivot point coordinates
   */

  public Coordinates getPivotCoords() {
    double x = getPivotX();
    double y = getPivotY();
    double z = getPivotZ();
    return new Coordinates(x, y, z);
  }

  /** Return fragment orientation
   *
   * @return fragment orientation
   */

  public Coordinates getOrientation() {
    double a = getAngleA();
    double b = getAngleB();
    double c = getAngleC();
    return new Coordinates(a, b, c);
  }

  /** Calculates atom coordinates relative to fragment system from atom "absolute"
   *  coordinates and current fragment position and orientation
   *
   * @param abs_c absolute coordinates of subordinate atom
   * @return Coordinates relative coordinates
   */

  public Coordinates calcRelativeCoords(Coordinates abs_c) {
    // Luca: OK
    XRDcat parent = getParent();
    if (parent instanceof Fragment)
      abs_c = ((Fragment) parent).calcRelativeCoords(abs_c);
    Coordinates pvt_c = getPivotCoords();
    double x_diff = abs_c.x;// - pvt_c.x;  now the rotation is always at the zero
    double y_diff = abs_c.y;// - pvt_c.y;
    double z_diff = abs_c.z;// - pvt_c.z;
    double[][] EI = eulerMatrix(getOrientation());
    double x_rel = EI[0][0] * x_diff + EI[0][1] * y_diff + EI[0][2] * z_diff;
    double y_rel = EI[1][0] * x_diff + EI[1][1] * y_diff + EI[1][2] * z_diff;
    double z_rel = EI[2][0] * x_diff + EI[2][1] * y_diff + EI[2][2] * z_diff;
    double x_piv = EI[0][0] * pvt_c.x + EI[0][1] * pvt_c.y + EI[0][2] * pvt_c.z;
    double y_piv = EI[1][0] * pvt_c.x + EI[1][1] * pvt_c.y + EI[1][2] * pvt_c.z;
    double z_piv = EI[2][0] * pvt_c.x + EI[2][1] * pvt_c.y + EI[2][2] * pvt_c.z;
    return new Coordinates(x_rel - x_piv, y_rel - y_piv, z_rel - z_piv);
  }

  public double[] calcRelativeCoords(double[] abs_c) {
    // Luca: OK
    XRDcat parent = getParent();
    if (parent instanceof Fragment)
      abs_c = ((Fragment) parent).calcRelativeCoords(abs_c);
    Coordinates pvt_c = getPivotCoords();
    double x_diff = abs_c[0];// - pvt_c.x;
    double y_diff = abs_c[1];// - pvt_c.y;
    double z_diff = abs_c[2];// - pvt_c.z;
    double[][] EI = eulerMatrix(getOrientation());
    double x_rel = EI[0][0] * x_diff + EI[0][1] * y_diff + EI[0][2] * z_diff;
    double y_rel = EI[1][0] * x_diff + EI[1][1] * y_diff + EI[1][2] * z_diff;
    double z_rel = EI[2][0] * x_diff + EI[2][1] * y_diff + EI[2][2] * z_diff;
    double x_piv = EI[0][0] * pvt_c.x + EI[0][1] * pvt_c.y + EI[0][2] * pvt_c.z;
    double y_piv = EI[1][0] * pvt_c.x + EI[1][1] * pvt_c.y + EI[1][2] * pvt_c.z;
    double z_piv = EI[2][0] * pvt_c.x + EI[2][1] * pvt_c.y + EI[2][2] * pvt_c.z;
    double[] coord = {x_rel - x_piv, y_rel - y_piv, z_rel - z_piv};
    return coord;
  }

  /** Calculates atom coordinates absolute in the cell system from atom "relative"
   *  coordinates and current fragment position and orientation
   *
   * @param abs_c relative coordinates of subordinate atom
   * @return Coordinates absolute coordinates
   */

  public Coordinates calcAbsoluteCoords(Coordinates abs_c) {
    // Luca: OK
    double EI[][] = eulerMatrix(getOrientation());
    Coordinates pvt_c = getPivotCoords();
    double x_piv = abs_c.x + pvt_c.x;
    double y_piv = abs_c.y + pvt_c.y;
    double z_piv = abs_c.z + pvt_c.z;
    double x_rel = EI[0][0] * x_piv + EI[0][1] * y_piv + EI[0][2] * z_piv;
    double y_rel = EI[1][0] * x_piv + EI[1][1] * y_piv + EI[1][2] * z_piv;
    double z_rel = EI[2][0] * x_piv + EI[2][1] * y_piv + EI[2][2] * z_piv;
//    Coordinates pvt_c = getPivotCoords();
//    x_rel += pvt_c.x;
//    y_rel += pvt_c.y;
//    z_rel += pvt_c.z;
    XRDcat parent = getParent();
    Coordinates newCoord = new Coordinates(x_rel, y_rel, z_rel);
    if (parent instanceof Fragment)
      newCoord = ((Fragment) parent).calcAbsoluteCoords(newCoord);
    return newCoord;
  }

  /** Calculates atom coordinates absolute in the cell system from atom "relative"
   *  coordinates and current fragment position and orientation
   *
   * @param abs_c relative coordinates of subordinate atom
   * @return double[] absolute coordinate
   */
  public double[] calcAbsoluteCoords(double[] abs_c) {
    // Luca: OK
    double EI[][] = eulerMatrix(getOrientation());
    Coordinates pvt_c = getPivotCoords();
    double x_piv = abs_c[0] + pvt_c.x;
    double y_piv = abs_c[1] + pvt_c.y;
    double z_piv = abs_c[2] + pvt_c.z;
    double x_rel = EI[0][0] * x_piv + EI[0][1] * y_piv + EI[0][2] * z_piv;
    double y_rel = EI[1][0] * x_piv + EI[1][1] * y_piv + EI[1][2] * z_piv;
    double z_rel = EI[2][0] * x_piv + EI[2][1] * y_piv + EI[2][2] * z_piv;
//    Coordinates pvt_c = getPivotCoords();
//    x_rel += pvt_c.x;
//    y_rel += pvt_c.y;
//    z_rel += pvt_c.z;
    XRDcat parent = getParent();
    double[] newCoord = new double[3];
    newCoord[0] = x_rel;
    newCoord[1] = y_rel;
    newCoord[2] = z_rel;
    if (parent instanceof Fragment)
      newCoord = ((Fragment) parent).calcAbsoluteCoords(newCoord);
    return newCoord;
  }

  /** Calculates subordinate fragment coordinates relative to fragment system from fragment "absolute"
   *  coordinates and current fragment position and orientation
   *
   * @param frag subordinate fragment
   * @return Coordinates relative coordinates
   */

/*  private Coordinates calcRelativeCoords(Fragment frag) {
    Coordinates pvt_c = getPivotCoords();
    Coordinates abs_c = frag.getPivotCoords();
    double x_diff = abs_c.x - pvt_c.x;
    double y_diff = abs_c.y - pvt_c.y;
    double z_diff = abs_c.z - pvt_c.z;
    double[][] EI = eulerMatrixInverted(getOrientation());
    double x_rel = EI[0][0] * x_diff + EI[0][1] * y_diff + EI[0][2] * z_diff;
    double y_rel = EI[1][0] * x_diff + EI[1][1] * y_diff + EI[1][2] * z_diff;
    double z_rel = EI[2][0] * x_diff + EI[2][1] * y_diff + EI[2][2] * z_diff;
    return new Coordinates(x_rel, y_rel, z_rel);
  }*/

  /** Calculates subordinate fragment orientation relative to fragment system from fragment "absolute"
   *  orientation and current fragment position and orientation
   *
   * @param frag subordinate fragment
   * @return Coordinates relative orientation
   */

/*  private Coordinates calcRelativeOrient(Fragment frag) {
    Coordinates rel_o = frag.getOrientation();
    Coordinates abs_o = getOrientation();
    double a_diff = abs_o.x - rel_o.x;
    double b_diff = abs_o.y - rel_o.y;
    double c_diff = abs_o.z - rel_o.z;
    return new Coordinates(a_diff, b_diff, c_diff);
  }*/

  /**
   * Recursively updates subordinate objects coordinates relative to fragment system from absolute
   * coordinates and current fragment position and orientation
   */

  public void updateRelativeCoords(double dx, double dy, double dz) {
//    System.out.println("updateRelativeCoords()");
//    System.out.println("getAtomNumber() " + getAtomNumber());
//    System.out.println("getFragmentNumber()" + getFragmentNumber());
    int na = getAtomNumber();
    int nf = getFragmentNumber();
    for (int i = 0; i < na; i++) {
      AtomSite anatom = getAtom(i);
      anatom.refreshPositions(true);
      Coordinates coord = anatom.getLocalCoordinates();
      coord.x += dx;
      coord.y += dy;
      coord.z += dz;
      anatom.setLocalCoordinates(coord);
      anatom.refreshPositions(true);
    }
    for (int i = 0; i < nf; i++) {
      Fragment f_tmp = (Fragment) getFragmentList().get(i);
      f_tmp.addToPivot(dx, dy, dz, false);
//      fragmentRelativeOrientation.add(calcRelativeOrient(f_tmp));
    }
  }

  /**
   * Recursively updates subordinate objects absolute coordinates from relative
   * coordinates and current fragment position and orientation
   */

/*  public void updateAbsoluteCoordinates() {

//    System.out.println("Fragment: updateAbsoluteCoordinates()");

    double X = getPivotX();
    double Y = getPivotY();
    double Z = getPivotZ();
    double A = getAngleA();
    double B = getAngleB();
    double C = getAngleC();

    double EI[][] = eulerMatrixInverted(A, B, C);

// Updates all child atoms

    for (int na = 0; na < getAtomNumber(); na++) {

      AtomSite a_tmp = getAtom(na);
      a_tmp.refreshPositions();
//System.out.println("a_tmp.getx(0): " + a_tmp.getx(0));
//System.out.println("a_tmp.gety(0): " + a_tmp.gety(0));
//System.out.println("a_tmp.getz(0): " + a_tmp.getz(0));

      double x_rel = ((Coordinates) atomRelativeCoordinates.elementAt(na)).x;
      double y_rel = ((Coordinates) atomRelativeCoordinates.elementAt(na)).y;
      double z_rel = ((Coordinates) atomRelativeCoordinates.elementAt(na)).z;
//System.out.println("x_rel: " + x_rel);
//System.out.println("y_rel: " + y_rel);
//System.out.println("z_rel: " + z_rel);
      double x_abs = X + x_rel * EI[0][0] + y_rel * EI[0][1] + z_rel * EI[0][2];
      double y_abs = Y + x_rel * EI[1][0] + y_rel * EI[1][1] + z_rel * EI[1][2];
      double z_abs = Z + x_rel * EI[2][0] + y_rel * EI[2][1] + z_rel * EI[2][2];
      a_tmp.setCoordinates(new Coordinates(x_abs, y_abs, z_abs));
      a_tmp.refreshPositions();
//System.out.println("a_tmp.getx(0): " + a_tmp.getx(0));
//System.out.println("a_tmp.gety(0): " + a_tmp.gety(0));
//System.out.println("a_tmp.getz(0): " + a_tmp.getz(0));

    }

// Updates all child fragments

    for (int nf = 0; nf < getFragmentNumber(); nf++) {

      Fragment f_tmp = getFragment(nf);

      double x_rel = ((Coordinates) fragmentRelativeCoordinates.elementAt(nf)).x;
      double y_rel = ((Coordinates) fragmentRelativeCoordinates.elementAt(nf)).y;
      double z_rel = ((Coordinates) fragmentRelativeCoordinates.elementAt(nf)).z;
      double x_abs = X + x_rel * EI[0][0] + y_rel * EI[0][1] + z_rel * EI[0][2];
      double y_abs = Y + x_rel * EI[1][0] + y_rel * EI[1][1] + z_rel * EI[1][2];
      double z_abs = Z + x_rel * EI[2][0] + y_rel * EI[2][1] + z_rel * EI[2][2];
      f_tmp.setPivotCoords(x_abs, y_abs, z_abs, false);

      double a_rel = ((Coordinates) fragmentRelativeOrientation.elementAt(nf)).x;
      double b_rel = ((Coordinates) fragmentRelativeOrientation.elementAt(nf)).y;
      double c_rel = ((Coordinates) fragmentRelativeOrientation.elementAt(nf)).z;
      double a_abs = A + a_rel;
      double b_abs = B + b_rel;
      double c_abs = C + c_rel;
      f_tmp.setOrientation(a_abs, b_abs, c_abs, false);

      f_tmp.updateAbsoluteCoordinates();
    }
  }*/

  /* Perform an absolute rigid - body translation of the entire fragment.
   *  Updates recursively the absolute coordinates of all the subordinate objects.
   *
   * @param x new pivot X coordinate
   * @param y new pivot Y coordinate
   * @param z new pivot Z coordinate
   */

/*  public void translateTo(double x, double y, double z) {
    Coordinates old_pivot = getPivotCoords();
    double x_diff = x - old_pivot.x;
    double y_diff = y - old_pivot.y;
    double z_diff = z - old_pivot.z;
    for (int na = 0; na < getAtomList().size(); na++) {
      AtomSite atm = getAtom(na);
      atm.refreshPositions();
      Coordinates a_coord = atm.getCoordinates();
      atm.setCoordinates(new Coordinates((a_coord.x + x_diff), (a_coord.y + y_diff), (a_coord.z + z_diff)));
      atm.refreshPositions();
    }
    for (int nf = 0; nf < getFragmentList().size(); nf++) {
      Fragment frg = (Fragment) getFragmentList().get(nf);
      Coordinates f_coord = frg.getPivotCoords();
      ((Fragment) getFragmentList().get(nf)).translateTo((f_coord.x + x_diff), (f_coord.y + y_diff), (f_coord.z + z_diff));
    }
    setPivotCoords(x, y, z, false);
  }*/

  /* Perform a relative rigid - body translation of the entire fragment.
   *  Updates recursively the absolute coordinates of all the subordinate objects.
   *
   * @param dx X axis pivot translation
   * @param dy Y axis pivot translation
   * @param dz Z axis pivot translation
   */

/*  public void translate(double dx, double dy, double dz) {
    Coordinates old_pivot = getPivotCoords();
    for (int na = 0; na < getAtomList().size(); na++) {
      AtomSite atm = getAtom(na);
      atm.refreshPositions();
      Coordinates a_coord = atm.getCoordinates();
      atm.setCoordinates(new Coordinates((a_coord.x + dx), (a_coord.y + dy), (a_coord.z + dz)));
      atm.refreshPositions();
    }
    for (int nf = 0; nf < getFragmentList().size(); nf++) {
      Fragment frg = (Fragment) getFragmentList().get(nf);
      Coordinates f_coord = frg.getPivotCoords();
      ((Fragment) getFragmentList().get(nf)).translateTo((f_coord.x + dx), (f_coord.y + dy), (f_coord.z + dz));
    }
    setPivotCoords(old_pivot.x + dx, old_pivot.y + dy, old_pivot.z + dz, false);
  }*/

  /* Perform an absolute rigid - body rotation of the entire fragment.
   *  Updates recursively the absolute coordinates of all the subordinate objects.
   *
   * @param a new fragment A orientation angle
   * @param b new fragment B orientation angle
   * @param c new fragment C orientation angle
   */

/*  public void rotateTo(double a, double b, double c) {
    Coordinates pivot = getPivotCoords();
    Coordinates old_orient = getOrientation();
    double a_diff = a - old_orient.x;
    double b_diff = b - old_orient.y;
    double c_diff = c - old_orient.z;
    double[][] EI = eulerMatrixInverted(a_diff, b_diff, c_diff);
    for (int na = 0; na < getAtomList().size(); na++) {
      Coordinates rel_coords = getAtomRelativeCoordinates(na);
      double x_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double y_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double z_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      AtomSite atm = getAtom(na);
      atm.setCoordinates(new Coordinates((pivot.x + x_rot), (pivot.y + y_rot), (pivot.z + z_rot)));
      atm.refreshPositions();
    }
    for (int nf = 0; nf < getFragmentList().size(); nf++) {
      Coordinates rel_coords = getFragmentRelativeCoordinates(nf);
      double x_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double y_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double z_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      getFragment(nf).translate(x_rot, y_rot, z_rot);
      getFragment(nf).rotate(a_diff, b_diff, c_diff);
    }
    setOrientation(a, b, c, false);
  }*/

  /* Perform a relative rigid - body rotation of the entire fragment.
   *  Updates recursively the absolute coordinates of all the subordinate objects.
   * @param da A angle rotation
   * @param db B angle rotation
   * @param dc C angle rotation
   */

/*  public void rotate(double da, double db, double dc) {
    Coordinates pivot = getPivotCoords();
    Coordinates old_orient = getOrientation();
    double[][] EI = eulerMatrixInverted(da, db, dc);
    for (int na = 0; na < getAtomList().size(); na++) {
      AtomSite atm = getAtom(na);
      Coordinates rel_coords = getAtomRelativeCoordinates(na);
      double x_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double y_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double z_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      atm.setCoordinates(new Coordinates((pivot.x + x_rot), (pivot.y + y_rot), (pivot.z + z_rot)));
      atm.refreshPositions();
    }
    for (int nf = 0; nf < getFragmentList().size(); nf++) {
      Coordinates rel_coords = getFragmentRelativeCoordinates(nf);
      double x_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double y_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double z_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      getFragment(nf).translate(x_rot, y_rot, z_rot);
      getFragment(nf).rotate(da, db, dc);
    }
    setOrientation(old_orient.x + da, old_orient.y + db, old_orient.z + dc, false);
  }*/

  /*
   * Perform an absolute rigid body transformation of the entire fragment.
   * Updates recursively the absolute coordinates of all the subordinate objects.
   *
   * @param x x new pivot X coordinate
   * @param y x new pivot X coordinate
   * @param z x new pivot X coordinate
   * @param a new fragment A orientation angle
   * @param b new fragment B orientation angle
   * @param c new fragment C orientation angle
   */

/*  public void transformTo(double x, double y, double z, double a, double b, double c) {
//    System.out.println("transformTo(...)");
    Coordinates old_pivot = getPivotCoords();
//    System.out.println("old_pivot.x " + old_pivot.x);
//    System.out.println("old_pivot.y " + old_pivot.y);
//    System.out.println("old_pivot.z " + old_pivot.z);
    double x_diff = x - old_pivot.x;
    double y_diff = y - old_pivot.y;
    double z_diff = z - old_pivot.z;
    Coordinates old_orient = getOrientation();
    double a_diff = a - old_orient.x;
    double b_diff = b - old_orient.y;
    double c_diff = c - old_orient.z;
    double[][] EI = eulerMatrixInverted(a_diff, b_diff, c_diff);

//    System.out.println("getAtomList().size() " + getAtomList().size());
    for (int na = 0; na < getAtomList().size(); na++) {
      AtomSite atm = getAtom(na);
      atm.refreshPositions();
      Coordinates a_coord = atm.getCoordinates();
      Coordinates rel_coords = getAtomRelativeCoordinates(na);
      double x_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double y_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double z_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      atm.setCoordinates(new Coordinates((a_coord.x + x_diff + x_rot), (a_coord.y + y_diff + y_rot), (a_coord.z + z_diff + z_rot)));
      atm.refreshPositions();
    }

//    System.out.println("getFragmentList().size() " + getFragmentList().size());
    for (int nf = 0; nf < getFragmentList().size(); nf++) {
      Coordinates rel_coords = getFragmentRelativeCoordinates(nf);
      double x_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double y_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double z_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      getFragment(nf).translate(x_diff + x_rot, y_diff + y_rot, z_diff + z_rot);
      getFragment(nf).rotate(a_diff, b_diff, c_diff);
    }

    setPivotCoords(x, y, z, false);
    setOrientation(a, b, c, false);
  }*/

  /*
   * Perform a relative rigid body transformation of the entire fragment.
   * Updates recursively the absolute coordinates of all the subordinate objects.
   *
   * @param dx X axis pivot translation
   * @param dy Y axis pivot translation
   * @param dz Z axis pivot translation
   * @param da A angle rotation
   * @param db B angle rotation
   * @param dc C angle rotation
   */

/*  public void transform(double dx, double dy, double dz, double da, double db, double dc) {
    Coordinates old_pivot = getPivotCoords();
    Coordinates old_orient = getOrientation();
    double[][] EI = eulerMatrixInverted(da, db, dc);
    for (int na = 0; na < getAtomList().size(); na++) {
      AtomSite atm = getAtom(na);
      atm.refreshPositions();
      Coordinates a_coord = atm.getCoordinates();
      Coordinates rel_coords = getAtomRelativeCoordinates(na);
      double x_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double y_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double z_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      atm.setCoordinates(new Coordinates((a_coord.x + dx + x_rot), (a_coord.y + dy + y_rot), (a_coord.z + dz + z_rot)));
      atm.refreshPositions();
    }
    for (int nf = 0; nf < getFragmentList().size(); nf++) {
      Coordinates rel_coords = getFragmentRelativeCoordinates(nf);
      double x_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double y_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      double z_rot = EI[0][0] * rel_coords.x + EI[0][1] * rel_coords.y + EI[0][2] * rel_coords.z;
      getFragment(nf).translate(dx + x_rot, dy + y_rot, dz + z_rot);
      getFragment(nf).rotate(da, db, dc);
    }

    setPivotCoords(old_pivot.x + dx, old_pivot.y + dy, old_pivot.z + dz, false);
    setOrientation(old_orient.x + da, old_orient.y + db, old_orient.z + dc, false);

  }*/

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
    return numberofelementSubL(AtomListID);
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
    addsubordinateloopField(0, newatom);
    getPhaseParent().refreshAtoms = true;
    getPhaseParent().fullAtomList = null;
    getPhaseParent().refreshFhklcomp = true;

  }

  public boolean removeSelectedAtom() {
    return removeselSubLField(0);
  }

  public void removeAtomAt(int number) {
    getAtomList().removeItemAt(number);
    getPhaseParent().refreshAtoms = true;
    getPhaseParent().fullAtomList = null;
    getPhaseParent().refreshFhklcomp = true;
  }

/*  public void switchAtomPositionAandB() {
    if (getAtomList() == null)
      return;
    int numberInternalAtoms = getAtomList().size();
    for (int i = 0; i < numberInternalAtoms; i++) {
      getAtom(i).switchPositionAandB();
    }
    swithPositionAandB();
    for (int i = 0; i < getFragmentNumber(); i++)
      getFragment(i).switchAtomPositionAandB();
  }*/

  public void swithAandB() {
    String label1 = getX().getLabel();
    String label2 = getY().getLabel();
    Parameter temp = getY();
    parameterField[ycoordNumber] = getX();
    parameterField[ycoordNumber].setLabel(label2);
    parameterField[xcoordNumber] = temp;
    parameterField[xcoordNumber].setLabel(label1);

    label1 = getA().getLabel();
    label2 = getB().getLabel();
    temp = getB();
    parameterField[bNumber] = getA();
    parameterField[bNumber].setLabel(label2);
    parameterField[aNumber] = temp;
    parameterField[aNumber].setLabel(label1);
  }

  public void swithBandC() {
    String label1 = getZ().getLabel();
    String label2 = getY().getLabel();
    Parameter temp = getY();
    parameterField[ycoordNumber] = getZ();
    parameterField[ycoordNumber].setLabel(label2);
    parameterField[zcoordNumber] = temp;
    parameterField[zcoordNumber].setLabel(label1);

    label1 = getC().getLabel();
    label2 = getB().getLabel();
    temp = getB();
    parameterField[bNumber] = getC();
    parameterField[bNumber].setLabel(label2);
    parameterField[cNumber] = temp;
    parameterField[cNumber].setLabel(label1);
  }

  public void swithCandA() {
    String label1 = getX().getLabel();
    String label2 = getZ().getLabel();
    Parameter temp = getZ();
    parameterField[zcoordNumber] = getZ();
    parameterField[zcoordNumber].setLabel(label2);
    parameterField[xcoordNumber] = temp;
    parameterField[xcoordNumber].setLabel(label1);

    label1 = getA().getLabel();
    label2 = getC().getLabel();
    temp = getC();
    parameterField[cNumber] = getA();
    parameterField[cNumber].setLabel(label2);
    parameterField[aNumber] = temp;
    parameterField[aNumber].setLabel(label1);
  }

/*  public void invertA() {
    if (getAtomList() == null)
      return;
    int numberInternalAtoms = getAtomList().size();
    for (int i = 0; i < numberInternalAtoms; i++) {
      getAtom(i).invertA();
    }
    for (int i = 0; i < getFragmentNumber(); i++)
      getFragment(i).invertA();
  }

  public void invertB() {
    if (getAtomList() == null)
      return;
    int numberInternalAtoms = getAtomList().size();
    for (int i = 0; i < numberInternalAtoms; i++) {
      getAtom(i).invertB();
    }
    for (int i = 0; i < getFragmentNumber(); i++)
      getFragment(i).invertB();
  }

  public void invertC() {
    if (getAtomList() == null)
      return;
    int numberInternalAtoms = getAtomList().size();
    for (int i = 0; i < numberInternalAtoms; i++) {
      getAtom(i).invertC();
    }
    for (int i = 0; i < getFragmentNumber(); i++)
      getFragment(i).invertC();
  }*/

/*  public void turnAtomPositionForward() {
    if (getAtomList() == null)
      return;
    int numberInternalAtoms = getAtomList().size();
    for (int i = 0; i < numberInternalAtoms; i++) {
      getAtom(i).turnPositionForward();
    }
    turnPositionForward();
    for (int i = 0; i < getFragmentNumber(); i++)
      getFragment(i).turnAtomPositionForward();
  }

  public void turnAtomPositionBackward() {
    if (getAtomList() == null)
      return;
    int numberInternalAtoms = getAtomList().size();
    for (int i = 0; i < numberInternalAtoms; i++) {
      getAtom(i).turnPositionBackward();
    }
    turnPositionBackward();
    for (int i = 0; i < getFragmentNumber(); i++)
      getFragment(i).turnAtomPositionBackward();
  }*/

  public void turnForward() {
    String label1 = getX().getLabel();
    String label2 = getY().getLabel();
    String label3 = getZ().getLabel();
    Parameter temp = getZ();
    parameterField[zcoordNumber] = getY();
    parameterField[zcoordNumber].setLabel(label3);
    parameterField[ycoordNumber] = getX();
    parameterField[ycoordNumber].setLabel(label2);
    parameterField[xcoordNumber] = temp;
    parameterField[xcoordNumber].setLabel(label1);

    label1 = getA().getLabel();
    label2 = getB().getLabel();
    label3 = getC().getLabel();
    temp = getC();
    parameterField[cNumber] = getB();
    parameterField[cNumber].setLabel(label3);
    parameterField[bNumber] = getA();
    parameterField[bNumber].setLabel(label2);
    parameterField[aNumber] = temp;
    parameterField[aNumber].setLabel(label1);
  }

  public void turnBackward() {
    String label1 = getX().getLabel();
    String label2 = getY().getLabel();
    String label3 = getZ().getLabel();
    Parameter temp = getX();
    parameterField[xcoordNumber] = getY();
    parameterField[xcoordNumber].setLabel(label1);
    parameterField[ycoordNumber] = getZ();
    parameterField[ycoordNumber].setLabel(label2);
    parameterField[zcoordNumber] = temp;
    parameterField[zcoordNumber].setLabel(label3);

    label1 = getA().getLabel();
    label2 = getB().getLabel();
    label3 = getC().getLabel();
    temp = getA();
    parameterField[aNumber] = getB();
    parameterField[aNumber].setLabel(label1);
    parameterField[bNumber] = getC();
    parameterField[bNumber].setLabel(label2);
    parameterField[cNumber] = temp;
    parameterField[cNumber].setLabel(label3);
  }

  public boolean getQuantityFromOccupancy() {
    return getParentBaseStructure().getQuantityFromOccupancy();
  }

  public void setQuantityFromOccupancy(boolean status) {
    getParentBaseStructure().setQuantityFromOccupancy(status);
  }

	public boolean isDebyeWallerModelDimensionLess() {
		return getParentBaseStructure().isDebyeWallerModelDimensionLess();
	}

	public void setDebyeWallerModelDimensionLess(boolean value) {
		getParentBaseStructure().setDebyeWallerModelDimensionLess(value);
	}

	public StructureAtomic getParentBaseStructure() {
    XRDcat parent = getParent();
    while (parent != null && !(parent instanceof StructureAtomic)) {
      parent = parent.getParent();
    }
    if (parent != null)
      return (StructureAtomic) parent;
    return null;
  }

/*  public void addAtom(AtomSite atm) {
    addsubordinateloopField(AtomListID, atm);
//    System.out.println("addAtom(AtomSite atm)");
    atomRelativeCoordinates.add(calcRelativeCoords(atm));
    //  probably the following is not necessary, the refreshing should go through the usual default notify
    //  but just to be sure
//    getPhaseParent().refreshAtoms = true;
//    getPhaseParent().fullAtomList = null;
  }

  public void removeAtom(AtomSite atm) {
    removeAtomAt(getAtomList().indexOf(atm));
  }

  public void removeAtomAt(int index) {
    removeSubLFieldAt(AtomListID, index);
    atomRelativeCoordinates.remove(index);
    // probably the following is not necessary, the refreshing should go through the notify
    // but just to be sure
//    getPhaseParent().refreshAtoms = true;
//    getPhaseParent().fullAtomList = null;
  }

  public void addAtom() {
    AtomSite newatom = new AtomSite(this);
    addAtom(newatom);
    newatom.setAtomSymbol("Ca");
  }

  public void removeSelectedAtom() {
    removeAtomAt(getAtomList().getSelectedIndex());
  }*/

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
// Luca    fragmentRelativeCoordinates.add(calcRelativeCoords(frg));
// Luca    fragmentRelativeOrientation.add(calcRelativeOrient(frg));
    // probably the following is not necessary, the refreshing should go through the notify
    // but just to be sure
//    getPhaseParent().refreshAtoms = true;
//    getPhaseParent().fullAtomList = null;
  }

  public void addFragment() {
    Fragment newfragment = new Fragment(this);
    addFragment(newfragment);
  }

  public void removeFragmentAt(int number) {
// Luca    fragmentRelativeCoordinates.remove(number);
// Luca    fragmentRelativeOrientation.remove(number);
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
    // probably the following is not necessary, the refreshing should go through the notify
    // but just to be sure
//    getPhaseParent().refreshAtoms = true;
//    getPhaseParent().fullAtomList = null;
  }

  public boolean removeSelectedBond() {
    return removeselSubLField(BondListID);
  }

  public void removeBondAt(int number) {
    removeSubLFieldAt(BondListID, number);
  }

  /**
   * Returns the center of mass of the fragment.
   *
   */

  public Coordinates getCenterOfMass() {
    double M = 0.0;
    double m_x = 0.0;
    double m_y = 0.0;
    double m_z = 0.0;
    for (int na = 0; na < getAtomList().size(); na++) {
      AtomSite a_tmp = getAtom(na);
      a_tmp.refreshPositions(true);
//      System.out.println("a_tmp.getAtomSymbol() " + a_tmp.getAtomSymbol());
      Coordinates a_coord = a_tmp.getCoordinates();
      double a_mass = a_tmp.getMeanWeight();
      M += a_mass;
      m_x += a_mass * a_coord.x;
      m_y += a_mass * a_coord.y;
      m_z += a_mass * a_coord.z;
    }
    for (int nf = 0; nf < getFragmentList().size(); nf++) {
      Fragment f_tmp = (Fragment) getFragmentList().get(nf);
      Coordinates f_coord = f_tmp.getCenterOfMass();
      double f_mass = f_tmp.getWeight();
      M += f_mass;
      m_x += f_mass * f_coord.x;
      m_y += f_mass * f_coord.y;
      m_z += f_mass * f_coord.z;
    }
    return new Coordinates(m_x / M, m_y / M, m_z / M);
  }

  public double getWeight() {
    double M = 0.0;
    for (int na = 0; na < getAtomList().size(); na++)
      M += getAtom(na).getMeanWeight();
    for (int nf = 0; nf < getFragmentList().size(); nf++)
      M += getFragment(nf).getWeight();
    return M;
  }

  public static final double[][] eulerMatrix(double a, double b, double c) {
    double[][] eulerM = new double[3][3];

    double sin_a = Math.sin(a);
    double cos_a = Math.cos(a);
    double sin_b = Math.sin(b);
    double cos_b = Math.cos(b);
    double sin_c = Math.sin(c);
    double cos_c = Math.cos(c);

    eulerM[0][0] = cos_a * cos_b * cos_c - sin_a * sin_c; //cos_a * cos_b;
    eulerM[0][1] = sin_a * cos_b * cos_c + cos_a * sin_c; // sin_a * cos_c + cos_a * sin_b * sin_c;
    eulerM[0][2] = -sin_b * cos_c; // sin_a * sin_c - cos_a * sin_b * cos_c;
    eulerM[1][0] = -cos_a * cos_b * sin_c - sin_a * cos_c; // -sin_a * cos_b;
    eulerM[1][1] = -sin_a * cos_b * sin_c + cos_a * cos_c; // cos_a * cos_c - sin_a * sin_b * sin_c;
    eulerM[1][2] = sin_b * sin_c; // cos_a * sin_c + sin_a * sin_b * cos_c;
    eulerM[2][0] = cos_a * sin_b; // sin_b;
    eulerM[2][1] = sin_a * sin_b; // -cos_b * sin_c;
    eulerM[2][2] = cos_b; // cos_b * cos_c;

    return eulerM;
  }

  public static final double[][] eulerMatrix(Coordinates angles) {
    return eulerMatrix(angles.x, angles.y, angles.z);
  }

  public static final double[][] eulerMatrixInverted(double a, double b, double c) {
    double[][] eulerMInv = new double[3][3];

    double sin_a = Math.sin(a);
    double cos_a = Math.cos(a);
    double sin_b = Math.sin(b);
    double cos_b = Math.cos(b);
    double sin_c = Math.sin(c);
    double cos_c = Math.cos(c);

    eulerMInv[0][0] = cos_a * cos_b;
    eulerMInv[0][1] = -sin_a * cos_b;
    eulerMInv[0][2] = sin_b;
    eulerMInv[1][0] = sin_a * cos_c + cos_a * sin_b * sin_c;
    eulerMInv[1][1] = cos_a * cos_c - sin_a * sin_b * sin_c;
    eulerMInv[1][2] = -cos_b * sin_c;
    eulerMInv[2][0] = sin_a * sin_c - cos_a * sin_b * cos_c;
    eulerMInv[2][1] = cos_a * sin_c + sin_a * sin_b * cos_c;
    eulerMInv[2][2] = cos_b * cos_c;

    return eulerMInv;
  }

  public static final double[][] eulerMatrixInverted(Coordinates angles) {
    return eulerMatrixInverted(angles.x, angles.y, angles.z);
  }

  public Phase getPhaseParent() {    // we retroeve Phase that could be some levels below
    Object aparent = getParent();
    while (aparent != null && !(aparent instanceof Phase))
      aparent = ((XRDcat) aparent).getParent();
    return (Phase) aparent;
  }

  public void notifyParameterChanged(Parameter source) {
    FilePar filepar = getFilePar();
    if ((filepar != null && !filepar.isLoadingFile()) && isAbilitatetoRefresh) {
      for (int i = 0; i < 6; i++) {
        if (parameterField[i] == source) {
//          System.out.println("Constants.FRAGMENT_POSITION_CHANGED");
          notifyParameterChanged(source, Constants.FRAGMENT_POSITION_CHANGED);
          notifyParameterChanged(source, Constants.ATOM_POSITION_CHANGED);
          notifyParameterChanged(source, Constants.STRUCTURE_FACTOR_CHANGED);
          return;
        }
      }
      super.notifyParameterChanged(source);
    }
  }

/*	public int refreshAtoms() {
		int totalNumber = getAtomNumber();
		for (int i = 0; i < getAtomNumber(); i++)
			getAtom(i).refreshPositions();
		for (int i = 0; i < getFragmentNumber(); i++)
			totalNumber += getFragment(i).refreshAtoms();
		return totalNumber;
	}

	public void checkAtomPositions() {
		for (int i = 0; i < getAtomNumber(); i++) {
			getAtom(i).collapsePositions();
		}
		for (int i = 0; i < getFragmentNumber(); i++)
			getFragment(i).checkAtomPositions();
	}

	public void refreshOccupancyAndQuantity() {
		for (int i = 0; i < getAtomNumber(); i++) {
			getAtom(i).refreshPositions();
			getAtom(i).refreshOccupancyAndQuantity();
		}
		for (int i = 0; i < getFragmentNumber(); i++)
			getFragment(i).refreshOccupancyAndQuantity();
	}

	public double[] Fhklcomp(Reflection refl, RadiationType rad, DataFileSet adataset) {
//    double structurefactor = ((StructureFactorModel) getActiveSubordinateModel(structureFactorModelID)).
//            computeStructureFactor(refl, rad, adataset, 0.0);
//    if (structurefactor != 0.0)
//      return structurefactor;
		int h = refl.h, k = refl.k, l = refl.l;
		double dspacing = refl.d_space;
		double[] a1 = new double[2];

		for (int j = 0; j < getAtomNumber(); j++) {
			AtomSite ato = getAtom(j);
			double[] scatf = ato.scatfactor(dspacing, rad);
			double scatFactor = ato.DebyeWaller(h, k, l, dspacing, rad) * ato.getOccupancyValue();
			scatf[0] *= scatFactor;
			scatf[1] *= scatFactor;
			for (int ix = 0; ix < ato.getSiteMultiplicity(); ix++) {
				double[] x = ato.getCoordinates(ix);
				double arg = Constants.PI2 * (h * x[0] + k * x[1] + l * x[2]);
				double w1 = Math.cos(arg);
				double w2 = Math.sin(arg);
				a1[0] += scatf[0] * w1 - scatf[1] * w2;
				a1[1] += scatf[0] * w2 + scatf[1] * w1;
			}
		}
		for (int i = 0; i < getFragmentNumber(); i++) {
			double[] a = getFragment(i).Fhklcomp(refl, rad, adataset);
			a1[0] += a[0];
			a1[1] += a[1];
		}
		return a1;
	}

	public double[] Fhklcomp(int h, int k, int l, double dspacing, RadiationType rad) {
		double[] a1 = new double[2];
		int nAtom = getAtomNumber();
		for (int j = 0; j < nAtom; j++) {
			AtomSite ato = getAtom(j);
			double[] scatf = ato.scatfactor(dspacing, rad);
			double DWfactor = ato.DebyeWaller(h, k, l, dspacing, rad);
			double scatFactor = DWfactor * ato.getQuantityD() / ato.getSiteMultiplicity();
			scatf[0] *= scatFactor;
			scatf[1] *= scatFactor;
			for (int ix = 0; ix < ato.getSiteMultiplicity(); ix++) {
				double[] x = ato.getCoordinates(ix);
				double arg = 2.0 * Constants.PI * (h * x[0] + k * x[1] + l * x[2]);
				double w1 = Math.cos(arg);
				double w2 = Math.sin(arg);
				a1[0] += scatf[0] * w1 - scatf[1] * w2;
				a1[1] += scatf[0] * w2 + scatf[1] * w1;
			}
		}
		for (int i = 0; i < getFragmentNumber(); i++) {
			double[] a = getFragment(i).Fhklcomp(h, k, l, dspacing, rad);
			a1[0] += a[0];
			a1[1] += a[1];
		}
		return a1;
	}

	public double[] getFhklraw(int h, int k, int l, double dspacing, RadiationType rad) {
		double[] a1 = new double[2];
		int nAtom = getAtomNumber();
		if (nAtom <= 0)
			return a1;
		for (int j = 0; j < nAtom; j++) {
			AtomSite ato = getAtom(j);
			double[] scatf = ato.scatfactor(dspacing, rad);
			double DWfactor = ato.DebyeWaller(h, k, l, dspacing, rad);
			double scatFactor = DWfactor * ato.getQuantityD() / ato.getSiteMultiplicity();
			scatf[0] *= scatFactor;
			scatf[1] *= scatFactor;
			a1[0] += scatf[0] * ato.getSiteMultiplicity();
			a1[1] += scatf[1] * ato.getSiteMultiplicity();
		}
		for (int i = 0; i < getFragmentNumber(); i++) {
			double[] a = getFragment(i).getFhklraw(h, k, l, dspacing, rad);
			a1[0] += a[0];
			a1[1] += a[1];
		}
		return a1;
	}

	public double getCellWeigth() {
		double weight = 0.0;
		for (int j = 0; j < getAtomNumber(); j++)
			weight += ((AtomSite) getAtomList().elementAt(j)).getSiteWeight();
		for (int i = 0; i < getFragmentNumber(); i++)
			weight += getFragment(i).getCellWeigth();
		return weight;
	}

	public double getAbsorption(Radiation rad) {
		double absorption = 0.0;
		for (int j = 0; j < getAtomNumber(); j++)
			absorption += getAtom(j).getSiteAbsorption(rad);
		for (int i = 0; i < getFragmentNumber(); i++)
			absorption += getFragment(i).getAbsorption(rad);
		return absorption;
	} */

  public void freeAllCrystalParameters() {
    // Luca: OK
    for (int i = 0; i < 6; i++)
      parameterField[i].setRefinableCheckBound();
    for (int i = 0; i < getFragmentNumber(); i++)
      getFragment(i).freeAllCrystalParameters();
    for (int i = 0; i < this.getAtomNumber(); i++)
      getAtom(i).freeAllCrystalParameters();
  }

/*  public Coordinates getAtomRelativeCoordinates(int na) {
    return (Coordinates) atomRelativeCoordinates.get(na);
  }

  public Coordinates getFragmentRelativeCoordinates(int nf) {
    return (Coordinates) fragmentRelativeCoordinates.get(nf);
  }

  public Coordinates getFragmentRelativeOrientation(int nf) {
    return (Coordinates) fragmentRelativeOrientation.get(nf);
  }*/
}
