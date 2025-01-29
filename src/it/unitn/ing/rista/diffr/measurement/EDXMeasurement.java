package it.unitn.ing.rista.diffr.measurement;

import it.unitn.ing.rista.diffr.*;

import java.awt.*;

/**
 * The EDXMeasurement is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Aug 15, 2024 10:51:47 AM $
 * @since JDK1.1
 */
public class EDXMeasurement extends Measurement {

  public static String modelID = "EDX Measurement";

  public static String[] diclistc = {};
  public static String[] diclistcrm = {};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshEDXMeasurement = true;

  public EDXMeasurement(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

  public EDXMeasurement(XRDcat afile) {
    this(afile, modelID);
  }

  public EDXMeasurement() {
    identifier = modelID;
    IDlabel = modelID;
    description = modelID;
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    System.arraycopy(diclistc, 0, diclist, 0, totsubordinateloop);
    System.arraycopy(diclistcrm, 0, diclistRealMeaning, 0, totsubordinateloop);
    System.arraycopy(classlistcs, 0, classlists, 0, totsubordinate - totparameterloop);
    System.arraycopy(classlistc, 0, classlist, 0, totsubordinateloop - totsubordinate);
  }

  public void initParameters() {
    super.initParameters();

/*
    stringField[0] = "";
    parameterField[0] = new Parameter(this, getParameterString(0), 0,
            ParameterPreferences.getDouble(getParameterString(0) + ".min", -1),
            ParameterPreferences.getDouble(getParameterString(0) + ".max", 1));
*/

  }

  @Override
  public boolean isEDX() {
    return true;
  }

  public void edit(Frame aframe) {
    autoDialog = true;
    super.edit(aframe);
  }

}
