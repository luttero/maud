/*
 * @(#)Parameter.java created 01/01/1997 Mesiano
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

import java.util.*;

import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.io.*;

/**
 * The Parameter is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.22 $, $Date: 2006/11/10 09:33:00 $
 * @since JDK1.1
 */

public class Parameter extends Object implements Cloneable, basicObj {
  // it must be impossible to change these from outside
  private String value = "0";
  private String valueMin = "0";
  private String valueMax = "0";
  private String tempBound = null;
  private double minimumSignificantValue = 0.0;
  private boolean positiveDefinite = false;
  private static boolean checkPositive = false;
  public boolean formattingRequired = true;

  protected String thelabel;
	public String refName = null;
  protected boolean free;
  protected boolean rangeActive;
  protected String ratio;
  protected String constant;
  protected String error;
  protected Parameter refParameter1;
  protected Vector<Parameter> registeredParameters;
  XRDcat theparent;
  protected boolean automaticGenerated = false;
  boolean refreshMinMax = true;
  public boolean changeMinMax = true;
  public boolean automaticOutput = false;

  public static final int MANUAL_FIXED = 0;
  public static final int MANUAL_FREE = 1;
  public static final int MANUAL_BOUND = 2;
  public static final int AUTOMATIC_BOUND = 3;

  public JTextField editingField = null;

  public static boolean doRefresh = true;
  public static boolean doInterfaceRefresh = true;

	private String tempExpression = null;
	private MathExpression boundEquation = null;
	private Vector<MathExpression> registeredMathExpressions = new Vector<MathExpression>();

  public Parameter(XRDcat parent) {
    super();
    setParent(parent);
    initParameter();
  }

  public Parameter(XRDcat parent, double avalue) {
    this(parent, Fmt.format(avalue));
  }

  public Parameter(XRDcat parent, double avalue, double aerror) {
    this(parent, Fmt.format(avalue), Fmt.format(aerror));
  }

  public Parameter(XRDcat parent, double avalue, double aerror, boolean isfree) {
    this(parent, Fmt.format(avalue), Fmt.format(aerror), isfree);
  }

  public Parameter(XRDcat parent, String alabel, double avalue, double aerror, boolean isfree) {
    this(parent, alabel, Fmt.format(avalue), Fmt.format(aerror), isfree);
  }

  public Parameter(XRDcat parent, String alabel, double avalue, double aerror, double minv, double maxv, boolean isfree,
                   String arefName, String refBound, String constant, String ratio, String expression,
                   boolean autoTrace, boolean positive) {
    this(parent, alabel, Fmt.format(avalue), Fmt.format(aerror), Fmt.format(minv), Fmt.format(maxv), isfree,
        arefName, refBound, constant, ratio, expression, autoTrace, positive);
  }

  public Parameter(XRDcat parent, String alabel, double avalue, double aerror) {
    this(parent, alabel, Fmt.format(avalue), Fmt.format(aerror));
  }

  public Parameter(XRDcat parent, String alabel, double avalue, double min, double max) {
    this(parent, alabel, Fmt.format(avalue), "0", Fmt.format(min), Fmt.format(max), false,
		    null, null, null, null, null, false, false);
  }

  public Parameter(XRDcat parent, String alabel, double avalue, double min, double max, boolean positive,
                   double minSignificantValue) {
    this(parent, alabel, Fmt.format(avalue), "0", Fmt.format(min), Fmt.format(max), false, null, null, null, null, null,
		    false, positive, minSignificantValue);
  }

  public Parameter(XRDcat parent, String alabel, double avalue) {
    this(parent, alabel, Fmt.format(avalue), new String("0"));
  }

  public Parameter(XRDcat parent, String avalue) {
    this(parent);
    if (avalue != null && avalue.length() > 12)
      avalue = Fmt.format(Double.valueOf(avalue).doubleValue());
    setValue(avalue);
  }

  public Parameter(XRDcat parent, String avalue, String aerror) {
    this(parent, avalue);
    if (aerror != null && aerror.length() > 12)
      aerror = Fmt.format(Double.valueOf(aerror).doubleValue());
    setError(aerror);
  }

  public Parameter(XRDcat parent, String avalue, String aerror, boolean isfree) {
    this(parent, avalue, aerror);
    setFree(isfree);
  }

  public Parameter(XRDcat parent, String alabel, String avalue, String aerror, boolean isfree) {
    this(parent, alabel, avalue, aerror);
    setFree(isfree);
  }

  public Parameter(XRDcat parent, String alabel, String avalue, String aerror, String minv, String maxv, boolean isfree) {
    this(parent, alabel, avalue, aerror, minv, maxv, isfree, null, null, null, null, null, false, false);
  }

  public Parameter(XRDcat parent, String alabel, String avalue, String aerror, String minv, String maxv, boolean isfree,
                   String arefName, String refBound, String constant, String ratio, String expression,
                   boolean autoTrace, boolean positive) {
    this(parent, alabel, avalue, aerror);
    setValueMin(minv);
    setValueMax(maxv);
    setFree(isfree);
    BoundTracker boundList = null;
    if ((boundList = parent.getFilePar().getBoundTracker()) != null) {
      if (arefName != null) {
	      refName = arefName;
        boundList.addReferenceParameter(arefName, this);
//        System.out.println("Add ref: " + refName);
      }
      if (refBound != null) {
        setConstant(constant);
        setRatio(ratio);
        setTempBound(refBound);
      }
	    if (expression != null) {
		    setTempExpression(expression);
	    }
    }
    setAutomaticOutput(autoTrace);
    if (positive)
      setPositiveOnly();
  }

  public Parameter(XRDcat parent, String alabel, String avalue, String aerror, String minv, String maxv, boolean isfree,
                   String arefName, String refBound, String constant, String ratio, String expression,
                   boolean autoTrace, boolean positive, double minimumValue) {
    this(parent, alabel, avalue, aerror, minv, maxv, isfree,
                   arefName, refBound, constant, ratio, expression, autoTrace, positive);
    minimumSignificantValue = minimumValue;
  }

  public Parameter(XRDcat parent, String alabel, String avalue, String aerror) {
    this(parent, avalue, aerror);
    setLabel(alabel);
  }

  protected void initParameter() {
    setValue("0");
    setValueMin("0");
    setValueMax("0");
    setLabel("unknown");
    setFree(false);
    setRatio("1");
    setConstant("0");
    setError("0");
    refParameter1 = null;
//    referenceParameters = new Vector(0, 1);
    registeredParameters = new Vector(0, 1);
  }

  public void setPositiveOnly() {
    positiveDefinite = true;
  }

  public void setMinimumSignificantValue(double value) {
    minimumSignificantValue = value;
  }

  public double getMinimumSignificantValue() {
    return minimumSignificantValue;
  }

  public basicObj[] getChildren(String searchString, boolean refinable) {
    return null;
  }

  public Object[] getObjectChildren() {
    return null;
  }

  public XRDcat getParent() {
    return theparent;
  }

  public void setParent(XRDcat obj) {
    theparent = obj;
  }

  public boolean isLeaf() {
    return true;
  }

  public boolean getAllowsChildren() {
    return false;
  }

  public int getChildCount(String searchString, boolean refinable) {
    return 0;
  }

  public basicObj getChildAt(int childIndex) {
    return null;
  }

  public Enumeration children() {
    return null;
  }

  public int getIndex(basicObj node) {
    return -1;
  }

  public boolean isObjectSupported(basicObj source, ListVector list) {
    return false;
  }

  public boolean isObjectSupported(ListVector list) {
    return false;
  }

  public String toIDString() {
    Object obj = getParent();
    String astring = new String("");
    if (obj != null && obj instanceof XRDcat)
      astring = new String(((XRDcat) obj).toIDString());
    if (!astring.equals(""))
      return new String(astring + ":" + thelabel);
    else
      return new String(thelabel);
  }

/*	public boolean equals(Object obj)
	{
		// to be completed
		Parameter par = (Parameter) obj;
		if (!getValue().equals(par.getValue()))
			return false;
	}	*/

  public Object clone() {
//	System.out.println("cloning: " + this);
    try {
      Parameter theclone = (Parameter) super.clone();
      theclone.setRefparameter(theclone.getRefparameter());
      return theclone;
    } catch (CloneNotSupportedException e) {
      return null;
    }
  }

  public Parameter getCopy(XRDcat parent) {
    Parameter thecopy = new Parameter(parent);
    thecopy.value = value.toString();
    thecopy.valueMin = valueMin.toString();
    thecopy.valueMax = valueMax.toString();
//    thecopy.tempBound = tempBound.toXRDcatString();
    thecopy.thelabel = thelabel.toString();
    thecopy.free = free;
    thecopy.rangeActive = rangeActive;
    thecopy.ratio = ratio.toString();
    thecopy.constant = constant.toString();
    thecopy.error = error.toString();
    if (refParameter1 != null)
      thecopy.setRefparameter(refParameter1);
    thecopy.automaticGenerated = automaticGenerated;
    thecopy.refreshMinMax = refreshMinMax;
    thecopy.changeMinMax = changeMinMax;
    thecopy.automaticOutput = automaticOutput;
    thecopy.positiveDefinite = positiveDefinite;
    thecopy.minimumSignificantValue = minimumSignificantValue;
    thecopy.setParent(parent);
    return thecopy;
  }

  public void merge(basicObj obj) {
  }

  public void setValue(String avalue) {
	  double dvalue = Double.parseDouble(avalue);
    if (Double.parseDouble(value) != dvalue) {
      if (positiveDefinite && dvalue < 0.0) { // && checkPositive) {
        double val = Math.abs(dvalue);
        avalue = getStringValue(val);
      }
      value = avalue;
      refreshMinMax = true;
      if (doInterfaceRefresh) {
        if (editingField != null && !editingField.getText().equals(avalue))
          editingField.setText(avalue);
      }
      if (doRefresh) {
        XRDcat obj = getParent();
        if (obj != null)
          obj.notifyParameterChanged(this);
        if (registeredParameters != null)
          for (int i = 0; i < registeredParameters.size(); i++)
            (registeredParameters.elementAt(i)).updateValue();
      }
    }
  }

  public boolean isReference() {
    if (registeredParameters == null || registeredParameters.size() <= 0)
      return false;
    return true;
  }

  public void updateValue() {
    if (getRefparameter() != null)
      notifyChangedValue();
  }

  public void setValue(double avalue) {
    setValue(getStringValue(avalue));
  }

  public String getStringValue(double value) {
    if (formattingRequired)
      return Fmt.format(value);
    return Double.toString(value);
  }

  public void setValueNoFormat(double avalue) {
    setValue(Double.toString(avalue));
  }

  public String getValue() {
    return value;
  }

  public String getValueForCOD() {
    updateValue();
    if (!getFree() || Double.parseDouble(error) == 0.0)
      return getValue();
    double valueV = Double.parseDouble(value);
    int index = getErrorExponent();
    int total = index;
    while (index != 0) {
      if (index > 0) {
        valueV *= 10;
        index--;
      } else {
        valueV /= 10;
        index++;
      }
    }
    valueV = ((long) (valueV + 0.4999999999)) / Math.pow(10, total);
    return Double.toString(valueV);
  }

  public String getErrorForCOD() {
    double errorV = Double.parseDouble(error);
    if (errorV == 0.0)
      return "0";
    double previousComp = 0.95;
    double nextComp = 9.5;
    while (/*errorV >= nextComp &&*/ errorV < previousComp) {
      if (errorV >= nextComp) {
        errorV /= 10.0;
      } else {
        errorV *= 10.0;
      }
    }
    int errorI = (int) (errorV + 0.499999999);
    return Integer.toString(errorI);
  }

  public int getErrorExponent() {
    double errorV = Double.parseDouble(error);
    if (errorV == 0.0)
      return 0;
    int i = 0;
    double previousComp = 0.95;
    double nextComp = 9.5;
    while (/*errorV >= nextComp && */ errorV < previousComp) {
      if (errorV >= nextComp) {
        i--;
        errorV /= 10.0;
      } else {
        i++;
        errorV *= 10.0;
      }
    }
    return i;
  }

  public double getValueD() {
    updateValue();
    return Double.parseDouble(value);
  }

  public void setError(String aerror) {
    if (aerror != null && !aerror.equals("NaN") && !aerror.startsWith("inf"))
      error = new String(aerror);
    else
      error = new String("0");
  }

  public void setError(double avalue) {
    if (Double.toString(avalue) != null && !Double.isNaN(avalue) &&
        !Double.isInfinite(avalue))
      setError(Fmt.format(avalue));
    else
      error = new String("0");
  }

  public String getError() {
    return error;
  }

  public void setLabel(String avalue) {
    thelabel = new String(avalue);
  }

  public String getLabel() {
    return toString();
  }

  public String toString() {
    return thelabel;
  }

  public void setFree(boolean isfree) {
    free = isfree;
  }

  public boolean getFree() {
    return free;
  }

  public boolean mayRefines() {
    boolean boundParMayRefine = false;
    if (registeredParameters != null) {
      for (int i = 0; i < registeredParameters.size(); i++)
        if (((Parameter) registeredParameters.elementAt(i)).mayRefines())
          boundParMayRefine = true;
    }

    return (getParent().mayRefines(this) || boundParMayRefine);
  }


	// Equal to section

	protected void setTempBound(String refBound) {
		tempBound = refBound;
//    System.out.println("Set temp bound: " + refBound + " " + constant + " " + ratio);
	}

	public void checkTempBound(BoundTracker boundList) {
		if (tempBound == null)
			return;
//    System.out.println("Check bound: " + tempBound + " " + constant + " " + ratio);
		Parameter refPar = boundList.getReferenceParameter(tempBound);
		setEqualTo(refPar, ratio, constant);
		tempBound = null;
	}

	public void setRatio(String avalue) {
		ratio = avalue;
	}

	public String getRatio() {
		return ratio;
	}

	public void setConstant(String avalue) {
		constant = avalue;
	}

	public String getConstant() {
		return constant;
	}

	private void notifyChangedValue() {
		double newvalue = Double.parseDouble(getRefparameter().getValue()); // we need to avoid the update method
		double aratio = Double.parseDouble(getRatio());
		double aconstant = Double.parseDouble(getConstant());
		double newvalued = newvalue * aratio + aconstant;
		if (!(positiveDefinite && newvalued < 0.0))
			setValue(newvalue * aratio + aconstant);
	}

	public void setEqualTo(Parameter par, String ratiopar, String constantpar) {
    setRatio(ratiopar);
    setConstant(constantpar);
    setRefparameter(par);
    updateValue();
  }

  public void setEqualTo(Parameter par, double ratiopar, double constantpar) {
    setRatio(new String(Fmt.format(ratiopar)));
    setConstant(new String(Fmt.format(constantpar)));
    setRefparameter(par);
    updateValue();
  }

  public Parameter getRefparameter() {
    return refParameter1;
  }

  private void setRefparameter(Parameter apar) {
    if (getRefparameter() != null)
	  getRefparameter().unregister(this);
    refParameter1 = apar;
    if (getRefparameter() != null) {
      getRefparameter().register(this);
      setFree(false);
    }
  }

  public void setRefparameterAutomatic(Parameter apar) {
    if (apar != null && getStatusIndex() != MANUAL_BOUND) {
	    setRatio("1");
      setConstant("0");
      setRefparameter(apar);
	    automaticGenerated = true;
    }
  }

  public void setEqualToAutomatic(Parameter apar, String ratiopar, String constantpar) {
    if (getStatusIndex() != MANUAL_BOUND) {
      setRatio(ratiopar);
      setConstant(constantpar);
      setRefparameter(apar);
      if (apar != null)
        automaticGenerated = true;
      updateValue();
    }
  }

  public void resetAutomaticBound() {
    if (automaticGenerated)
      resetParameterBound();
  }

  public void resetParameterBound() {
    setRefparameter(null);
    automaticGenerated = false;
  }

  public int getStatusIndex() {
    if (getRefparameter() != null)
      if (automaticGenerated)
        return AUTOMATIC_BOUND;
      else
        return MANUAL_BOUND;
    else if (getFree())
      return MANUAL_FREE;
    return MANUAL_FIXED;
  }

  public int getReducedStatusIndex() {
    if (getRefparameter() != null)
      return MANUAL_BOUND;
    else if (getFree())
      return MANUAL_FREE;
    return MANUAL_FIXED;
  }

  public void unregister(Parameter par) {
    registeredParameters.removeElement(par);
  }

  public void register(Parameter par) {
    registeredParameters.addElement(par);
  }

// MathExpression section

	protected void setTempExpression(String expression) {
		tempExpression = expression;
//    System.out.println("Set temp bound: " + refBound + " " + constant + " " + ratio);
	}

	public void checkTempExpression(BoundTracker boundList) {
		if (tempExpression == null)
			return;
//    System.out.println("Check bound: " + tempBound + " " + constant + " " + ratio);
		Parameter[] refPar = boundList.getReferenceParameters(tempExpression);
		setBoundWith(refPar, tempExpression);
		tempExpression = null;
	}

	public void setBoundWith(Parameter[] pars, String expression) {
		boundEquation = new MathExpression(this, expression, pars);
		updateValue();
	}

	public void unregister(MathExpression exp) {
		registeredMathExpressions.removeElement(exp);
	}

	public void register(MathExpression exp) {
		registeredMathExpressions.addElement(exp);
	}


	// refinable section

  public void setRefinable() {
    setFree(true);
    resetParameterBound();
  }

  public void setRefinableCheckBound() {
    if (getStatusIndex() != MANUAL_BOUND) {
      setFree(true);
      resetParameterBound();
    }
  }

  public void setNotRefinable() {
    setFree(false);
    resetParameterBound();
  }

  public void setNotRefinableCheckBound() {
    if (getStatusIndex() != MANUAL_BOUND) {
      setFree(false);
      resetParameterBound();
    }
  }

  public void freeAllParameters(String searchString, boolean refinable) {
    setRefinable();
  }

  public void fixAllParameters(String searchString, boolean refinable) {
    setNotRefinable();
  }

  boolean disposed = false;

  public void dispose() {
    editingField = null;
    if (theparent.getFilePar().getBoundTracker() != null)
      theparent.getFilePar().getBoundTracker().removeReferenceParameter(this);
    resetParameterBound();
    if (registeredParameters != null) {
//	    Iterator<Parameter> iter = registeredParameters.iterator();
//	    while (iter.hasNext())
//		    iter.next().setNotRefinable();  // why? To check
      registeredParameters.removeAllElements();
    }
	  if (boundEquation != null)
		  boundEquation.dispose();
	  if (registeredMathExpressions != null) {
		  Iterator<MathExpression> iter = registeredMathExpressions.iterator();
		  while (iter.hasNext())
			  iter.next().dispose();
		  registeredMathExpressions.removeAllElements();
	  }
    disposed = true;
  }

/*  protected void finalize() throws Throwable {
    if (!disposed)
      dispose();
    registeredParameters = null;
	  registeredMathExpressions = null;
	  boundEquation = null;
    value = null;
    thelabel = null;
    ratio = null;
    constant = null;
    error = null;
    super.finalize();
  }*/

  public void setComponent(Component edField) {
    editingField = (JTextField) edField;
  }

  public String getValueMin() {
    checkMinMax();
    return valueMin;
  }

  public double getValueMinD() {
    return Double.parseDouble(getValueMin());
  }

  public void setValueMin(String valueMin) {
    refreshMinMax = true;
    if (valueMin != null)
      this.valueMin = valueMin;
  }

  public void setValueMin(double avalue) {
    setValueMin(Fmt.format(avalue));
  }

  public String getValueMax() {
    checkMinMax();
    return valueMax;
  }

  public double getValueMaxD() {
    return Double.parseDouble(getValueMax());
  }

  public void setValueMax(String valueMax) {
    refreshMinMax = true;
    if (valueMax != null)
      this.valueMax = valueMax;
  }

  public void setValueMax(double avalue) {
    setValueMax(Fmt.format(avalue));
  }

  public void checkMinMax() {
    if (!refreshMinMax)
      return;
    refreshMinMax = false;

    String cifID = getParent().getCifID(this);
    if (cifID.startsWith("_atom_site_fract")) {
      changeMinMax = false;
    }

    double value = Double.parseDouble(getValue());
    double min = Double.parseDouble(getValueMin());
    double max = Double.parseDouble(getValueMax());
    boolean equal = (min == max);
    if ((min <= value && value <= max) && !equal)
      return;
    if (cifID.startsWith("_atom_site_fract")) {
      setValueMin(ParameterPreferences.getDouble(cifID + ".min", 0.0));
      setValueMax(ParameterPreferences.getDouble(cifID + ".max", 1.0));
      return;
    }
    if (min > value || equal) {
      min = value - 10.0 * Math.abs(value);
      setValueMin(ParameterPreferences.getDouble(cifID + ".min", min));
    }
    if (max < value || equal) {
      max = value + 10.0 * Math.abs(value);
      setValueMax(ParameterPreferences.getDouble(cifID + ".max", max));
    }
  }

  public void enlargeMinMax() {
    if (!changeMinMax)
      return;
    double value = Double.parseDouble(getValue());
    double min = Double.parseDouble(getValueMin());
    double max = Double.parseDouble(getValueMax());

    double range = value - min;
    range *= 1.5;
    min = value - range;
    range = max - value;
    range *= 1.5;
    max = range + value;
    setValueMin(min);
    setValueMax(max);
  }

  public void shrinkMinMax() {
    if (!changeMinMax)
      return;
    double value = Double.parseDouble(getValue());
    double min = Double.parseDouble(getValueMin());
    double max = Double.parseDouble(getValueMax());

    double range = value - min;
    range *= 0.7;
    min = value - range;
    range = max - value;
    range *= 0.7;
    max = range + value;
    setValueMin(min);
    setValueMax(max);
  }

  public void centerMinMax() {
    if (!changeMinMax || !getFree())
      return;
    double value = Double.parseDouble(getValue());
    double min = Double.parseDouble(getValueMin());
    double max = Double.parseDouble(getValueMax());

    double range = max - min;
    range /= 2.0;
    min = value - range;
    max = range + value;
    setValueMin(min);
    setValueMax(max);
  }

  public boolean isRangeActive() {
    return rangeActive;
  }

  public void setRangeActive(boolean rangeActive) {
    this.rangeActive = rangeActive;
  }

  public void printInformations(OutputStream out, XRDcat obj) {
    try {
	    obj.printString(out, "- Parameter:  " + toIDString() + " ");
	    obj.printString(out, " Value: " + value + ", minimum: " + valueMin + ", maximum: " + valueMax);
      if (free) {
        if (Double.parseDouble(error) >= 0)
	        obj.printString(out, " Status: refinable, " + "error: +-" + error);
        else
	        obj.printString(out, " Status: refinable, " + "Cholesky negative diagonal on this parameter, must be fixed");
      } else if (getRefparameter() != null) {
	      obj.printString(out, " Status: equal to " + constant + " + " + ratio + " * " + getRefparameter().toIDString());
      } else {
	      obj.printString(out, " Status: not refinable");
      }
      if (positiveDefinite)
	      obj.printString(out, ", only positive values permitted");
	    obj.printString(out, ", minimum significant value: " + minimumSignificantValue);
	    obj.newLine(out);
      if (registeredParameters != null && registeredParameters.size() > 0) {
	      obj.printLine(out, "       Parameters bounded to this parameter:");
        for (int i = 0; i < registeredParameters.size(); i++) {
	        obj.printLine(out, "          " + ((Parameter) registeredParameters.elementAt(i)).toIDString());
        }
	      obj.newLine(out);
      }
    } catch (IOException io) {
      io.printStackTrace();
    }
  }

  public boolean automaticOutput() {
    return automaticOutput;
  }

  public void setAutomaticOutput(boolean status) {
    automaticOutput = status;
  }

  public void writeParameter(BufferedWriter out, String dicterm, BoundTracker boundList) {
    try {
      out.write(dicterm);
      out.write(" ");
      out.write(getValue());
      if (getFree())
        out.write("(" + getError() + ")");
      if (automaticOutput)
        out.write(" #autotrace");
      if (positiveDefinite)
        out.write(" #positive");
      if (boundList != null) {
        out.write(" #min " + getValueMin());
        out.write(" #max " + getValueMax());
        if (isReference()) {
          out.write(" " + boundList.getReferenceName(this));
        }
        if (getRefparameter() != null) {
          // is bounded
          out.write(" " + "#equalTo " + getConstant() + " + " + getRatio() + " * " +
              boundList.getReferenceName(getRefparameter()));
        } else if (boundEquation != null)
		      boundEquation.writeExpression(out);
      }
      out.newLine();
    } catch (IOException ioe) {
      System.out.println("Error in writing the parameter " + this);
    }
  }

/* not used
  public void setRegisteredParameters(Vector avector) {
    registeredParameters = (Vector) avector.clone();
  }

  public void getTextFieldFrom(Parameter apar) {
    if (editingField != null)
      editingField = apar.editingField;
  }

   public void moveFrom(Parameter parameter) {
    setValueMin(parameter.getValueMin());
    setValueMax(parameter.getValueMax());
//    setLabel(parameter.getLabel());
    setFree(parameter.getFree());
    setRatio(parameter.getRatio());
    setConstant(parameter.getConstant());
    setError(parameter.getError());
    setAutomaticOutput(parameter.automaticOutput());
    setRefparameter(parameter.getRefparameter());
    parameter.setRefparameter(null);
    while (registeredParameters.size() > 0) {
      Parameter refpar = (Parameter) registeredParameters.elementAt(0);
      refpar.setRefparameter(null);
    }
    while (parameter.registeredParameters.size() > 0) {
      Parameter refpar = (Parameter) parameter.registeredParameters.elementAt(0);
      refpar.setRefparameter(this);
    }
    setValue(parameter.getValue());
  }
   */
}
