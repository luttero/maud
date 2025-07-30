package it.unitn.ing.rista.diffr.cal;

import it.unitn.ing.rista.comp.GeneticAlgorithmAltRefinement;
import it.unitn.ing.rista.diffr.*;

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.diffr.radiation.XrayEbelTubeRadiation;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 *  The RadiationIntensityCalibration is a class
 *
 *
 * @version $Revision: 1.0 $, $Date: 2024/08/16 14:11:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class RadiationIntensityCalibration extends IntensityCalibration {
  public static String[] diclistc = {};
  public static String[] diclistcrm = {};

  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  boolean refreshCalibration = true;

  double energyGap = 1000;

  Vector<EnergyIntensity> calIntensity = new Vector<>(0, 100);
  public RadiationIntensityCalibration(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "Radiation Distribution";
    IDlabel = "Radiation Distribution";
  }

  public RadiationIntensityCalibration(XRDcat aobj) {
    this(aobj, "Radiation distribution");
  }

  public RadiationIntensityCalibration() {
    identifier = "Radiation Distribution";
    IDlabel = "Radiation Distribution";
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

  public void computeAllPost() {
    // to be implemented in subclasses
    calIntensity.removeAllElements();
    RadiationType radType = getRadiation();
    energyGap = 1000;
    if (radType instanceof XrayEbelTubeRadiation) {
      energyGap = ((XrayEbelTubeRadiation) radType).getEnergyStep();
    }
    int lineCounts = radType.getLinesCount();
    for (int i = 0; i < lineCounts; i++) {
      double coordinate = radType.getRadiationEnergyForFluorescence(i) * 1000;
      double intensity = radType.getRadiationWeightForFluorescence(i);
      EnergyIntensity eni = new EnergyIntensity(coordinate, intensity);
      calIntensity.addElement(eni);
    }
    calIntensity.sort(new IncreasingEnergy());
  }

  public double calibrateData(DiffrDataFile datafile, double x, int index, double coord) {
    if (calIntensity.isEmpty()) {
      computeAllPost();
    }

    return 0;
  }

/*  public double getInterpolatedValueAt(double x) {
      if (x <= getXData(startingindex))
        return getFit(startingindex);
      if (x >= getXData(finalindex - 1))
        return getFit(finalindex - 1);
      int index = getOldNearestPoint(x);
      if ((x <= getXData(index) && index > startingindex) || index > finalindex - 2)
        return MoreMath.getLinearInterpolation(x, getXData(index - 1), getXData(index),
            getFit(index - 1), getFit(index));
      else
        return MoreMath.getLinearInterpolation(x, getXData(index), getXData(index + 1),
            getFit(index), getFit(index + 1));
  }*/

  private class EnergyIntensity {
    public double energy = 0;
    public double intensity = 0;

    public EnergyIntensity(double en, double inten) {
      energy = en;
      intensity = inten;
    }
  }

  class IncreasingEnergy implements Comparator {
    public int compare(Object obj1, Object obj2) {
      double en1 = ((EnergyIntensity) obj1).energy;
      double en2 = ((EnergyIntensity) obj2).energy;

      if (en1 == en2) {
        return 0;
      } else if (en1 < en2)
        return -1;
      return 1;
    }
  }

}
