/*
 * @(#)SimpleSite.java created Jul 4, 2011 Caen
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
package it.unitn.ing.rista.util;

import it.unitn.ing.rista.chemistry.AtomInfo;
import it.unitn.ing.rista.diffr.*;

import java.util.Vector;

/**
 * The SimpleSite is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jul 4, 2011 8:05:27 AM $
 * @since JDK1.1
 */
public class SimpleSite {

  public String siteLabel = "";
  public String atomSymbol = "";
  public double[] coord = new double[3];
  public double occupancy = 1;
  public double multeplicity = 1;
  private int atomListNumber = 1;
  private int isotopeListNumber = 1;
  private int oxidationNumber = 0;
  public Vector atomCoordinates;

  public SimpleSite(String label, String symbol, double[] xyz) {
    siteLabel = label;
    atomSymbol = symbol;
    for (int i = 0; i < 3; i++)
      coord[i] = xyz[i];
    loadAtomProperties();
  }

  public SimpleSite(String label, String symbol, double[] xyz, double occ, double mult) {
    this(label, symbol, xyz);
    occupancy = occ;
    multeplicity = mult;
  }

  public void loadAtomProperties() {
//      atomNumber = AtomInfo.retrieveAtomNumber(stripIsotopeNumber(atomSymbol));
      atomListNumber = AtomInfo.getAtomNumber(AtomSite.stripIsotopeNumber(atomSymbol));
      isotopeListNumber = AtomInfo.getIsotopeNumber(AtomSite.stripOxidation(atomSymbol));
      oxidationNumber = AtomSite.retrieveOxidationNumber(atomSymbol);
    System.out.println("AtomSite: " + siteLabel);
    System.out.println("AtomSite: " + atomSymbol);
    System.out.println("AtomSite list number " + atomListNumber);
    System.out.println("Isotope list number " + isotopeListNumber);
    System.out.println("Oxidation list number " + oxidationNumber);
//      weight = AtomInfo.retrieveAtomWeight(stripIsotopeNumber(atomSymbol));
//      radius = Math.abs(AtomInfo.retrieveAtomRadius(stripIsotopeNumber(atomSymbol)));
      //		System.out.println(magneticSF[0]);
  }

/*  public double[] scatfactor(double dspacing, int radType, int tubeNumber) {
    return AtomSite.scatfactor(dspacing, radType, tubeNumber,
        atomListNumber, isotopeListNumber, oxidationNumber);
  }*/

  public void computePositions(Phase aphase) {
    atomCoordinates = new Vector(10, 10);
    SitePosition sitepos;
    for (int i = 0; i < aphase.getPhaseInfo().getSitePositionNumber(); i++) {
      sitepos = aphase.getPhaseInfo().getSitePosition(i);
      double x[] = new double[3];
      for (int j = 0; j < 3; j++)
        x[j] = sitepos.getcoord(j, coord);
      boolean flagx = true;
      for (int kx = 0; kx < atomCoordinates.size(); kx++) {
        boolean flagx1 = true;
        double[] xyz = (double[]) atomCoordinates.elementAt(kx);
        double diff1 = Math.abs(x[0] - xyz[0]);
        double diff2 = Math.abs(x[1] - xyz[1]);
        double diff3 = Math.abs(x[2] - xyz[2]);
        if (diff1 > Constants.TOOLERANCE_COORD && diff1 < 1.0 - Constants.TOOLERANCE_COORD)
          flagx1 = false;
        if (diff2 > Constants.TOOLERANCE_COORD && diff2 < 1.0 - Constants.TOOLERANCE_COORD)
          flagx1 = false;
        if (diff3 > Constants.TOOLERANCE_COORD && diff3 < 1.0 - Constants.TOOLERANCE_COORD)
          flagx1 = false;
        if (flagx1)
          flagx = false;
      }
      if (flagx)
        atomCoordinates.addElement(x);
    }


  }

  public int getSiteMultiplicity() {
    return atomCoordinates.size();
  }

  public double[] getCoordinates(int ix) {
    return (double[]) atomCoordinates.get(ix);
  }

}
