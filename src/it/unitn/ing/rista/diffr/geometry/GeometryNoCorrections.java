/*
 * @(#)GeometryNoCorrections.java created Apr 23, 2005 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.geometry;

import it.unitn.ing.rista.diffr.*;


/**
 * The GeometryNoCorrections is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2005/05/06 18:10:23 $
 * @since JDK1.1
 */

public class GeometryNoCorrections extends GeometryDiffractometer {

  public GeometryNoCorrections(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "No Corrections";
    IDlabel = "No Corrections";
    description = "No Corrections instrument geometry";
  }

  public GeometryNoCorrections(XRDcat aobj) {
    this(aobj, "No Corrections");
  }

  public GeometryNoCorrections() {
    identifier = "No Corrections";
    IDlabel = "No Corrections";
    description = "No Corrections instrument geometry";
  }

  public double LorentzPolarization(DiffrDataFile adatafile, Sample asample, double position, boolean dspacingbase, boolean energyDispersive) {
    return 1.0;
  }

  public double Lorentz(DiffrDataFile adatafile, double position) {
    return 1.0;
  }

  public double polarization(DiffrDataFile adatafile, double position) {
    return 1.0;
  }

}
