/*
 * @(#)GeometryTransmissionFlatImageNoLorentz.java created Dec 10, 2006 Berkeley
 *
 * Copyright (c) 1996-2006 Luca Lutterotti All Rights Reserved.
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
 * The GeometryTransmissionFlatImageNoLorentz is a class
 *
 * @version $Revision: 1.0 $, $Date: 2005/09/07 17:14:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryTransmissionFlatImageNoLorentz extends GeometryTransmissionFlatImage {

  public GeometryTransmissionFlatImageNoLorentz(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = "Lorentz corrected transmission image";
    IDlabel = "Lorentz corrected transmission image";
    description = "Image 2D instrument geometry Lorentz corrected";
  }

  public GeometryTransmissionFlatImageNoLorentz(XRDcat aobj) {
    this(aobj, "Lorentz corrected transmission image");
  }

  public GeometryTransmissionFlatImageNoLorentz() {
    identifier = "Lorentz corrected transmission image";
    IDlabel = "Lorentz corrected transmission image";
    description = "Image 2D instrument geometry Lorentz corrected";
  }

  public double Lorentz(DiffrDataFile adatafile, double position) {
    double sintheta, costheta;
    sintheta = Math.sin(position);
    costheta = Math.cos(position);
    return 1.0 / (costheta * sintheta * sintheta);
//    double cos2theta = Math.cos(2.0 * position);
//    return 0.5 / cos2theta + 0.5 * cos2theta;
  }

}
