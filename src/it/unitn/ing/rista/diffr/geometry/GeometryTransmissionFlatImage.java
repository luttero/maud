/*
 * @(#)GeometryTransmissionFlatImage.java created Mar 13, 2003 Berkeley
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

package it.unitn.ing.rista.diffr.geometry;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Constants;


/**
 * The GeometryTransmissionFlatImage is a class
 *  
 * @version $Revision: 1.6 $, $Date: 2005/09/07 17:14:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeometryTransmissionFlatImage extends GeometryDebyeScherrer {

	public static String modelID = "Image 2D";

  public GeometryTransmissionFlatImage(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = modelID;
    IDlabel = modelID;
    description = "Image 2D instrument geometry";
  }

  public GeometryTransmissionFlatImage(XRDcat aobj) {
    this(aobj, "Image 2D");
  }

  public GeometryTransmissionFlatImage() {
    identifier = modelID;
    IDlabel = modelID;
    description = "Image 2D instrument geometry";
  }

  public double Lorentz(DiffrDataFile adatafile, double position) {
    double sintheta, costheta, cos2theta;
    // from P. Norby, J. Appl. Cryst. (1997), 30, 21-30.
    sintheta = Math.sin(position);
    costheta = Math.cos(position);
    cos2theta = Math.cos(2.0 * position);
//    cos2theta *= cos2theta;
    return cos2theta / (costheta * sintheta * sintheta);
  }

}
