/*
 * @(#)GeometryInclinedImage.java created Feb 13, 2008 Mesiano
 *
 * Copyright (c) 2008 Luca Lutterotti All Rights Reserved.
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
 * The GeometryInclinedImage is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 13, 2008 12:12:05 PM $
 * @since JDK1.1
 */
public class GeometryInclinedImage extends GeometryBraggBrentano {

  public GeometryInclinedImage(XRDcat obj, String alabel) {
    super(obj, alabel);
    identifier = "Reflection flat image";
    IDlabel = "Reflection flat image";
    description = "Reflection flat image instrument geometry";
  }

  public GeometryInclinedImage(XRDcat aobj) {
    this(aobj, "Reflection flat image");
  }

  public GeometryInclinedImage() {
    identifier = "Reflection flat image";
    IDlabel = "Reflection flat image";
    description = "Reflection flat image instrument geometry";
  }

  public double getCorrectedPosition(Sample asample, double x, double[] tilting_angles, DiffrDataFile adatafile) {
    return x;
  }

}
