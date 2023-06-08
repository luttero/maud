/*
 * @(#)IntensityExtnone.java created 19/08/1998 Pergine Vals.
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.intext;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;

/**
 *  The IntensityExtnone is a class
 *
 *
 * @version $Revision: 1.5 $, $Date: 2005/05/06 18:07:26 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class IntensityExtnone extends IntensityExtractor {

  public static String[] diclistc = {};
  public static String[] diclistcrm = {};
  public static String[] classlistc = {};
  public static String[] classlistcs = {};

  public IntensityExtnone(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initBaseObject();
    identifier = "none ie";
    IDlabel = "none ie";
    description = "select this to disable intensity extraction";
  }

  public IntensityExtnone(XRDcat aobj) {
    this(aobj, "disable");
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

  public IntensityExtnone() {
    identifier = "none ie";
    IDlabel = "none ie";
    description = "select this to disable intensity extraction";

  }

}
