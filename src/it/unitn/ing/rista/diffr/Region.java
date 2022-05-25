/*
 * @(#)Region.java created 01/01/1997 Mesiano
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


/**
 * The Region is a class
 *
 * @version $Revision: 1.4 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Region extends XRDcat {
  protected static String[] diclistc = {"_riet_excl_2theta_range_min", "_riet_excl_2theta_range_max"};
  protected static String[] diclistcrm = {"_riet_excl_2theta_range_min", "_riet_excl_2theta_range_max"};

  protected static String[] classlistc = {};

  double minimum = 0.0, maximum = 0.0;

  public Region(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
  }

  public Region(XRDcat afile) {
    this(afile, "Region x");
  }

	public Region() {}

  public void initConstant() {
    Nstring = 2;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  public void initParameters() {
    super.initParameters();
    stringField[0] = "0.0";
    stringField[1] = "0.0";
  }

  public void updateStringtoDoubleBuffering(boolean firstLoading) {
    minimum = Double.valueOf(stringField[0]).doubleValue();
    maximum = Double.valueOf(stringField[1]).doubleValue();
  }

  public double getMinimum() {
    return minimum;
  }

  public double getMaximum() {
    return maximum;
  }
}
