/*
 * @(#)StructureFactor.java created 19/08/2001 Riva Del Garda
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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
 * The StructureFactor is a class to store structure factor information
 *
 * @version $Revision: 1.3 $, $Date: 2005/09/07 17:14:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class StructureFactor {
  public int h = 0;
  public int k = 0;
  public int l = 0;
  public double d_spacing = 0.0;
  public double Fhkl_exp = 0.0;
  public double Fhkl_calc = 0.0;
  public double Fhkl_esd = 0.0;
  public double phase_exp = -999.9;
  public double phase_calc = -999.9;
  public int[] reflectionhList = null;
  public int[] reflectionkList = null;
  public int[] reflectionlList = null;
  public double weight = 1.0;

  public StructureFactor(int ht, int kt, int lt, double d_space,
                         double expSF, double SF, double esdSF) {
    h = ht;
    k = kt;
    l = lt;
    d_spacing = d_space;
    Fhkl_exp = expSF;
    Fhkl_calc = SF;
    Fhkl_esd = esdSF;
    weight = 1.0;
  }

  public StructureFactor(int ht, int kt, int lt, double d_space,
                         double expSF, double SF, double esdSF,
                         int[] hrefl, int[] krefl, int[] lrefl) {
    h = ht;
    k = kt;
    l = lt;
    d_spacing = d_space;
    Fhkl_exp = expSF;
    Fhkl_calc = SF;
    Fhkl_esd = esdSF;
    reflectionhList = hrefl.clone();
    reflectionkList = krefl.clone();
    reflectionlList = lrefl.clone();
    weight = 1.0;
  }

}
