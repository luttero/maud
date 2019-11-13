/*
 * @(#)Bound.java created 21/09/1998 05:33 Pergine Vals.
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
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
import java.lang.*;

import it.unitn.ing.rista.util.ListVector;

/**
 *  The Bound is a class for binding parameters input/output.
 *
 *
 *
 * @version $Revision: 1.4 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Bound extends Maincat {
  protected static String[] diclistc = {"_riet_par_bound", "_riet_par_bound_to",
                                        "_riet_par_bound_ratio", "_riet_par_bound_const"};
  protected static String[] diclistcrm = {"_riet_par_bound", "_riet_par_bound_to",
                                        "_riet_par_bound_ratio", "_riet_par_bound_const"};

  public Bound(XRDcat afile, String alabel) {
    super(afile, alabel);
    initXRD();
    identifier = new String("bound");
  }

  public Bound(XRDcat afile) {
    this(afile, "bound x");
  }

  public Bound(XRDcat afile, String alabel, int indexbound, int indexboundto,
               String ratio, String constant) {
    this(afile, alabel);
    setBound(Integer.toString(indexbound));
    setBoundTo(Integer.toString(indexboundto));
    setRatio(ratio);
    setConstant(constant);
  }

  public Bound() {}

  public void initConstant() {
    Nstring = 4;
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
  }

  public void initParameters() {
    super.initParameters();
  }

  public void setBound(String value) {
    stringField[0] = new String(value);
  }

  public void setBoundTo(String value) {
    stringField[1] = new String(value);
  }

  public void setRatio(String value) {
    stringField[2] = new String(value);
  }

  public void setConstant(String value) {
    stringField[3] = new String(value);
  }

  public int getBound() {
    return Integer.valueOf(stringField[0]).intValue();
  }

  public int getBoundTo() {
    return Integer.valueOf(stringField[1]).intValue();
  }

  public String getRatio() {
    return stringField[2];
  }

  public String getConstant() {
    return stringField[3];
  }

}

