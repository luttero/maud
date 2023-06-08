/*
 * @(#)Plane.java created 01/01/1997 Mesiano
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

import java.lang.*;
import java.util.*;

/**
 * The Plane is a class
 *
 * @version $Revision: 1.4 $, $Date: 2005/05/06 18:07:25 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Plane extends XRDcat {
  protected static String[] diclistc = {"refln_index_h", "refln_index_k", "refln_index_l"};
  protected static String[] diclistcrm = {"refln_index_h", "refln_index_k", "refln_index_l"};

  protected static String[] classlistc = {};

  public Plane(XRDcat obj, String alabel) {
    super(obj, alabel);
    initBaseObject();
  }

  public Plane(XRDcat afile) {
    this(afile, "Plane x");
  }

	public Plane() {}

  public void initConstant() {
    Nstring = 3;
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
  }

  public void initParameters() {
    super.initParameters();
    stringPlaneTohkl("0 0 1");
  }

  public static int[] stringPlaneTohkl(String aplanestring) {

    StringTokenizer st = new StringTokenizer(aplanestring, " ,");

    int hkl[] = new int[3];
    for (int i = 0; i < 3; i++) {
      String token = st.nextToken();
      hkl[i] = Integer.valueOf(token).intValue();
    }
    return hkl;
  }

  public void seth(int number) {
    stringField[0] = Integer.toString(number);
  }

  public void seth(String number) {
    stringField[0] = new String(number);
  }

  public void setk(int number) {
    stringField[1] = Integer.toString(number);
  }

  public void setk(String number) {
    stringField[1] = new String(number);
  }

  public void setl(int number) {
    stringField[2] = Integer.toString(number);
  }

  public void setl(String number) {
    stringField[2] = new String(number);
  }

  public int geth() {
    return Integer.valueOf(stringField[0]).intValue();
  }

  public int getk() {
    return Integer.valueOf(stringField[1]).intValue();
  }

  public int getl() {
    return Integer.valueOf(stringField[2]).intValue();
  }

  public String getPlanehkl() {
    String tmpstring = new String(stringField[0]);
    tmpstring = tmpstring.concat(" ");
    tmpstring = tmpstring.concat(stringField[1]);
    tmpstring = tmpstring.concat(" ");
    return tmpstring.concat(stringField[2]);
  }
}
