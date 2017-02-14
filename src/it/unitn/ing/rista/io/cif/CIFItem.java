/*
 * @(#)CIFItem.java created 8/04/1999 Pergine Vals.
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io.cif;

import java.io.*;
import java.util.*;
import java.net.*;

import it.unitn.ing.rista.util.*;

/**
 *  The CIFItem is a utility class to read a CIF entry from a CIF file
 *
 *
 * @version $Revision: 1.5 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CIFItem {

  public String cif = null;
  public String thestring = null;
  protected CIFUtil cifUtil = null;

  public boolean free = false;
  public String thestringerror = null;
  public String theminValue = null;
  public String themaxValue = null;
  public String refName = null;
  public String refBound = null;
  public String constant = null;
  public String ratio = null;
	public String expression = null;
  public boolean autoTrace = false, positive = false;

  public CIFItem(String acif, String astring, String astringerror, boolean isFree) {
    super();
    cif = new String(acif);
    free = isFree;
    thestring = new String(astring);
    if (astringerror != null)
      thestringerror = new String(astringerror);
  }

  public CIFItem(String acif, CIFtoken atoken) {
    this(acif, atoken.thestring, atoken.thestringerror, atoken.free);
    theminValue = atoken.minValue;
    themaxValue = atoken.maxValue;
    refName = atoken.refName;
    refBound = atoken.refBound;
    constant = atoken.constant;
    ratio = atoken.ratio;
	  expression = atoken.expression;
    autoTrace = atoken.autoTrace;
    positive = atoken.positive;
  }

  public CIFItem(CIFUtil cifutil) {
    cifUtil = cifutil;
  }

  public void readCIFItem() {
  }

}




