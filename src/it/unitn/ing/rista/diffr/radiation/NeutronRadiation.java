/*
 * @(#)NeutronRadiation.java created 06/01/1999 Riva del Garda
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

package it.unitn.ing.rista.diffr.radiation;

import java.lang.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

import java.awt.*;
import java.awt.*;
import java.awt.event.*;

/**
 *  The NeutronRadiation is a class
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:07 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class NeutronRadiation extends RadiationType {

  public NeutronRadiation(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = "Neutron";
    IDlabel = "Neutron";
    description = "Neutron radiation type";
  }

  public NeutronRadiation(XRDcat aobj) {
    this(aobj, "Neutron");
  }

  public NeutronRadiation() {
    identifier = "Neutron";
    IDlabel = "Neutron";
    description = "Neutron radiation";
  }

  public void initParameters() {
    super.initParameters();
  }

  public boolean isNeutron() {
    return true;
  }

}
