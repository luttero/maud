/*
 * @(#)LayerSolutionMethod.java created 29/10/2001 Le Mans
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.io.*;
import java.lang.*;
import java.util.*;
import java.awt.event.*;
import javax.swing.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.cif.*;


/**
 *  The LayerSolutionMethod is a generic model for working out heterostructures
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:04 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class LayerSolutionMethod extends XRDcat {

  public LayerSolutionMethod(XRDcat aobj, String alabel) {
    super(aobj, alabel);
  }

  public LayerSolutionMethod(XRDcat aobj) {
    this(aobj, "Layer workout method x");
  }

  public LayerSolutionMethod() {
  }

  public boolean workoutHeterostructure(DataFileSet[] tmpDataSet) {
    return true;
  }

  public boolean canWorkoutHeterostructure() {
    return false;
  }

}
