/*
 * @(#)Inel2DataFile.java created 31/12/1998 Mesiano
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

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;

import java.io.*;
import java.lang.*;
import java.util.*;

import it.unitn.ing.rista.util.*;


/**
 *  The Inel2DataFile is a class
 *
 *
 * @version $Revision: 1.4 $, $Date: 2004/08/12 09:36:06 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Inel2DataFile extends InelDataFile {

  public Inel2DataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".ttx";
  }

  public Inel2DataFile() {
    identifier = ".ttx";
  }

}
