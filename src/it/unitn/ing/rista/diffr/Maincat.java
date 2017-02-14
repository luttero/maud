/*
 * @(#)Maincat.java created 01/01/1997 Mesiano
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
import java.io.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.io.cif.*;

/**
 * The Maincat is a class
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Maincat extends XRDcat {

	public Maincat(XRDcat afile, String alabel) {
    super(afile, alabel);
  }

  public void writeDataField(BufferedWriter out) {
    String blockID = identifier + "_" + Misc.toStringNoBlank(toXRDcatString());
    CIFDataBlock.writeBlockDecl(out, blockID, this);
  }
}

