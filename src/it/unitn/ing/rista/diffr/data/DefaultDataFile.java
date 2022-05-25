/*
 * @(#)DefaultDataFile.java created 10/12/1998 Mesiano
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

// the package name

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;

import java.io.*;
import java.lang.*;
import java.util.*;

/*
 * The DefaultDataFile is a class to load the Venezia data format
 * @version $Revision: 1.3 $, $Date: 2003/04/23 00:20:42 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DefaultDataFile extends it.unitn.ing.rista.diffr.data.VeneziaDataFile {

  public DefaultDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);

    identifier = "*";
  }

  public DefaultDataFile() {

    identifier = "*";
  }

}
