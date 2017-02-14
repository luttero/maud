/*
 * @(#)XYEThreeColumnDataFile.java created Feb 12, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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
package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.XRDcat;

/**
 * The XYEThreeColumnDataFile is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 12, 2009 5:13:54 PM $
 * @since JDK1.1
 */
public class XYEThreeColumnDataFile  extends it.unitn.ing.rista.diffr.data.ETHThreeColumnDataFile {

  public XYEThreeColumnDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    identifier = ".xye";
  }

  public XYEThreeColumnDataFile() {
    identifier = ".xye";
  }

}
