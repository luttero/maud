/*
 * @(#)D20ILLf20format.java created May 11, 2005 Riva Del Garda, Italstructures
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.MultDiffrDataFile;
import it.unitn.ing.rista.diffr.XRDcat;


/**
 * The D20ILLf20format is a class to load .f20 datafiles from D20
 * instrument at ILL
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2005/09/07 17:23:34 $
 * @since JDK1.1
 */

public class D20ILLf20format extends D1BILLDataFile {

    public D20ILLf20format(XRDcat aobj, String alabel) {
      super(aobj, alabel);
      identifier = ".f20";
    }

    public D20ILLf20format() {
      identifier = ".f20";
    }
}
