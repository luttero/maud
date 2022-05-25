/*
 * @(#)Maudette.java created Oct 12, 2004 Casalino
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

package com.radiographema;

import it.unitn.ing.rista.util.Constants;
import com.radiographema.Maud;


/**
 * The Maudette is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2006/07/20 13:39:02 $
 * @since JDK1.1
 */

public class Maudette {

  public static void main(String args[]) {

//	  System.out.println("Starting Maudette program, wait........");

    System.setProperty("apple.laf.useScreenMenuBar","true");
    System.setProperty("apple.awt.use-file-dialog-packages","true");
    System.setProperty("apple.awt.showGrowBox","true");
    Constants.textonly = false;
    Constants.newMaud = true;
    Maud.initInteractive();
    Maud.goDiffractionInteractive();
  }

}
