/*
 * @(#)MaudWebStart.java created 10/06/2002 Mesiano
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

//import it.unitn.ing.rista.util.*;

//import javax.jnlp.*;
//import java.net.*;

/**
 * The MaudWebStart is a basic class to
 * startup the Maud program as an application.
 * It performs constant and interface (swing) initialization and launch
 * the principal frame mainFrame.
 *
 * @version $Revision: 1.5 $, $Date: 2006/07/20 13:39:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MaudWebStart extends Maud {

  /*public static void main(String args[]) {
    System.setProperty("apple.laf.useScreenMenuBar","true");
    System.setProperty("apple.awt.use-file-dialog-packages","true");
    System.setProperty("apple.awt.showGrowBox","true");
    Constants.webStart = true;
    try {
      BasicService bs = ((BasicService) ServiceManager.lookup("javax.jnlp.BasicService"));
      String ourCodebase = bs.getCodeBase().toString();
      Constants.ourCodebase = new URL(ourCodebase);
    } catch (Exception ue) {
      ue.printStackTrace();
      try {
        Constants.ourCodebase = new URL("http://localhost");
      } catch (MalformedURLException mue) {
        mue.printStackTrace();
      }
    }
    initInteractive();
    try {
//      if (Constants.macosx)
//        goMacInteractive();
//      else
        goInteractive();
    } catch (Throwable e) {
      goInteractive();
    }
  }*/

}
