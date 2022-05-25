/*
 * @(#)XGridHello.java created Mar 29, 2006 Casalino
 *
 * Copyright (c) 1996-2006 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.xgridclient;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.io.*;

/**
 * The XGridHello is a class to show how to use the XGridClient.
 * Compile it and put it in a jar with the name: xgridhello.jar
 *
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridHello {

  public static void main(String[] args) {
      try {
        String hostName = InetAddress.getLocalHost().getCanonicalHostName();
        System.out.println("Hello world from " + hostName);
      } catch (UnknownHostException e) {
        System.out.println("Hello world from unknown!");
      }
      if ((new File(XGrid.xgridFilenames[1])).exists()) {
        System.out.println("The file " + XGrid.xgridFilenames[1] + " is correctly present here.");
      }
    System.exit(0);
  }

}
