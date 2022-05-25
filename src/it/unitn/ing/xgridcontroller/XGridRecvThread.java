/*
 * @(#)XGridRecvThread.java created Feb 17, 2006 Newark Liberty Int. Airport
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

package it.unitn.ing.xgridcontroller;

import it.unitn.ing.rista.util.Misc;


/**
 * The XGridRecvThread is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridRecvThread extends Thread {
  private XGridRecvConnection conn;
  private XGridQueue queue;
  int my_num;
  static int num = 0;


  public XGridRecvThread(XGridRecvConnection c, XGridQueue q) {
    conn = c;
    queue = q;
    num++;
    my_num = num;
  }

  public void run() {
    boolean alive = true;
    while (alive) {
      try {
        System.out.println("XGridRecvThread (" + my_num + ") - blocking " +
                "for a message.");
        XGridServerMessage nw = new XGridServerMessage(conn);
        System.out.println("XGridRecvThread (" + my_num + ") - got a " +
                "new message.");
        queue.enqueue(nw);
        Thread.yield();
      } catch (XGridException jpe) {
        System.out.println("XGridRecvThread, " +
                "connection closed");
        alive = false;
      }
    }
  }

}
