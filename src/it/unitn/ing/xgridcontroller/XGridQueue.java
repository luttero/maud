/*
 * @(#)XGridQueue.java created Feb 17, 2006 Newark Liberty Int. Airport
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


/**
 * The XGridQueue is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:05 $
 * @since JDK1.1
 */

public class XGridQueue {

  private XgridMessageQueueElement list_head;
  private XgridMessageQueueElement list_tail;

  private synchronized void addElement(XgridMessageQueueElement nw) {
    if (list_head == null) {
      list_head = list_tail = nw;
      return;
    }
    list_tail.next = nw;
    list_tail = nw;
  }

  private synchronized void deleteElement(XgridMessageQueueElement d) {
    if (list_head == null) return;
    if (list_head == d) {
      // Deleting head element.
      list_head = d.next;
      if (list_tail == d) list_tail = null;
      return;
    }
    XgridMessageQueueElement tmp = list_head;
    while (tmp.next != d) {
      tmp = tmp.next;
      if (tmp == null) {
        // Element wasn't in the list
        return;
      }
    }
    tmp.next = d.next;
    if (list_tail == d) list_tail = tmp;
  }

  private synchronized XgridMessageQueueElement find() {
    return list_head;
  }

  private synchronized XgridMessageQueueElement find(int tag) {
    XgridMessageQueueElement tmp = list_head;
    while (tmp != null) {
      if (tmp.message.messageTag == tag) return tmp;
      tmp = tmp.next;
    }
    return null;
  }

  private synchronized XgridMessageQueueElement find(XGridTaskId tid) {
    XgridMessageQueueElement tmp = list_head;
    while (tmp != null) {
      if (tmp.message.sourceTid.equals(tid)) return tmp;
      tmp = tmp.next;
    }
    return null;
  }

  private synchronized XgridMessageQueueElement find(XGridTaskId tid,
                                                    int tag) {
    XgridMessageQueueElement tmp = list_head;
    while (tmp != null) {
      if ((tmp.message.sourceTid.equals(tid)) &&
              (tmp.message.messageTag == tag))
        return tmp;
      tmp = tmp.next;
    }
    return null;
  }

  public XGridQueue() {
    list_head = list_tail = null;
  }

  public synchronized void enqueue(XGridServerMessage m) {
    XgridMessageQueueElement nw = new XgridMessageQueueElement(m);
    addElement(nw);
    notifyAll();
  }

  public synchronized boolean probe() {
    XgridMessageQueueElement tmp = find();
    return (tmp != null);
  }

  public synchronized boolean probe(int tag) {
    XgridMessageQueueElement tmp = find(tag);
    return (tmp != null);
  }

  public synchronized boolean probe(XGridTaskId tid) {
    XgridMessageQueueElement tmp = find(tid);
    return (tmp != null);
  }

  public synchronized boolean probe(XGridTaskId tid, int tag) {
    XgridMessageQueueElement tmp = find(tid, tag);
    return (tmp != null);
  }

  public synchronized XGridServerMessage dequeue() {
    XgridMessageQueueElement tmp = null;
    while (true) {
      if ((tmp = find()) != null) {
        deleteElement(tmp);
        return tmp.message;
      }
      try {
        wait();
      } catch (InterruptedException ie) {
      }
    }
  }

  public synchronized XGridServerMessage dequeue(int tag) {
    XgridMessageQueueElement tmp = null;
    while (true) {
      if ((tmp = find(tag)) != null) {
        deleteElement(tmp);
        return tmp.message;
      }
      try {
        wait();
      } catch (InterruptedException ie) {
      }
    }
  }

  public synchronized XGridServerMessage dequeue(XGridTaskId tid) {
    XgridMessageQueueElement tmp = null;
    while (true) {
      if ((tmp = find(tid)) != null) {
        deleteElement(tmp);
        return tmp.message;
      }
      try {
        wait();
      } catch (InterruptedException ie) {
      }
    }
  }

  public synchronized XGridServerMessage dequeue(XGridTaskId tid, int tag) {
    XgridMessageQueueElement tmp = null;
    while (true) {
      if ((tmp = find(tid, tag)) != null) {
        deleteElement(tmp);
        return tmp.message;
      }
      try {
        wait();
      } catch (InterruptedException ie) {
      }
    }
  }

  public synchronized XGridServerMessage dequeueNonBlock() {
    XgridMessageQueueElement tmp = find();
    if (tmp != null) {
      deleteElement(tmp);
      return tmp.message;
    }
    return null;
  }

  public synchronized XGridServerMessage dequeueNonBlock(int tag) {
    XgridMessageQueueElement tmp = find(tag);
    if (tmp != null) {
      deleteElement(tmp);
      return tmp.message;
    }
    return null;
  }

  public synchronized XGridServerMessage dequeueNonBlock(XGridTaskId tid) {
    XgridMessageQueueElement tmp = find(tid);
    if (tmp != null) {
      deleteElement(tmp);
      return tmp.message;
    }
    return null;
  }

  public synchronized XGridServerMessage dequeueNonBlock(XGridTaskId tid, int tag) {
    XgridMessageQueueElement tmp = find(tid, tag);
    if (tmp != null) {
      deleteElement(tmp);
      return tmp.message;
    }
    return null;
  }
}

class XgridMessageQueueElement {
  public XGridServerMessage message;
  public XgridMessageQueueElement next;

  public XgridMessageQueueElement() {
    message = null;
    next = null;
  }

  public XgridMessageQueueElement(XGridServerMessage m) {
    message = m;
    next = null;
  }
}
