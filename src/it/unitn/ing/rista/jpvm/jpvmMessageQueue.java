/* jpvmMessageQueue.java
 *
 * The jpvmMessageQueue class implements a searchable database
 * of jpvm messages. The database can be searched by message tag,
 * source process, both, or neither (i.e. wildcard search).
 *
 * Adam J Ferrari
 * Sat 05-25-1996
 *
 * modified by
 * Luca Lutterotti
 * Verona, 12 November 1999
 *
 * Copyright (C) 1996  Adam J Ferrari, 1999 Luca Lutterotti
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */

package it.unitn.ing.rista.jpvm;

/**
 *
 * @version $Revision: 1.3 $, $Date: 2004/01/21 11:14:51 $
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class jpvmMessageQueue {
  private jpvmMessageQueueElement list_head;
  private jpvmMessageQueueElement list_tail;

  private synchronized void addElement(jpvmMessageQueueElement nw) {
    if (list_head == null) {
      list_head = list_tail = nw;
      return;
    }
    list_tail.next = nw;
    list_tail = nw;
  }

  private synchronized void deleteElement(jpvmMessageQueueElement d) {
    if (list_head == null) return;
    if (list_head == d) {
      // Deleting head element.
      list_head = d.next;
      if (list_tail == d) list_tail = null;
      return;
    }
    jpvmMessageQueueElement tmp = list_head;
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

  private synchronized jpvmMessageQueueElement find() {
    return list_head;
  }

  private synchronized jpvmMessageQueueElement find(int tag) {
    jpvmMessageQueueElement tmp = list_head;
    while (tmp != null) {
      if (tmp.message.messageTag == tag) return tmp;
      tmp = tmp.next;
    }
    return null;
  }

  private synchronized jpvmMessageQueueElement find(jpvmTaskId tid) {
    jpvmMessageQueueElement tmp = list_head;
    while (tmp != null) {
      if (tmp.message.sourceTid.equals(tid)) return tmp;
      tmp = tmp.next;
    }
    return null;
  }

  private synchronized jpvmMessageQueueElement find(jpvmTaskId tid,
                                                    int tag) {
    jpvmMessageQueueElement tmp = list_head;
    while (tmp != null) {
      if ((tmp.message.sourceTid.equals(tid)) &&
              (tmp.message.messageTag == tag))
        return tmp;
      tmp = tmp.next;
    }
    return null;
  }

  public jpvmMessageQueue() {
    list_head = list_tail = null;
  }

  public synchronized void enqueue(jpvmMessage m) {
    jpvmMessageQueueElement nw = new jpvmMessageQueueElement(m);
    addElement(nw);
    notifyAll();
  }

  public synchronized boolean probe() {
    jpvmMessageQueueElement tmp = find();
    return (tmp != null);
  }

  public synchronized boolean probe(int tag) {
    jpvmMessageQueueElement tmp = find(tag);
    return (tmp != null);
  }

  public synchronized boolean probe(jpvmTaskId tid) {
    jpvmMessageQueueElement tmp = find(tid);
    return (tmp != null);
  }

  public synchronized boolean probe(jpvmTaskId tid, int tag) {
    jpvmMessageQueueElement tmp = find(tid, tag);
    return (tmp != null);
  }

  public synchronized jpvmMessage dequeue() {
    jpvmMessageQueueElement tmp = null;
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

  public synchronized jpvmMessage dequeue(int tag) {
    jpvmMessageQueueElement tmp = null;
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

  public synchronized jpvmMessage dequeue(jpvmTaskId tid) {
    jpvmMessageQueueElement tmp = null;
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

  public synchronized jpvmMessage dequeue(jpvmTaskId tid, int tag) {
    jpvmMessageQueueElement tmp = null;
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

  public synchronized jpvmMessage dequeueNonBlock() {
    jpvmMessageQueueElement tmp = find();
    if (tmp != null) {
      deleteElement(tmp);
      return tmp.message;
    }
    return null;
  }

  public synchronized jpvmMessage dequeueNonBlock(int tag) {
    jpvmMessageQueueElement tmp = find(tag);
    if (tmp != null) {
      deleteElement(tmp);
      return tmp.message;
    }
    return null;
  }

  public synchronized jpvmMessage dequeueNonBlock(jpvmTaskId tid) {
    jpvmMessageQueueElement tmp = find(tid);
    if (tmp != null) {
      deleteElement(tmp);
      return tmp.message;
    }
    return null;
  }

  public synchronized jpvmMessage dequeueNonBlock(jpvmTaskId tid, int tag) {
    jpvmMessageQueueElement tmp = find(tid, tag);
    if (tmp != null) {
      deleteElement(tmp);
      return tmp.message;
    }
    return null;
  }
}

class jpvmMessageQueueElement {
  public jpvmMessage message;
  public jpvmMessageQueueElement next;

  public jpvmMessageQueueElement() {
    message = null;
    next = null;
  }

  public jpvmMessageQueueElement(jpvmMessage m) {
    message = m;
    next = null;
  }
}

