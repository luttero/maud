package it.unitn.ing.esqui.server;

import java.net.*;
import java.io.*;
import java.util.*;

/** Vulture.java
 * <br>
 * Title:			<b>ESQUI Vulture</b>
 * </br>
 * Description:	This class waits to be notified that a thread
 is dying (exiting)  and then cleans up the list
 of threads and the graphical list.
 * @author:			Leonardo Cont, December 2000
 * @revision:		December 2000
 * @comment:		none
 */

class Vulture extends Thread {
  protected Server server;

  protected Vulture(Server server) {
    super(server.threadgroup, "Connections Vulture");
    this.server = server;
    this.start();
  }

//	This is the method that waits for notification of exiting threads
//	and cleans up the lists.  It is a synchronized method, so it
//	acquires a lock on the `this' object before running.  This is
//	necessary so that it can call wait() on this.  Even if the
//	the Connection objects never call notify(), this method wakes up
//	every ten seconds and checks all the connections, just in case.
//	Note also that all access to the Vector of connections and to
//	the GUI List component are within a synchronized block as well.
//	This prevents the Server class from adding a new connection while
//	we're removing an old one.
  public synchronized void run() {
    ClientConnection connection;
    boolean mark = false;
    for (; ;) {
      try {
        this.wait(10000);
      } catch (InterruptedException e) {
        ;
      }
      mark = false;
//					Prevent simultaneous access
      synchronized (server.connections) {
//							Loop through the connections
        for (int i = 0; i < server.connections.size(); i++) {
          connection = (ClientConnection) server.connections.elementAt(i);
          if (mark)
            connection.setConnectionNumber(i);
//									If the connection thread isn't alive anymore,
//									remove it from the vector and list
          if (!connection.isAlive()) {
            server.connections.removeElementAt(i);
            server.connectionListModel.removeElementAt(i);
//											Back of 1 step
            i--;
//											Mark this connection, so the next elements can
//											change its own name with the correct connection number
            mark = true;
          }
        }
      }
    }
  }
}
