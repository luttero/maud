/* jpvmDaemon.java
 *
 * The jpvm Daemon is a special jpvm task that runs on every
 * host in a jpvm parallel virtual machine. The jpvm Daemon
 * is responsible for servicing requests to create new jpvm
 * tasks. Unlike standard PVM, the jpvm Daemon is never
 * responsible for routing message communication.
 *
 * Adam J Ferrari
 * Sun 05-26-1996
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

import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;

/**
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:58 $
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class jpvmDaemon {
  private static jpvmEnvironment jpvm = null;
  private static jpvmTaskId myTid = null;

  private static jpvmTaskList tasks = null;
  private static jpvmTaskList hosts = null;
  private static jpvmSpawnWorkOrderList spawnOrders = null;
  private static int maxCreateOrders = 256;
  private static jpvmCreateWorkOrder createOrders[];
  private static int nextCreateOrder = 0;

  private static boolean log_on = true;
  private static boolean debug_on = true;
//  public static String data_HTTP_URL = MaudPreferences.getPref("jpvm.defaultHTMLdataLocation",
//          "http://127.0.0.1/");

  public static boolean shouldRun = false;
  public static boolean newVM = false;
  public static boolean useLocalTask = false;
  public static int numTasks = 4;
  public static final int sendPVTag = 111;
  public static final int recvPVTag = 112;
  public static final String classpath = "Maud.jar";


  // Which version of the JVM should be used to host tasks?
  private static String java_exec = "java";
  // private static String       		java_exec="kaffe";

  public static void main(String args[]) {
    newVM = false;
    if (args != null && args.length > 0) {
      for (int i = 0; i < args.length; i++) {
        if (args[i].equalsIgnoreCase("-newVM"))
          newVM = true;
      }
    }

    startDaemon();

    System.exit(0);
  }

  public static jpvmEnvironment getJpvm() {
    return jpvm;
  }

  public static void startDaemon() {
    if (shouldRun)
      return;
    shouldRun = true;
    try {
      // Initialize data structures
      jpvm = new jpvmEnvironment(true);
      myTid = jpvm.pvm_mytid();
      tasks = new jpvmTaskList();
      hosts = new jpvmTaskList();
      spawnOrders = new jpvmSpawnWorkOrderList();
      createOrders = new jpvmCreateWorkOrder[maxCreateOrders];

      // Announce location
      log(jpvm.pvm_mytid().toString());
      hosts.addTask(jpvm.pvm_mytid(), jpvm.pvm_mytid().getHost());

      writeDaemonFile();

      // Main server loop...
      while (shouldRun) {
        jpvmMessage req = jpvm.pvm_recv();
        jpvmTaskId client = req.sourceTid;
        int request = req.messageTag;
        String reqName;

        switch (request) {
          case (jpvmDaemonMessageTag.jpvmdPingRequest):
            reqName = "Ping";
            break;
          case (jpvmDaemonMessageTag.jpvmdRegisterTask):
            reqName = "RegisterTask";
            break;
          case (jpvmDaemonMessageTag.jpvmdRegisterChild):
            reqName = "RegisterChild";
            break;
          case jpvmDaemonMessageTag.jpvmdSpawnTask:
            reqName = "SpawnTask";
            break;
          case jpvmDaemonMessageTag.jpvmdCreateTask:
            reqName = "CreateTask";
            break;
          case jpvmDaemonMessageTag.jpvmdCreatedTask:
            reqName = "CreateTaskReturn";
            break;
          case jpvmDaemonMessageTag.jpvmdDeleteTask:
            reqName = "DeleteTask";
            break;
          case jpvmDaemonMessageTag.jpvmdTaskStatus:
            reqName = "TaskStatus";
            break;
          case jpvmDaemonMessageTag.jpvmdAddHost:
            reqName = "AddHost";
            break;
          case jpvmDaemonMessageTag.jpvmdAddHostBcast:
            reqName = "AddHostNotify";
            break;
          case jpvmDaemonMessageTag.jpvmdDeleteHost:
            reqName = "DeleteHost";
            break;
          case jpvmDaemonMessageTag.jpvmdDeleteHostBcast:
            reqName = "DeleteHostNotify";
            break;
          case jpvmDaemonMessageTag.jpvmdHostStatus:
            reqName = "HostStatus";
            break;
          case jpvmDaemonMessageTag.jpvmdHostHalt:
            reqName = "HostHalt";
            break;
          case jpvmDaemonMessageTag.jpvmdHalt:
            reqName = "Halt";
            break;
          default:
            reqName = "Unknown Request";
        }

        if (debug_on) {
          log("new request type=" + request +
                  ",\"" + reqName + "\", from " +
                  client.toString());
        }

        switch (request) {
          case (jpvmDaemonMessageTag.jpvmdPingRequest):
            Ping(client, req.buffer);
            break;
          case (jpvmDaemonMessageTag.jpvmdRegisterTask):
            RegisterTask(client, req.buffer);
            break;
          case (jpvmDaemonMessageTag.jpvmdRegisterChild):
            RegisterChild(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdSpawnTask:
            SpawnTask(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdCreateTask:
            CreateTask(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdCreatedTask:
            CreatedTask(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdDeleteTask:
            DeleteTask(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdTaskStatus:
            TaskStatus(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdAddHost:
            AddHost(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdAddHostBcast:
            AddHostBcast(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdDeleteHost:
            DeleteHost(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdDeleteHostBcast:
            DeleteHostBcast(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdHostStatus:
            HostStatus(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdHostHalt:
            HostHalt(client, req.buffer);
            break;
          case jpvmDaemonMessageTag.jpvmdHalt:
            Halt(client, req.buffer);
            break;
          default:
            perror("unknown request type");
        }

        try {
          Thread.sleep(100);
        } catch (Exception ie) {
        }
      }
    } catch (jpvmException jpe) {
      jpvmDebug.note("jpvmDaemon, internal jpvm error.");
    }
  }

  public static void stopDaemon() {
    try {
      jpvm.pvm_halt();
    } catch (jpvmException jpe) {
      jpe.printStackTrace();
      jpvmDebug.note("jpvmDaemon, internal jpvm error.");
    }
    shouldRun = false;
    jpvm = null;
    myTid = null;
    tasks = null;
    hosts = null;
    spawnOrders = null;
    createOrders = null;
  }

  private static void daemonBcast(jpvmBuffer buf, int tag) {
    jpvmTaskListRecord tmp = hosts.firstIter();
    while (tmp != null) {
      try {
        jpvm.pvm_send(buf, tmp.tid, tag);
        tmp = hosts.nextIter();
      } catch (jpvmException jpe) {
        perror("problem sending to daemon " + tmp.tid.toString());
        hosts.deleteTask(tmp.tid);
      }
    }
  }

  private static void RegisterTask(jpvmTaskId client, jpvmBuffer req) {
    try {
      String name = req.upkstr();
      tasks.addTask(client, name);
    } catch (jpvmException jpe) {
      log("bad RegisterTask invocation");
    }
  }

  private static void RegisterChild(jpvmTaskId client, jpvmBuffer req) {
    int regNum = -1;

    // Child process reporting in. Notify the remote client that
    // requested this local task creation.

    try {
      regNum = req.upkint();
    } catch (jpvmException jpe) {
      perror("bad RegisterChild invocation");
      return;
    }

    if (regNum < 0 || regNum >= maxCreateOrders) {
      perror("RegisterChild, child registration number " +
              regNum + "out of bounds");
      return;
    }

    jpvmCreateWorkOrder order = createOrders[regNum];
    if (order == null) {
      perror("RegisterChild, child registration number " +
              regNum + "not expected");
      return;
    }
    if (!order.outstanding) {
      perror("RegisterChild, child registration number " +
              regNum + "unexpected");
      return;
    }
    order.outstanding = false;

    // Return the blessed event to the requester
    try {
      jpvmBuffer buf = new jpvmBuffer();
      buf.pack(1);
      buf.pack(client);
      buf.pack(order.order);
      jpvm.pvm_send(buf, order.client,
              jpvmDaemonMessageTag.jpvmdCreatedTask);
    } catch (jpvmException jpe) {
      perror("RegisterChild, \"" + jpe + "\" sending to client " +
              order.client.toString());
    }
  }

  private static void SpawnTask(jpvmTaskId client, jpvmBuffer req) {
    int num = 0;
    String name = null;
    jpvmBuffer buf = new jpvmBuffer();
    try {
      num = req.upkint();
      name = req.upkstr();
    } catch (jpvmException jpe) {
      perror("bad SpawnTask invocation");
      return;
    }
    if (num == 0) {
      buf.pack(num);
      try {
        jpvm.pvm_send(buf, client,
                jpvmDaemonMessageTag.jpvmdSpawnTask);
      } catch (jpvmException jpe) {
        perror("SpawnTask, problem sending "
                + "to client " +
                client.toString());
      }
      return;
    }

    // Create a work order for the spawn
    jpvmSpawnWorkOrder order;
    order = spawnOrders.newOrder(num);
    order.client = client;

    // Create a request to create a task on a remote host
    jpvmBuffer creq = new jpvmBuffer();
    creq.pack(name); // Pack the class to create
    creq.pack(client); // Pack parent of the created tasks
    creq.pack(order.order);
//    creq.pack(data_HTTP_URL);

    // Schedule on known hosts round robin style
    jpvmTaskListRecord target = null;
    for (int i = 0; i < num; i++) {
      if (target == null) target = hosts.firstIter();
      if (target == null) {
        perror("no hosts in SpawnTask invocation");
        return;
      }
      if (target.tid.equals(jpvm.pvm_mytid())) {
        creq.rewind();
        CreateTask(jpvm.pvm_mytid(), creq);
      } else {
        try {
          jpvm.pvm_send(creq, target.tid,
                  jpvmDaemonMessageTag.jpvmdCreateTask);
        } catch (jpvmException jpe) {
          perror("SpawnTask, error scheduling " +
                  "on host " + target.tid.toString());
        }
      }
      target = hosts.nextIter();
    }
  }

  private static void CreatedTask(jpvmTaskId client, jpvmBuffer req) {
    int count = -1;
    jpvmTaskId child = null;
    int orderNum = -1;

    try {
      count = req.upkint();
      if (count == 1) {
        child = req.upktid();
        orderNum = req.upkint();
      }
    } catch (jpvmException jpe) {
      perror("CreatedTask, bad report from " +
              client.toString());
    }

    // Look up which spawn order this is in regards to
    jpvmSpawnWorkOrder order;
    order = spawnOrders.lookup(orderNum);

    if (order == null) {
      perror("CreatedTask, order number " + orderNum +
              " is not valid");
      return;
    }

    // Update the status of the order
    order.tids[order.numDone] = child;
    order.numDone++;

    if (order.numDone == order.num) {
      // The order is complete - return the good
      // news to the original client
      jpvmBuffer buf = new jpvmBuffer();
      buf.pack(order.numDone);
      buf.pack(order.tids, order.numDone, 1);
      try {
        jpvm.pvm_send(buf, order.client,
                jpvmDaemonMessageTag.jpvmdSpawnTask);
      } catch (jpvmException jpe) {
        perror("CreatedTask, \"" + jpe + "\" sending to client "
                + order.client.toString());
      }

      // Throw away the order
      spawnOrders.doneOrder(order);
    }
  }

  private static void CreateTask(jpvmTaskId client, jpvmBuffer req) {
    String name = null;//, httpDataServer = null;
    jpvmTaskId parent = null;
    int order;

    try {
      name = req.upkstr();
      parent = req.upktid();
      order = req.upkint();
//      httpDataServer = req.upkstr();
    } catch (jpvmException jpe) {
      perror("bad CreateTask invocation");
      return;
    }

    if (createOrders[nextCreateOrder] == null)
      createOrders[nextCreateOrder] = new jpvmCreateWorkOrder();

    if (createOrders[nextCreateOrder].outstanding) {
      perror("too many outstanding task creation requests");
      return;
    }

    // Log the task creation request so when the task reports
    // in we'll know it was expected
    createOrders[nextCreateOrder].order = order;
    createOrders[nextCreateOrder].client = client;
    createOrders[nextCreateOrder].outstanding = true;


    // Create a thread to execute the new task
    String args[] = new String[11];
    args[0] = java_exec;
    args[1] = "-Djpvm.daemon=" + jpvm.pvm_mytid().getPort();
    args[2] = "-Djpvm.parhost=" + parent.getHost();
    args[3] = "-Djpvm.parport=" + parent.getPort();
    args[4] = "-Djpvm.taskname=" + name;
    args[5] = "-Djpvm.regnum=" + nextCreateOrder;
	args[6] = "-cp";
	args[7] = classpath;
    args[8] = name;
//    args[9] = "-jpvm";
//    args[10] = "-location=" + httpDataServer;
    if (debug_on)
      log("exec( " + args[0] + " " + args[1] + " " + args[2] + " " +
              args[3] + " " + args[4] + " " + args[5] + " )");

    jpvmExecTaskThread spawnThread;
    spawnThread = new jpvmExecTaskThread(jpvm, client,
            args, createOrders[nextCreateOrder]);
    nextCreateOrder++;
    spawnThread.start();
  }

  private static void DeleteTask(jpvmTaskId client, jpvmBuffer req) {
    tasks.deleteTask(client);
  }

  private static void TaskStatus(jpvmTaskId client, jpvmBuffer req) {
    jpvmBuffer buf = new jpvmBuffer();
    int n = tasks.numTasks();
    jpvmTaskId tids[] = new jpvmTaskId[n];
    buf.pack(n);
    jpvmTaskListRecord tmp = tasks.firstIter();
    int i = 0;
    while (tmp != null) {
      tids[i] = tmp.tid;
      buf.pack(tmp.name);
      tmp = tasks.nextIter();
      i++;
    }
    buf.pack(tids, n, 1);
    try {
      jpvm.pvm_send(buf, client, jpvmDaemonMessageTag.jpvmdTaskStatus);
    } catch (jpvmException jpe) {
      perror("TaskStatus, \"" + jpe + "\" sending to client " +
              client.toString());
    }
  }

  private static void AddHost(jpvmTaskId client, jpvmBuffer req) {
    jpvmBuffer buf = internalAddHosts(req);
    if (buf == null) {
      perror("AddHost, problem adding hosts");
      return;
    }
    daemonBcast(buf, jpvmDaemonMessageTag.jpvmdAddHostBcast);
  }

  private static void Ping(jpvmTaskId client, jpvmBuffer req) {
    try {
      jpvm.pvm_send(req, client,
              jpvmDaemonMessageTag.jpvmdPingReply);
    } catch (jpvmException jpe) {
      perror("ping, \"" + jpe + "\" sending to client " +
              client.toString());
    }
  }

  private static void AddHostBcast(jpvmTaskId client, jpvmBuffer req) {
    if (client.equals(myTid)) return;
    try {
      int i;
      int num = req.upkint();
      String names[] = new String[num];
      jpvmTaskId daemonTids[] = new jpvmTaskId[num];
      for (i = 0; i < num; i++)
        names[i] = req.upkstr();
      req.unpack(daemonTids, num, 1);
      for (i = 0; i < num; i++)
        hosts.addTask(daemonTids[i], names[i]);
    } catch (jpvmException jpe) {
      log("bad AddHost invocation");
    }
  }

  private static jpvmBuffer internalAddHosts(jpvmBuffer req) {
    int i,j;
    jpvmBuffer ret = new jpvmBuffer();

    int newNum = 0;
    String newNames[] = null;
    jpvmTaskId newTids[] = null;
    boolean newValid[] = null;
    int newValidNum = 0;
    try {
      // First, get the addresses of all new daemons
      newNum = req.upkint();
      newNames = new String[newNum];
      newTids = new jpvmTaskId[newNum];
      newValid = new boolean[newNum];
      for (i = 0; i < newNum; i++) newNames[i] = req.upkstr();
      req.unpack(newTids, newNum, 1);
    } catch (jpvmException jpe) {
      log("bad AddHost call");
      perror("internalAddHost, " + jpe);
      return null;
    }

    // Check the validity of all new names
    jpvmBuffer pingBuf = new jpvmBuffer();
    newValidNum = newNum;
    for (i = 0; i < newNum; i++) {
      boolean valid = true;
      try {
        jpvm.pvm_send(pingBuf, newTids[i],
                jpvmDaemonMessageTag.jpvmdPingRequest);
        jpvmMessage pingMess = jpvm.pvm_recv(newTids[i],
                jpvmDaemonMessageTag.jpvmdPingReply);
      } catch (jpvmException jpe) {
        valid = false;
        newValidNum--;
        perror("internalAddHost, ping, " + jpe);
      }
      newValid[i] = valid;
      if (valid)
        hosts.addTask(newTids[i], newNames[i]);
    }

    if (newValidNum < 1) {
      // no hosts to add!
      ret = null;
      perror("internalAddHost, no hosts added");
      return ret;
    }

    // Create the message to add all daemons, new and old
    int oldNum = hosts.numTasks();
    int totalNum = oldNum + newValidNum;
    jpvmTaskId allTids[] = new jpvmTaskId[totalNum];
    jpvmTaskListRecord tmp = hosts.firstIter();

    // Pack in the old names...
    ret.pack(totalNum);
    i = 0;
    while (tmp != null) {
      allTids[i] = tmp.tid;
      ret.pack(tmp.name);
      tmp = hosts.nextIter();
      i++;
    }
    // Pack in the old names...
    for (j = 0; j < newNum; j++)
      if (newValid[j]) {
        allTids[i] = newTids[j];
        ret.pack(newNames[j]);
        i++;
      }

    // Pack in all of the tids...
    ret.pack(allTids, totalNum, 1);
    return ret;
  }

  private static void DeleteHost(jpvmTaskId client, jpvmBuffer req) {
  }

  private static void DeleteHostBcast(jpvmTaskId client, jpvmBuffer req) {
  }

  private static void HostStatus(jpvmTaskId client, jpvmBuffer req) {
    jpvmBuffer buf = new jpvmBuffer();
    int nhosts = hosts.numTasks();
    buf.pack(nhosts);
    jpvmTaskId dTids[] = new jpvmTaskId[nhosts];
    jpvmTaskListRecord tmp = hosts.firstIter();
    int i = 0;
    while (tmp != null) {
      dTids[i] = tmp.tid;
      buf.pack(tmp.name);
      tmp = hosts.nextIter();
      i++;
    }
    buf.pack(dTids, nhosts, 1);
    try {
      jpvm.pvm_send(buf, client, jpvmDaemonMessageTag.jpvmdHostStatus);
    } catch (jpvmException jpe) {
      perror("HostStatus, \"" + jpe + "\" sending to client " +
              client.toString());
    }
  }

  private static void HostHalt(jpvmTaskId client, jpvmBuffer req) {
    log("shutting down");
    stopDaemon(); //shouldRun = false;
  }

  private static void Halt(jpvmTaskId client, jpvmBuffer req) {
    jpvmBuffer buf = new jpvmBuffer();
    daemonBcast(buf, jpvmDaemonMessageTag.jpvmdHostHalt);
  }

  private static void log(String message) {
    if (log_on) {
      System.out.println("jpvm daemon: " + message);
      System.out.flush();
    }
  }

  private static void perror(String message) {
    System.err.println("jpvm daemon: " + message);
    System.err.flush();
  }

  private static void writeDaemonFile() {
    String fileName = jpvmEnvironment.pvm_daemon_file_name();
    try {
      File f = new File(fileName);
      FileOutputStream fout = new FileOutputStream(f);
      DataOutputStream dout = new DataOutputStream(fout);
      int port = myTid.getPort();
      dout.writeInt(port);
      dout.flush();
    } catch (IOException ioe) {
      perror("error writing \"" + fileName + "\"");
    }
  }
}

class jpvmTaskListRecord {
  public jpvmTaskId tid;
  public String name;
  public jpvmTaskListRecord next;

  public jpvmTaskListRecord(jpvmTaskId t, String n) {
    tid = t;
    name = n;
    next = null;
  }
}

class jpvmTaskList {
  jpvmTaskListRecord tasks;
  int num_tasks;
  jpvmTaskListRecord iter;

  public jpvmTaskList() {
    tasks = null;
    num_tasks = 0;
    iter = null;
  }

  public int numTasks() {
    return num_tasks;
  }

  public void addTask(jpvmTaskId tid, String name) {
    if (find(tid) != null) {
      // Already know about this task...
      return;
    }
    jpvmTaskListRecord nw = new jpvmTaskListRecord(tid, name);
    nw.next = tasks;
    tasks = nw;
    num_tasks++;
  }

  public void deleteTask(jpvmTaskId tid) {
    if (tasks == null) return;
    jpvmTaskListRecord tmp = tasks;

    // Check head
    if (tmp.tid.equals(tid)) {
      if (iter == tmp) iter = tmp.next;
      tasks = tasks.next;
      num_tasks--;
      return;
    }
    // Check body
    while (tmp.next != null) {
      if (tmp.next.tid.equals(tid)) {
        if (iter == tmp.next) iter = tmp.next.next;
        tmp.next = tmp.next.next;
        num_tasks--;
        return;
      }
      tmp = tmp.next;
    }
  }

  public jpvmTaskListRecord find(jpvmTaskId tid) {
    jpvmTaskListRecord tmp = tasks;
    while (tmp != null) {
      if (tmp.tid.equals(tid))
        return tmp;
      tmp = tmp.next;
    }
    return tmp;
  }

  public jpvmTaskListRecord firstIter() {
    if (tasks == null) return null;
    jpvmTaskListRecord ret = tasks;
    iter = tasks.next;
    return ret;
  }

  public jpvmTaskListRecord nextIter() {
    if (iter == null) return null;
    jpvmTaskListRecord ret = iter;
    iter = iter.next;
    return ret;
  }
}

class jpvmCreateWorkOrder {
  public int order;
  public jpvmTaskId client;
  public boolean outstanding;

  jpvmCreateWorkOrder() {
    order = 0;
    client = null;
    outstanding = false;
  }
}

class jpvmSpawnWorkOrder {
  public int order;	// Which partially completed spawn
  public jpvmTaskId tids[]; // Tids spawned
  public int num;    // Number to spawn
  public int numDone;// Number actually done
  public jpvmTaskId client; // Who placed the order?
  jpvmSpawnWorkOrder next;   // Linked list

  jpvmSpawnWorkOrder(int o, int n) {
    order = o;
    num = n;
    tids = new jpvmTaskId[n];
    numDone = 0;
    client = null;
    next = null;
  }
}

class jpvmSpawnWorkOrderList {
  private jpvmSpawnWorkOrder list = null;
  private int nextOrder = 1;

  jpvmSpawnWorkOrderList() {
    list = null;
    nextOrder = 1;
  }

  public jpvmSpawnWorkOrder newOrder(int num) {
    jpvmSpawnWorkOrder ret;

    ret = new jpvmSpawnWorkOrder(nextOrder, num);
    nextOrder++;
    ret.next = list;
    list = ret;
    return ret;
  }

  public jpvmSpawnWorkOrder lookup(int order) {
    jpvmSpawnWorkOrder ret;

    ret = list;

    while (ret != null) {
      if (ret.order == order)
        return ret;
      ret = ret.next;
    }
    return null;
  }

  public void doneOrder(jpvmSpawnWorkOrder order) {
    jpvmSpawnWorkOrder tmp;
    if (list == null || order == null)
      return;
    if (order == list) {
      list = list.next;
      return;
    }
    tmp = list;
    while (tmp.next != null) {
      if (tmp.next == order) {
        tmp.next = order.next;
        return;
      }
      tmp = tmp.next;
    }
  }

  public void doneOrder(int order) {
    jpvmSpawnWorkOrder tmp;
    if (list == null)
      return;
    if (order == list.order) {
      list = list.next;
      return;
    }
    tmp = list;
    while (tmp.next != null) {
      if (tmp.next.order == order) {
        tmp.next = tmp.next.next;
        return;
      }
      tmp = tmp.next;
    }
  }
}

class jpvmDaemonWaiter extends Thread {
  private Process process;

  jpvmDaemonWaiter(Process p) {
    process = p;
  }

  public void run() {
    boolean wait = true;
    while (wait) {
      try {
        process.waitFor();
        wait = false;
      } catch (InterruptedException ie) {
      }
    }
  }
}

class jpvmExecTaskThread extends Thread {
  private jpvmEnvironment jpvm = null;
  private Process process;
  private jpvmTaskId client;
  private String args[];
  private boolean log_on = true;
  private boolean debug_on = false;
  private jpvmCreateWorkOrder order;

  jpvmExecTaskThread(jpvmEnvironment j, jpvmTaskId c, String a[],
                     jpvmCreateWorkOrder o) {
    jpvm = j;
    client = c;
    args = a;
    order = o;
  }

  private boolean doExec() {
    try {
      if (jpvmDaemon.newVM)
        process = Runtime.getRuntime().exec(args);
      else
        factory(args[8]).execute(args);
    } catch (Exception ioe) {
      perror("i/o exception on exec");
      order.outstanding = false;
      jpvmBuffer buf = new jpvmBuffer();
      buf.pack(order.order);
      buf.pack(-1);
      ioe.printStackTrace();
      try {
        jpvm.pvm_send(buf, client,
                jpvmDaemonMessageTag.jpvmdCreatedTask);
      } catch (jpvmException jpe) {
        perror("CreateTask, \"" + jpe + " sending "
                + "to client " + client.toString());
        jpe.printStackTrace();
      }
      return false;
    }
    return true;
  }

  public void run() {
    boolean wait = doExec();
    if (jpvmDaemon.newVM)
      while (wait) {
        try {
          process.waitFor();
          wait = false;
        } catch (InterruptedException ie) {
        }
      }
  }

  private void log(String message) {
    if (log_on) {
      System.out.println("jpvm daemon: " + message);
      System.out.flush();
    }
  }

  private void perror(String message) {
    System.err.println("jpvm daemon: " + message);
    System.err.flush();
  }

  private static class PrototypeNotFound extends Exception {
  }

  private static class CannotCreateTask extends Exception {
  }

  private static Vector TaskTypes = new Vector(0, 1);

  public static jpvmObject factory(String classname)
          throws PrototypeNotFound, CannotCreateTask {
//		System.out.println("Creating " + classname);
    for (int i = 0; i < TaskTypes.size(); i++) {
      Class xc = (Class) TaskTypes.elementAt(i);
      if (xc.getName().indexOf(classname) != -1) {
        try {
          Constructor ctor = xc.getConstructor(
                  new Class[]{});
          return (jpvmObject) ctor.newInstance(
                  new Object[]{});
        } catch (Exception ex) {
//          System.out.println(classname);
          ex.printStackTrace();
          throw new CannotCreateTask();
        }
      }
    }
    try {
//			System.out.println("Loading " + classname);
      TaskTypes.addElement(Class.forName(classname));
    } catch (Exception ex) {
      ex.printStackTrace();
      throw new PrototypeNotFound();
    }
    return factory(classname);
  }

}

