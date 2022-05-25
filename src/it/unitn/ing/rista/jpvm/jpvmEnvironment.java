/* jpvmEnvironment.java
 *
 * The jpvmEnvironment class implements the jpvm run time environment
 * of a task enrolled in the jpvm parallel virtual machine.
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

import java.io.*;

/**
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */

public class jpvmEnvironment {
  public final jpvmTaskId PvmNoParent = null;

  private jpvmTaskId myTid;
  private jpvmTaskId parentTid;
  private jpvmTaskId daemonTid;
  private jpvmMessageQueue myMessageQueue;
  private jpvmConnectionSet myConnectionSet;
//  private static jpvmConnectionServer connectionServer;
//  private static String myName;
  private int registrationNumber = -1;
  public static final int defaultPort = 49500;

  public jpvmEnvironment() throws jpvmException {
    init(false, null);
  }

  public jpvmEnvironment(String taskName) throws jpvmException {
    init(false, taskName);
  }

  public jpvmEnvironment(boolean isDaemon) throws jpvmException {
    init(isDaemon, null);
  }

  public jpvmEnvironment(boolean isDaemon, String taskName)
      throws jpvmException {
    init(isDaemon, taskName);
  }

  public jpvmTaskId pvm_mytid() {
    return myTid;
  }

  public jpvmTaskId pvm_parent() {
    return parentTid;
  }

  public int pvm_spawn(String task_name, int num, jpvmTaskId tids[])
      throws jpvmException {
    int ret = 0;
    jpvmBuffer buf = new jpvmBuffer();
    buf.pack(num);
    buf.pack(task_name);
    pvm_send(buf, daemonTid, jpvmDaemonMessageTag.jpvmdSpawnTask);
    try {
      jpvmMessage m = pvm_recv(jpvmDaemonMessageTag.jpvmdSpawnTask);
      ret = m.buffer.upkint();
      m.buffer.unpack(tids, ret, 1);
    }
    catch (jpvmException jpe) {
      jpvmDebug.error("pvm_spawn, internal error");
    }
    return ret;
  }

  public synchronized void pvm_send(jpvmBuffer buf, jpvmTaskId tid,
                                    int tag) throws jpvmException {
    jpvmDebug.note("pvm_send, sending message to " + tid.toString());
    jpvmMessage message = new jpvmMessage(buf, tid, myTid, tag);
    if (myTid.equals(tid)) {
      // Just enqueue the message
      myMessageQueue.enqueue(message);
    } else {
      jpvmSendConnection conn = getConnection(tid);
      message.send(conn);
    }
  }

  public synchronized void pvm_mcast(jpvmBuffer buf, jpvmTaskId tids[],
                                     int ntids, int tag) throws jpvmException {
    int exceptions = 0;
    jpvmMessage message = new jpvmMessage(buf, null, myTid, tag);
    for (int i = 0; i < ntids; i++) {
      jpvmTaskId tid = tids[i];
      message.destTid = tid;
      jpvmDebug.note("pvm_mcast, sending message to " +
          tid.toString());
      try {
        jpvmSendConnection conn = getConnection(tid);
        message.send(conn);
      }
      catch (jpvmException jpe) {
        exceptions++;
      }
    }
    if (exceptions > 0)
      throw new jpvmException("pvm_mcast, some messages " +
          "failed");
  }

  public jpvmMessage pvm_recv(jpvmTaskId tid, int tag)
      throws jpvmException {
    //
    // Thanks to Professor Thomas R. James of the Dept. of
    // Mathematical Sciences at Otterbein College for finding
    // and fixing race condition in the previous implementation
    // of pvm_recv.
    //
    // Adam Ferrari - Mon Feb  1 13:11:12 EST 1999
    //
    return myMessageQueue.dequeue(tid, tag);
  }

  public jpvmMessage pvm_recv(jpvmTaskId tid) throws jpvmException {
    return myMessageQueue.dequeue(tid);
  }

  public jpvmMessage pvm_recv(int tag) throws jpvmException {
    return myMessageQueue.dequeue(tag);
  }

  public jpvmMessage pvm_recv() throws jpvmException {
    return myMessageQueue.dequeue();
  }

  public jpvmMessage pvm_nrecv(jpvmTaskId tid, int tag)
      throws jpvmException {
    return myMessageQueue.dequeueNonBlock(tid, tag);
  }

  public jpvmMessage pvm_nrecv(jpvmTaskId tid) throws jpvmException {
    return myMessageQueue.dequeueNonBlock(tid);
  }

  public jpvmMessage pvm_nrecv(int tag) throws jpvmException {
    return myMessageQueue.dequeueNonBlock(tag);
  }

  public jpvmMessage pvm_nrecv() throws jpvmException {
    return myMessageQueue.dequeueNonBlock();
  }

  public boolean pvm_probe(jpvmTaskId tid, int tag) throws jpvmException {
    return myMessageQueue.probe(tid, tag);
  }

  public boolean pvm_probe(jpvmTaskId tid) throws jpvmException {
    return myMessageQueue.probe(tid);
  }

  public boolean pvm_probe(int tag) throws jpvmException {
    return myMessageQueue.probe(tag);
  }

  public boolean pvm_probe() throws jpvmException {
    return myMessageQueue.probe();
  }

  public void pvm_exit() throws jpvmException {
    jpvmBuffer buf = new jpvmBuffer();
    try {
      Thread.sleep(1000);
    }
    catch (InterruptedException ie) {
    }
    pvm_send(buf, daemonTid, jpvmDaemonMessageTag.jpvmdDeleteTask);
  }

  public jpvmConfiguration pvm_config() {
    jpvmBuffer buf = new jpvmBuffer();
    jpvmConfiguration ret = null;
    try {
      pvm_send(buf, daemonTid, jpvmDaemonMessageTag.jpvmdHostStatus);
      jpvmMessage m = pvm_recv(jpvmDaemonMessageTag.jpvmdHostStatus);
      int n = m.buffer.upkint();
      ret = new jpvmConfiguration(n);
      for (int i = 0; i < n; i++)
        ret.hostNames[i] = m.buffer.upkstr();
      m.buffer.unpack(ret.hostDaemonTids, n, 1);
    }
    catch (jpvmException jpe) {
      jpvmDebug.error("pvm_config, internal error");
    }
    return ret;
  }

  public jpvmTaskStatus pvm_tasks(jpvmConfiguration conf, int which) {
    jpvmTaskStatus ret = null;
    if (conf == null || which < 0 || which >= conf.numHosts)
      return null;

    try {
      jpvmBuffer buf = new jpvmBuffer();
      pvm_send(buf, conf.hostDaemonTids[which],
          jpvmDaemonMessageTag.jpvmdTaskStatus);
      jpvmMessage m = pvm_recv(jpvmDaemonMessageTag.jpvmdTaskStatus);
      ret = new jpvmTaskStatus();
      ret.hostName = conf.hostNames[which];
      ret.numTasks = m.buffer.upkint();
      if (ret.numTasks == 0) {
        ret.taskNames = null;
        ret.taskTids = null;
        return ret;
      }
      ret.taskNames = new String[ret.numTasks];
      ret.taskTids = new jpvmTaskId[ret.numTasks];
      for (int i = 0; i < ret.numTasks; i++)
        ret.taskNames[i] = m.buffer.upkstr();
      m.buffer.unpack(ret.taskTids, ret.numTasks, 1);
    }
    catch (jpvmException jpe) {
      jpvmDebug.error("pvm_tasks, internal error");
    }
    return ret;
  }

  public void pvm_halt() throws jpvmException {
    jpvmBuffer buf = new jpvmBuffer();
    pvm_send(buf, daemonTid, jpvmDaemonMessageTag.jpvmdHalt);
  }

  public void pvm_addhosts(int nhosts, String hostnames[],
                           jpvmTaskId daemonTids[]) throws jpvmException {
    jpvmBuffer buf = new jpvmBuffer();
    buf.pack(nhosts);
    for (int i = 0; i < nhosts; i++)
      buf.pack(hostnames[i]);
    buf.pack(daemonTids, nhosts, 1);
    pvm_send(buf, daemonTid, jpvmDaemonMessageTag.jpvmdAddHost);
  }

  // Internal methods:
  private void init(boolean isDaemon, String taskName)
      throws jpvmException {
    int aport = defaultPort;
    if (!isDaemon)
      aport++;
    myMessageQueue = new jpvmMessageQueue();
    myConnectionSet = new jpvmConnectionSet();
    jpvmConnectionServer connectionServer = new jpvmConnectionServer(myConnectionSet,
        myMessageQueue, aport);
    myTid = new jpvmTaskId(connectionServer.getConnectionPort());
//    System.out.println("new jpvmEnvironment " + this + " " + myTid.getPort() + " " + isDaemon);
    connectionServer.setDaemon(true);
    connectionServer.start();
    if (!isDaemon) {
      findDaemon();
      findParent();
      registerDaemon(taskName);
    }
  }

  private jpvmTaskId initTaskId() {
    jpvmTaskId ret = null;
    return ret;
  }

  private jpvmSendConnection getConnection(jpvmTaskId tid)
      throws jpvmException {
    jpvmSendConnection ret = null;
    ret = myConnectionSet.lookupSendConnection(tid);
    if (ret != null) {
      // Had a cached connection...
      return ret;
    }
    // Must establish a new connection.
    ret = jpvmSendConnection.connect(tid, myTid);
    if (ret != null) {
      myConnectionSet.insertSendConnection(ret);
      return ret;
    }
    throw new jpvmException("getConnection, connect failed");
  }

  private void findDaemon() {
    int daemonPort = -1;
    String daemonPortStr = System.getProperty("jpvm.daemon");
    if (daemonPortStr != null) {
      try {
        daemonPort = Integer.valueOf(daemonPortStr).intValue();
      }
      catch (NumberFormatException nfe) {
        jpvmDebug.error("couldn't bind to daemon, " +
            "jpvm.daemon not an integer");
        daemonPort = -1;
      }
    } else {
      daemonPort = readDaemonFile();
    }
    if (daemonPort == -1) {
      jpvmDebug.error("couldn't bind to daemon, " +
          "jpvm.daemon not defined");
    }
    daemonTid = new jpvmTaskId(daemonPort);
  }

  private void findParent() throws jpvmException {
    String parentHost = System.getProperty("jpvm.parhost");
    int parentPort = 0;
    if (parentHost == null) {
      parentTid = null;
      return;
    }
    String parentPortStr = System.getProperty("jpvm.parport");
    try {
      parentPort = Integer.valueOf(parentPortStr).intValue();
    }
    catch (NumberFormatException nfe) {
      jpvmDebug.error("couldn't bind to parent, " +
          "jpvm.parport not an integer");
    }
    parentTid = new jpvmTaskId(parentHost, parentPort);

    // Since we have a parent, register with the daemon
    String regStr = System.getProperty("jpvm.regnum");
    if (regStr == null) {
      jpvmDebug.error("no task registration number");
    } else {
      try {
        registrationNumber =
            Integer.valueOf(regStr).intValue();
      }
      catch (NumberFormatException nfe) {
        jpvmDebug.error("invalid task registration number");
      }
    }
    jpvmBuffer buf = new jpvmBuffer();
    buf.pack(registrationNumber);
    pvm_send(buf, daemonTid, jpvmDaemonMessageTag.jpvmdRegisterChild);
  }

  private void registerDaemon(String taskName) throws jpvmException {
    // Find out the name of this task
    String myName;
    if (taskName == null) {
      myName = System.getProperty("jpvm.taskname");
      if (myName == null) myName = "(command line jpvm task)";
    } else {
      myName = new String(taskName);
    }

    // Register this task with the daemon
    jpvmBuffer buf = new jpvmBuffer();
    buf.pack(myName);
    pvm_send(buf, daemonTid, jpvmDaemonMessageTag.jpvmdRegisterTask);
  }

  private int readDaemonFile() {
    int port = -1;
    String fileName = pvm_daemon_file_name();
    try {
      File f = new File(fileName);
      FileInputStream fin = new FileInputStream(f);
      DataInputStream din = new DataInputStream(fin);
      port = din.readInt();
    }
    catch (IOException ioe) {
      jpvmDebug.error("error writing \"" + fileName + "\"");
      port = -1;
    }
    return port;
  }

  public static String pvm_daemon_file_name() {
    String osName = System.getProperty("os.name");
    String userName = System.getProperty("user.name");
    String fileName = null;
    if (osName.startsWith("Windows")) {
      fileName = "c:\\temp\\jpvmd-" + userName + ".txt";
    } else {
      fileName = "/tmp/jpvmd." + userName;
    }
    return fileName;
  }
}

