/*
 * @(#)PersistentThread.java created July 13, 2006 Casalino
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

package it.unitn.ing.rista.util;

import it.unitn.ing.rista.awt.ProgressFrame;

import java.util.Vector;

/**
 * The PersistentThread is a class
 * <p/>
 * Usage

 int ip;
 final int maxThreads = Math.min(Constants.maxNumberOfThreads, numberOfReflex);
 PersistentThread[] threads = new PersistentThread[maxThreads];
 for (ip = 0; ip < maxThreads; ip++) {
   threads[ip] = new PersistentThread(ip) {
     public void executeJob() {
       int i1 = this.getJobNumberStart();
       int i2 = this.getJobNumberEnd();

       for (int i = i1; i < i2; i++) {
         Reflection refl = getReflex(i);
          // do something with reflex.....
       }
     }
   };
 }
 ip = 0;
 int istep = (int) (0.9999 + numberOfReflex / Constants.maxNumberOfThreads);
 for (int jp = 0; jp < maxThreads; jp++) {
   int isp = ip;
   if (jp < Constants.maxNumberOfThreads - 1)
     ip = Math.min(ip + istep, numberOfReflex);
   else
     ip = numberOfReflex;
   threads[jp].setJobRange(isp, ip);
   threads[jp].start();
 }
 do {
   running = false;
   try {
     Thread.sleep(Constants.timeToWaitThreadsEnding);
   } catch (InterruptedException r) {
   }
    for (int h = 0; h < maxThreads; h++) {
    if (!threads[h].isEnded())
      running = true;
    }
 } while (running);

 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/12/04 14:30:16 $
 * @since JDK1.1
 */

public class PersistentThread extends Thread {

  int jobNumberStart = -1;
  int jobNumberEnd = -1;
  public int threadNumber = -1;
  public Vector data = null;

  public static ProgressFrame threadsMonitor = null;
  public static int totalThreads = 0;
  public static String textPrefix = "Working threads number: ";
  public boolean started = false;
  private boolean ended = false;
  public static int normalPriority = MaudPreferences.getInteger("computation.priority",
      Constants.computationPriority);
  public static int pausePriority = MIN_PRIORITY;

  public PersistentThread(int number) {
    setPriority(normalPriority);
    threadNumber = number;
    started = false;
    ended = false;
  }

  public PersistentThread() {
    this(-1);
  }

  public int getJobNumberStart() {
    return jobNumberStart;
  }

  public int getJobNumberEnd() {
    return jobNumberEnd;
  }

  public void setJobRange(int jobStart, int jobEnd) {
    jobNumberStart = jobStart;
    jobNumberEnd = jobEnd;
  }

  static {
    startMonitor();
  }

  public static void startMonitor() {
    Constants.maxNumberOfThreads = MaudPreferences.getInteger("parallel_processing.threads_maxNumber",
        Constants.maxNumberOfThreads);
//    if (Constants.maxNumberOfThreads > 1)
//       Constants.warnAboutThreads();
    if (MaudPreferences.getBoolean("parallel_processing.showMonitor", false)) {
      threadsMonitor = new ProgressFrame(Constants.maxNumberOfThreads * 3 + 1);
      threadsMonitor.setTitle("threads monitor");
      threadsMonitor.setProgressText(textPrefix + totalThreads);
      threadsMonitor.setProgressBar(totalThreads);
//      threadsIncreased(); // the main running thread
    }
  }

  public void run() {
    threadsIncreased();
    started = true;
//    if (Constants.debugThreads)
//      System.out.println("Start thread " + threadNumber);
    executeJob();
    setEnded();
//    if (Constants.debugThreads)
//      System.out.println("End thread " + threadNumber);
    threadsDecreased();
  }

  public synchronized void setEnded() {
    ended = true;
  }

  public synchronized boolean isEnded() {
    return ended;
  }

  public void executeJob() {
  }

  private static void threadsIncreased() {
    totalThreads++;
    if (threadsMonitor != null) {
      threadsMonitor.setProgressText(textPrefix + totalThreads);
      threadsMonitor.increaseProgressBarValue();
    } else
      startMonitor();
  }

  private static void threadsDecreased() {
    totalThreads--;
    if (threadsMonitor != null) {
      threadsMonitor.setProgressText(textPrefix + totalThreads);
      threadsMonitor.decreaseProgressBarValue();
    } else
      startMonitor();
  }

}
