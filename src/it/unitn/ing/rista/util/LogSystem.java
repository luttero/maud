/**
 * @(#)LogSystem.java created Feb 23, 2007 Trento
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is
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
 **/

package it.unitn.ing.rista.util;

import java.io.*;

/**
 * The LogSystem is a class to manage errors and log files
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.0 $, $Date: Feb 23, 2007 3:18:50 PM $
 * @since JDK1.1
 */

public class LogSystem {

  public static PrintStream logStream;
  public static boolean errorsToConsole = MaudPreferences.getBoolean("exceptions.showInConsole", true);
  public static boolean errorsToFile = MaudPreferences.getBoolean("exceptions.logInFile", true);
  static String logFileForErrors = MaudPreferences.getPref("exceptions.logfileName", "maudLogfile.txt");
  static boolean outputFilestreamOpened = false;

  public static void printStackTrace(Throwable e) {
    if (errorsToConsole)
      e.printStackTrace();
    if (errorsToFile) {
      if (!outputFilestreamOpened)
        logStream = openFilestream();
      if (logStream != null)
        e.printStackTrace(logStream);
    } else
      if (outputFilestreamOpened) {
        closeLog();
      }
  }

  public static void closeLog() {
    if (logStream != null)
      logStream.close();
    outputFilestreamOpened = false;
    logStream = null;
  }

  private static PrintStream openFilestream() {
    OutputStream out = null;
    try {
      out = new FileOutputStream(new File(Constants.logsDirectory + logFileForErrors));
    } catch (FileNotFoundException e) {
      if (errorsToConsole) {
        System.out.println("Not able to open file for errors log output: " +
            Constants.logsDirectory + logFileForErrors);
        e.printStackTrace();
      }
    }
    if (out != null) {
      logStream = new PrintStream(out);
      outputFilestreamOpened = true;
    }
    return logStream;
  }

  public static void printToDo(String s) {
    if (Constants.testing) {
      System.out.println("Feature not implemented (todo): " + s);
    }
  }
}
