/*
 * @(#)Sla33Constants.java created 05/10/2001 Mesiano
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rsa;

import java.io.*;
import java.util.*;

import it.unitn.ing.rista.util.*;

/**
 *  The Sla33Constants is a class to store data for the moment pole method
 *  of Siegfried Matthies.
 *
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:48 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class Sla33Constants {

  public static double[] cwww = null;
  public static int[] iadd = null;

  public static final String cwwArraryFile = "files/CWWARRAY.DAT";
  public static final String iadArraryFile = "files/IADARRAY.DAT";

  public static final void initConstants() {
    cwww = new double[9885];
    iadd = new int[9885];

    BufferedReader in = Misc.getResourceReader(Constants.maudJar, cwwArraryFile);
    String token;
    if (in != null) {
      try {
        int index = 0;
        String line = in.readLine();
        while (line != null) {
          StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            cwww[index++] = Double.valueOf(token).doubleValue();
          }
          line = in.readLine();
        }
        in.close();
      } catch (IOException ie) {
        try {
          in.close();
        } catch (IOException iex) {
        }
      }
    }

    in = Misc.getResourceReader(Constants.maudJar, iadArraryFile);
    if (in != null) {
      try {
        int index = 0;
        String line = in.readLine();
        while (line != null) {
          StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
          while (st.hasMoreTokens()) {
            token = st.nextToken();
            iadd[index++] = Integer.valueOf(token).intValue();
          }
          line = in.readLine();
        }
        in.close();
      } catch (IOException ie) {
        try {
          in.close();
        } catch (IOException iex) {
        }
      }
    }

  }

}
