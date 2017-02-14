/*
 * @(#)ConvertTextImageFromColumnsToArea.java created Jun 26, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.awt.*;

import java.io.*;
import java.util.StringTokenizer;
import java.util.Vector;
import java.awt.*;

/**
 * The ConvertTextImageFromColumnsToArea is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 26, 2009 4:53:57 PM $
 * @since JDK1.1
 */

public class ConvertTextImageFromColumnsToArea {

  protected static String eol = System.getProperty("line.separator", "\n");

  public static void run(Frame parent) {
    String filename = Utility.openFileDialog(parent, "Open 3 columns txt image file", FileDialog.LOAD,
        MaudPreferences.getPref(principalJFrame.datafilePath, Constants.documentsDirectory),
        null, null);
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
      MaudPreferences.setPref(principalJFrame.datafilePath,
          folderAndName[0]);
      String string_1_ = folderAndName[0];
      String string_2_ = folderAndName[1];
      if (string_2_ != null) {
//        System.out.println("Opening: " + filename);
        File file = new File(filename);
        try {
          Vector <int[]> data = new Vector <> (1048576, 1048576);
          BufferedReader dis = new BufferedReader(new InputStreamReader(new FileInputStream(file)));
          String linedata = dis.readLine();

          int maxX = 0;
          int maxY = 0;
          int minX = 50000;
          int minY = 50000;

          String token1 = new String("");
          String token2 = new String("");
          int index = 0;
          while (linedata != null) {
//            if (++index == 1024) {
//              System.out.println(linedata);
//              index = 0;
//            }
            StringTokenizer st = new StringTokenizer(linedata, " ,\t\r\n");
            if (st.hasMoreTokens())
              token1 = st.nextToken();
            if (st.hasMoreTokens())
              token2 = st.nextToken();
            if (st.hasMoreTokens()) {
              int[] adata = new int[3];
              adata[0] = Integer.parseInt(token1);
              adata[1] = Integer.parseInt(token2);
              if (adata[0] > maxX)
                maxX = adata[0];
              if (adata[1] > maxY)
                maxY = adata[1];
              if (adata[0] < minX)
                minX = adata[0];
              if (adata[1] < minY)
                minY = adata[1];
              adata[2] = Integer.parseInt(st.nextToken());
              data.add(adata);
            }
            linedata = dis.readLine();
          }

          int dataNumberX = maxX - minX + 1;
          int dataNumberY = maxY - minY + 1;

          int[][] image = new int[dataNumberX][dataNumberY];
          for (int i = 0; i < data.size(); i++) {
            int[] elem = data.elementAt(i);
            image[elem[0]][elem[1]] = elem[2];
          }

          dis.close();

          index = string_2_.lastIndexOf(".");
          String tmp;
          if (index == -1) {
            tmp = string_2_ + "IJ";
          } else {
            tmp = string_2_.substring(0, index) + "IJ" + string_2_.substring(index, string_2_.length());
          }
	       filename = Utility.openFileDialog(parent, "Choose filename to save...", FileDialog.SAVE,
			        MaudPreferences.getPref(principalJFrame.datafilePath, ""),
			        null, string_1_ + tmp);
	        if (filename != null) {
		        file = new File(filename);
//          System.out.println("Opening: " + string_1_ + tmp);
		        BufferedWriter sis = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)));
//          index = 1;
		        for (int j = 0; j < dataNumberY; j++) {
			        for (int i = 0; i < dataNumberX; i++)
				        sis.write(Integer.toString(image[i][j]) + " ");
			        sis.write(eol);
//            System.out.println("Wrote line: " + index++);
		        }
		        sis.close();
	        }
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    }

}
}