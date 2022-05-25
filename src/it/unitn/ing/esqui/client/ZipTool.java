package it.unitn.ing.esqui.client;

import java.io.*;
import java.util.*;
import java.util.zip.*;
import java.text.*;
import java.awt.*;
import javax.swing.*;

/** ZipTools.java
 * <br>
 * Title:			<b>ESQUI Zip Tool</b>
 * <br>
 * Description:	Zip tools for the ESQUI client
 * @author:			Leonardo Cont, December 2000
 * @revision:		February 2001
 * @comment:		none
 */

class ZipTool {

  private JFrame parentFrame = null;
  private ProgressWindow unzipStatusWindow = null;
  private int bytesRead = 0;
  private int mark = 0;

  public ZipTool(JFrame parentFrame) {
    this.parentFrame = parentFrame;
  }

  public void unzipFile(File fileToUnzip, File finalFile,
                        int finalFileSize) throws IOException {
    unzipStatusWindow = new ProgressWindow(parentFrame, "Unzipping analysis data...please wait...", finalFileSize);
    InputStream in = new BufferedInputStream(new FileInputStream(fileToUnzip));
    ZipInputStream zipIn = new ZipInputStream(in);
    ZipEntry entry = null;
    while ((entry = zipIn.getNextEntry()) != null) {
      unzip(zipIn, finalFile);
      zipIn.closeEntry();
      entry = zipIn.getNextEntry();
      mark = 0;
      unzipStatusWindow.resetBar();
    }
    zipIn.close();
    unzipStatusWindow.setVisible(false);
    unzipStatusWindow.dispose();
  }

  void unzip(ZipInputStream zipIn, File finalFile)
          throws IOException {
    FileOutputStream zipOut = new FileOutputStream(finalFile);
    byte[] b = new byte[512];
    while ((bytesRead = zipIn.read(b)) != -1) {
      mark += bytesRead;
      zipOut.write(b, 0, bytesRead);
      unzipStatusWindow.setBarValue(mark);
    }
    zipOut.close();
  }
}
