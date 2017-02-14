/*
 * @(#)ZIPNLextractor.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io;

import com.apple.mrj.*;

import java.io.*;
import java.util.zip.*;
import java.awt.*;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.util.*;

/**
 * The ZIPNLextractor is a class
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class ZIPNLextractor {

  private ZipFile zip_file = null;
  private File newBaseDir = null;
  private SimpleConverter simple_converter = null;

  public ZIPNLextractor() {
  }

  public void zipExtractor() {

    File f = new File(System.getProperty("user.dir") +
            System.getProperty("file.separator") +
            "maudLib.sit");
    File f1 = new File(System.getProperty("user.dir") +
            System.getProperty("file.separator") +
            "maudLib");

    if (!f.exists() || f1.exists())
      return;

/*		simple_converter = new SimpleConverter();

		try
		{
			zip_file = new ZipFile(System.getProperty("user.dir") +
							System.getProperty("file.separator") +
							"maudlib.zip");
			newBaseDir = new File(System.getProperty("user.dir") +
							System.getProperty("file.separator"));
		}
		catch(ZipException e)
		{
			System.out.println(e);
		}
		catch(Exception e)
		{
		}


		Enumeration enum1 = zip_file.entries();
		while(enum1.hasMoreElements())
		{
			ZipEntry entry = (ZipEntry)(enum1.nextElement());
			processEntry(entry);
		}*/

    try {
      MRJFileUtils.setFileTypeAndCreator(f, new MRJOSType("adrp"),
              new MRJOSType("SITx"));
    } catch (Exception e) {
      System.out.println("Couldn't set the file type and creator");
    }

    boolean converted = launchMRJApplication("SITx", f);

    boolean secondconversion = false;
    if (!converted) {
      showFirstAlertDialog();
      secondconversion = launchMRJApplication(f);
    }

    if (!converted && !secondconversion) {
      showAlertDialog();

      // System.exit(0);
    }

    f1 = new File(System.getProperty("user.dir") +
            System.getProperty("file.separator") +
            "maudLib");
    int i = 0;
    while (!f1.exists() && i < 200) {
      try {
        Thread.currentThread().sleep(1000);
        i++;
      } catch (InterruptedException ie) {
      }
    }

    for (i = 0; i < 5; i++)
      try {
        Thread.currentThread().sleep(1000);
      } catch (InterruptedException ie) {
      }

    f1 = new File(System.getProperty("user.dir") +
            System.getProperty("file.separator") +
            "maudLib");

    if (f1.exists())
      f.delete();
    else {
      showAlertDialog();

      // System.exit(0);
    }

  }

  public void showAlertDialog() {
    (new AttentionD("Failed to unstuff maudLib.sit, try manually using Stuffit Expander. Exiting.....")).setVisible(true);
  }

  public void showFirstAlertDialog() {
    (new AttentionD("Failed to find Stuffit Expander, try to locate it manually")).setVisible(true);
  }

  public void processEntry(ZipEntry entry) {
    String org_path = entry.getName();

    File dest = new File(newBaseDir, org_path);
    new File(dest.getParent()).mkdirs();
    try {
      simple_converter.convert(zip_file.getInputStream(entry), new FileOutputStream(dest));
      try {
        MRJFileUtils.setFileTypeAndCreator(dest, new MRJOSType("shlb"),
                new MRJOSType("????"));
      } catch (Exception e) {
        System.out.println("Couldn't set the file type and creator");
        return;
      }
    } catch (IOException e) {
    }
  }

  public static boolean launchMRJApplication(String AppCreator, File f) {
    try {
      File targetApp = MRJFileUtils.findApplication(new MRJOSType(AppCreator));

      // launch SimpleText with our new text file
      String[] params = {targetApp.toString(), f.toString()};

      // parameters to runtime exec:
      // { AppToRun, FileToOpen, ... }
      Runtime.getRuntime().exec(params);

    } catch (Exception e) {
      return false;
    }
    return true;
  }

  public static boolean launchMRJApplication(File f) {
    String targetApp = null;
//			ExtensionFilter filefilter = new ExtensionFilter(".par");
    String filename = Utility.openFileDialog(new Frame(), "Locate Stuffit Expander or Stuffit", FileDialog.LOAD,
            "",
            null, null);
    if (filename != null) {
      targetApp = new String(filename);
    }
    try {
      // launch SimpleText with our new text file
      String[] params = {targetApp.toString(), f.toString()};

      // parameters to runtime exec:
      // { AppToRun, FileToOpen, ... }
      Runtime.getRuntime().exec(params);

    } catch (Exception e) {
      return false;
    }
    return true;
  }

  class SimpleConverter {
    private byte[] buffer;

    SimpleConverter() {
      buffer = new byte[8 * 1024];
    }

    public void convert(InputStream in, OutputStream out) throws IOException {
      try {
        int count;
        while ((count = in.read(buffer)) != -1)
          out.write(buffer, 0, count);
      } catch (IOException e) {
        throw e;
      } finally {
        in.close();
        out.close();
      }
    }
  }

}
