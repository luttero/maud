/*
 * @(#)Misc.java created 16/03/1999 Firenze-Trento
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
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
import it.unitn.ing.rista.diffr.Phase;
import it.unitn.ing.rista.io.StringNumber;

import java.io.*;
import java.net.URL;
import java.util.*;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;


/**
 * The Misc is an utility class to perform various operations like
 * converting paths, opening/closing files or checking class compatibility.
 *
 * @version $Revision: 1.16 $, $Date: 2006/12/04 14:30:15 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Misc {

  private Misc() {
  }

  public static final boolean areClassCompatibles(String superclass, String subclass) {
//		System.out.println("Comparing: " + superclass + " " + subclass);
    try {
      Class sup = Constants.maudClassLoader.loadClass(superclass);
      Class sub = Constants.maudClassLoader.loadClass(subclass);
      return sup.isAssignableFrom(sub);
    } catch (ClassNotFoundException e) {
    }
    return false;
  }

  public static final boolean areClassCompatibles(String superclass, Class subclass) {
//		System.out.println("Comparing: " + superclass + " " + subclass);
    try {
      Class sup = Constants.maudClassLoader.loadClass(superclass);
      return sup.isAssignableFrom(subclass);
    } catch (ClassNotFoundException e) {
    }
    return false;
  }

  public static final String checkNotPermittedChars(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    for (int i = 0; i < astring.length(); i++)
      if (astring.charAt(i) != ' ' && astring.charAt(i) != '/' && astring.charAt(i) != '\\' &&
              astring.charAt(i) != ':' && astring.charAt(i) != '?' && astring.charAt(i) != '*' &&
              astring.charAt(i) != '<' && astring.charAt(i) != '>' && astring.charAt(i) != '|' &&
              astring.charAt(i) != '"')
        tempstring.append(astring.charAt(i));
    return tempstring.toString();
  }

	public static final String toStringStripLeadingTrailingBlankAndTab(String astring) {
		if (astring == null || astring.length() == 0)
			return astring;
		int starting = 0;
		while (starting < astring.length() &&
				(astring.charAt(starting) == ' ' || astring.charAt(starting) == '\t'))
			starting++;
		int length = starting;
		for (int i = starting; i < astring.length(); i++)
			if (astring.charAt(starting) != ' ' && astring.charAt(starting) != '\t')
				length = i;
		if (length + 1 > astring.length())
			return astring;
		return astring.substring(starting, length + 1);
	}

  public static final String toStringDeleteBlank(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    for (int i = 0; i < astring.length(); i++)
      if (astring.charAt(i) != ' ')
        tempstring.append(astring.charAt(i));
    return tempstring.toString();
  }

  public static final String toStringDeleteBlankAndTab(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    for (int i = 0; i < astring.length(); i++)
      if (astring.charAt(i) != ' ' && astring.charAt(i) != '\t')
        tempstring.append(astring.charAt(i));
    return tempstring.toString();
  }

	public static final String toStringDeleteBlankTabAndEOF(String astring) {
		StringBuffer tempstring = new StringBuffer("");
		for (int i = 0; i < astring.length(); i++)
			if (astring.charAt(i) != ' ' && astring.charAt(i) != '\t' && astring.charAt(i) != '\r' && astring.charAt(i) != '\n')
				tempstring.append(astring.charAt(i));
		return tempstring.toString();
	}

	public static final String toStringNoBlank(String astring) {
    StringBuffer tempstring = new StringBuffer(astring);
    for (int i = 0; i < tempstring.length(); i++)
      if (tempstring.charAt(i) == ' ')
        tempstring.setCharAt(i, '~');
    return tempstring.toString();
  }

  public static final String toStringRecoverBlank(String astring) {
    StringBuffer tempstring = new StringBuffer(astring);
    for (int i = 0; i < tempstring.length(); i++)
      if (tempstring.charAt(i) == '~')
        tempstring.setCharAt(i, ' ');
    return tempstring.toString();
  }

  public static final String toStringChangeChar(String astring, char oldc, char newc) {
    StringBuffer tempstring = new StringBuffer(astring);
    for (int i = 0; i < tempstring.length(); i++)
      if (tempstring.charAt(i) == oldc)
        tempstring.setCharAt(i, newc);
    return tempstring.toString();
  }

  public static final String toStringDeleteSign(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    for (int i = 0; i < astring.length(); i++)
      if (!StringNumber.isminusplus(astring.charAt(i)))
        tempstring.append(astring.charAt(i));
    return tempstring.toString();
  }

  public static final String toStringDeleteDigits(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    for (int i = 0; i < astring.length(); i++)
      if (!StringNumber.isANumberOrSign(astring.charAt(i)))
        tempstring.append(astring.charAt(i));
    return tempstring.toString();
  }

  public static final String toStringDeleteFirstDigits(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    boolean firstDone = false;
    for (int i = 0; i < astring.length(); i++) {
      if (firstDone || !StringNumber.isANumberOrSign(astring.charAt(i)))
        tempstring.append(astring.charAt(i));
      if (!firstDone && !StringNumber.isANumberOrSign(astring.charAt(i)))
        firstDone = true;
    }
    return tempstring.toString();
  }

  public static final String toStringDeleteFinalDigits(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    boolean firstDone = false;
    for (int i = 0; i < astring.length(); i++) {
      if (!firstDone || !StringNumber.isANumberOrSign(astring.charAt(i)))
        tempstring.append(astring.charAt(i));
      if (!firstDone && !StringNumber.isANumberOrSign(astring.charAt(i)))
        firstDone = true;
    }
    return tempstring.toString();
  }

  public static final String toStringOnlyDigits(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    for (int i = 0; i < astring.length(); i++)
      if (StringNumber.isANumberOrSign(astring.charAt(i)))
        tempstring.append(astring.charAt(i));
    return tempstring.toString();
  }

  public static final String toStringFirstOnlyDigits(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    for (int i = 0; i < astring.length(); i++)
      if (StringNumber.isANumberOrSign(astring.charAt(i)))
        tempstring.append(astring.charAt(i));
      else
        break;
    return tempstring.toString();
  }

  public static final String toStringFinalOnlyDigits(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    int i = 0;
    while ((i < astring.length()) && !StringNumber.isANumberOrSign(astring.charAt(i)))
      i++;
    for (; i < astring.length(); i++)
      if (StringNumber.isANumberOrSign(astring.charAt(i)))
        tempstring.append(astring.charAt(i));
      else
        break;
    return tempstring.toString();
  }

  public static final String toStringDeleteDot(String astring) {
    StringBuffer tempstring = new StringBuffer("");
    for (int i = 0; i < astring.length(); i++)
      if (astring.charAt(i) != '.')
        tempstring.append(astring.charAt(i));
    return tempstring.toString();
  }

	public static String getFormattedValue(double valueD) {
		float value = (float)valueD;
		return Float.toString(value);
	}

	public static final String getCurrentDateTime() {
    return (new Date()).toString();
  }

  public static int getMinute() {
    return Calendar.getInstance().get(Calendar.MINUTE);
  }

  public static boolean isThirdMinute() {
    return (0 == MoreMath.is3Neven(getMinute()));
  }

	public static final String getFilenameNoParAndVersionExtension(String name) {
/*		if (name.endsWith(".par")) {
			String newName = name.substring(0, name.length() - 4);
			if (newName.endsWith("v") && Character.isDigit(newName.charAt(newName.length() - 2))) {
				newName = newName.substring(0, newName.length() - 1);
				while (Character.isDigit(newName.charAt(newName.length() - 1))) {
					newName = newName.substring(0, newName.length() - 1);
				}
				if (name.endsWith("_"))
					newName = newName.substring(0, newName.length() - 1);
			}
			return newName;
		}*/
		return name;
	}

	public static final String filterFileName(String filename) {
    if (filename == null || filename.equals(""))
      return filename;
    StringBuffer tmp = new StringBuffer(filename);
    for (int i = 0; i < tmp.length(); i++) {
      if (tmp.charAt(i) == '\\')
        tmp.setCharAt(i, '/');
    }
    String startpath = "";
    String astring = tmp.toString();
    if (!astring.startsWith("//") && astring.startsWith("/"))
      startpath = Constants.startPath;
    else if (Constants.windoze && (astring.length() > 1 && astring.charAt(1) == ':'))
      startpath = Constants.startPath;
    return new String(startpath + astring);
  }

  /**
   * Return the relative path between two folders.
   * @param fromFolder the relative path start from here and arrive at
   * @param toFolder the end of the relative path.
   * The two paths must be absolute paths with the "/" separator.
   */

  public static final String getRelativePath(String fromFolder, String toFolder) {
    StringBuffer relativePath = new StringBuffer("");
    try {
    int minLength = Math.min(fromFolder.length(), toFolder.length());
    int i = 0;
    if (minLength > 0) {
      for (; i < minLength; i++) {
//	      System.out.println(i + " " + fromFolder.charAt(i) + " =? " + toFolder.charAt(i));
        if (fromFolder.charAt(i) != toFolder.charAt(i)) {
          i--;
          break;
        }
      }
//	    System.out.println(i + ", break");
      if (i >= minLength)
        i = minLength - 1;
      for (; i > 0; i--) {
        if (fromFolder.charAt(i) == '/') {
          i++;
          break;
        }
      }
    }
    for (int j = fromFolder.length() - 1; j >= i; j--) {
      if (fromFolder.charAt(j) == '/')
        relativePath.append("../");
    }
    if (i < toFolder.length())
      relativePath.append(toFolder.substring(i));
    } catch (Exception e) {
      System.out.println(fromFolder + " | " + toFolder);
      e.printStackTrace();
    }
//    System.out.println(relativePath);
    return relativePath.toString();
  }

  /**
   * Convert the relative path between two folders in absolute path.
   * @param relativePath the relative path from the reference Folder
   * @param referenceFolder the absolute path of the reference Folder.
   * The two paths must use the "/" separator.
   */

  public static final String getAbsolutePath(String relativePath, String referenceFolder) {

//    System.out.println("abs :" + relativePath + " | " + referenceFolder);
    if (referenceFolder.equalsIgnoreCase(""))
      referenceFolder = getUserDir() + "/";
    int index = 0;
//    System.out.println("abs :" + relativePath + " | " + referenceFolder);
    int i = 0;
    for (; i < relativePath.length() - 2; i += 3) {
      if (relativePath.substring(i, i + 3).equals("../"))
        index++;
      else
        break;
    }
    int j = referenceFolder.length() - 1;
    for (; j >= 0 && index >= 0; j--) {
      if (referenceFolder.charAt(j) == '/')
        index--;
    }

    StringBuffer absolutePath = new StringBuffer(referenceFolder.substring(0, j + 2));
    absolutePath.append(relativePath.substring(i));

//    System.out.println("abs :" + relativePath + " | " + referenceFolder);
    return absolutePath.toString();
  }

  public static final String[] getFolderandName(String name) {
    name = filterFileName(name);
    String[] filename = new String[2];

    int index = name.lastIndexOf("/");
    if (index >= name.length() - 2 || index < 0) {
      filename[0] = "";
      filename[1] = name;
    } else {
      filename[0] = name.substring(0, index + 1);
      filename[1] = name.substring(index + 1, name.length());
    }
    return filename;
  }

  public static final String getUserDir() {
    return filterFileName(System.getProperty("user.dir"));
  }

  public static final String getCorrectFilename(String filename) {
    return filename;
  }

  public static final String checkForWindowsPath(String path) {
    if (!Constants.windoze)
      return path;
    StringBuffer tmp = new StringBuffer("");
    if (path.startsWith("//"))
      tmp.append(path.substring(2, path.length()));
    else
      tmp.append(path);
    for (int i = 0; i < tmp.length(); i++) {
      if (tmp.charAt(i) == '/')
        tmp.setCharAt(i, '\\');
    }
    return tmp.toString();
  }

/*	public static java.net.URL getFilesResource(String name) {
//  	name = "/" + name;
//  System.out.println("get file: " + name);
//	  System.out.println("get file: " + Constants.filesfolder + name);

		if (Misc.checkForFile(Constants.filesfolder + name)) {
			try {
				File file = new File(Constants.filesfolder + name);
				return file.toURL();
			} catch (MalformedURLException e) {
				e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
			}
		}
		return getResourceURL(Constants.filesJar, "files/" + name);
	}*/

	public static java.net.URL getResourceURL(String jarFile, String name) {
		Misc obj = new Misc();
		if (obj != null) {
			try {
//			System.out.println("Reading from jar as URL: " + jarFile + ", entry: " + name);
				java.net.URL url = obj.getClass().getResource("/" + name);
//			java.net.URL url = new java.net.URL(jarFile + "!/" + name);
			return url;
		} catch (Exception e) {
			e.printStackTrace();
		}
		}
		return null;
	}

	public static final BufferedReader getResourceReader(String jarFile, String filename) {
		if (filename == null || filename.equalsIgnoreCase("null"))
			return null;
		try {
			System.out.println("Reading from jar file: " + jarFile + ", entry(" + filename + ")");
			JarFile jar = new JarFile(jarFile);
			ZipEntry entry = jar.getEntry(filename);
			return new BufferedReader(new InputStreamReader(jar.getInputStream(entry)));
		} catch (IOException e) {
			e.printStackTrace();
		}
		return getReader(new File(filename));
	}

	public static final BufferedReader getReader(String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    BufferedReader aReader = getReader("", filename);
    if (aReader == null)
      aReader = getReader("./", filename);
    return aReader;
  }

	public static final BufferedReader getReader(String folder, String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    filename = getCorrectFilename(folder + filename);
    try {
      return getReader(new File(new java.net.URL(filename).getFile()));
    } catch (IOException e) {
//      System.out.println("Trying to open: " + filename);
//      e.printStackTrace(System.out);
    }
    return getReader(new File(filename));
  }

  public static final BufferedReader getReader(File file) {
    if (file == null)
      return null;
    BufferedReader in = null;
    try {
      in = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF8"));
    } catch (IOException e) {
      if (file.getName().endsWith("default.par"))
        in = Misc.getResourceReader(Constants.maudJar, "files/default.par");
      else if (file.getName().endsWith("marker.txt"))
		    in = Misc.getResourceReader(Constants.maudJar, "files/marker.txt");
	    else {
	      // Manual location
	      if (MaudPreferences.getBoolean("fileNotFound.selectManually", false)) {
		      System.out.println("File not loaded, select it manually: " + file.getAbsolutePath());
		      String filename = Utility.openFileDialogForLoad("File not loaded, select it manually", "", file.getAbsolutePath());
		      if (filename != null) {
			      try {
				      in = new BufferedReader(new InputStreamReader(new FileInputStream(
						      new File(filename)), "UTF8"));
			      } catch (IOException ie) {
				      System.out.println("File not loaded: " + filename);
				      ie.printStackTrace();
			      }
		      } else {
			      System.out.println("File not found: " + file.getName());
			      e.printStackTrace();
		      }
	      }
      }

    }
//	  }
    return in;
  }

  public static final InputStream getInputStream(String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    return getInputStream("", filename);
  }

  public static final InputStream getInputStream(String folder, String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    InputStream in = null;
    filename = getCorrectFilename(folder + filename);
    try {
      in = new FileInputStream(new File(RelativeURL.getURL(filename).getFile()));
    } catch (IOException e) {
      try {
        in = new FileInputStream(new File(filename));
      } catch (IOException ie) {
        in = null;
        System.out.println("File not found: " + filename);
        ie.printStackTrace();
      }
    }
//	  System.out.println("Filename to load: " + filename);
	  if (in == null) {
//		  System.out.println("Asking for a filename: " + filename);
		  System.out.println("File not loaded, select it manually: " + filename);
		  filename = Utility.openFileDialogForLoad("File not loaded, select it manually", "", filename);
		  if (filename != null) {
			  try {
				  in = new FileInputStream(new File(filename));
			  } catch (IOException ie) {
				  System.out.println("File not loaded: " + filename);
				  ie.printStackTrace();
			  }
		  }
	  }
//	  }
    return in;
  }

	public static final InputStream getInputStreamNoChance(String folder, String filename) {
		if (filename == null || filename.equalsIgnoreCase("null"))
			return null;
		InputStream in = null;
		filename = getCorrectFilename(folder + filename);
		try {
			in = new FileInputStream(new File(RelativeURL.getURL(filename).getFile()));
		} catch (IOException e) {
			try {
				in = new FileInputStream(new File(filename));
			} catch (IOException ie) {
				in = null;
				System.out.println("File not found: " + filename);
				ie.printStackTrace();
			}
		}
		return in;
	}

	public static final OutputStream getOutputStream(String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    return getOutputStream("", filename);
  }

  public static final OutputStream getOutputStream(String folder, String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    OutputStream in = null;
    filename = getCorrectFilename(folder + filename);
    try {
      in = new FileOutputStream(new File(RelativeURL.getURL(filename).getFile()));
    } catch (IOException e) {
      // e.printStackTrace();
      try {
        in = new FileOutputStream(new File(filename));
      } catch (IOException ie) {
        in = null;
        System.out.println("File not found: " + filename);
        ie.printStackTrace();
      }
    }
    return in;
  }

  public static final BufferedWriter getWriter(String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    return getWriter("", filename);
  }

  public static final BufferedWriter getWriter(String folder, String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    BufferedWriter out = null;
    filename = getCorrectFilename(folder + filename);
//		if (!it.unitn.ing.rista.MaudApplet.fromApplet) {
    try {
      out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(RelativeURL.getURL(filename).getFile()), "UTF8"));
    } catch (IOException e) {
      // e.printStackTrace();
      try {
        out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename), "UTF8"));
      } catch (IOException ie) {
        System.out.println("Unable to open file: " + filename);
        ie.printStackTrace();
      }
    }
//	  }
    return out;
  }

  public static final BufferedWriter getWriterForAppend(String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    return getWriterForAppend("", filename);
  }

  public static final BufferedWriter getWriterForAppend(String folder, String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    BufferedWriter out = null;
    filename = getCorrectFilename(folder + filename);
//		if (!it.unitn.ing.rista.MaudApplet.fromApplet) {
    try {
      out = new BufferedWriter(new FileWriter(filename, true));
    } catch (IOException ie) {
      System.out.println("Unable to open file: " + filename);
      ie.printStackTrace();
    }
//	  }
    return out;
  }

/*  public static final FileOutputStream getFileOutputStream(String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    return getFileOutputStream("", filename);
  }*/

  public static final FileOutputStream getFileOutputStream(String folder, String filename) {
    if (filename == null || filename.equalsIgnoreCase("null"))
      return null;
    FileOutputStream out = null;
    filename = getCorrectFilename(folder + filename);
//		if (!it.unitn.ing.rista.MaudApplet.fromApplet) {
    try {
      out = new FileOutputStream(new File(RelativeURL.getURL(filename).getFile()));
    } catch (IOException e) {
      // e.printStackTrace();
      try {
        out = new FileOutputStream(new File(filename));
      } catch (IOException ie) {
        System.out.println("Unable to open file: " + filename);
        ie.printStackTrace();
      }
    }
//	  }
    return out;
  }

  public static final String getIntStringFormatted(int number, int digits) {
    String temp = Integer.toString(number);
    int slenght = temp.length();
    for (int i = 0; i < digits - slenght; i++)
      temp = new String(" " + temp);
    return temp.substring(temp.length() - digits, temp.length());
  }

  public static final String getIntStringFormattedFullZeros(int number, int digits) {
    String temp = Integer.toString(number);
    int slenght = temp.length();
    for (int i = 0; i < digits - slenght; i++)
      temp = new String("0" + temp);
    return temp.substring(temp.length() - digits, temp.length());
  }

  public static final String getDoubleStringFormatted(double number, int digits, int decimals) {
    int intpart = (int) number;
    int decimalpart = 0;
    if (intpart < 0) {
      intpart++;
      decimalpart = (int) (-(number - intpart) * MoreMath.powint(10, decimals));
    } else {
      decimalpart = (int) ((number - intpart) * MoreMath.powint(10, decimals));
    }
    return new String(getIntStringFormatted(intpart, digits) + "." +
            getIntStringFormattedFullZeros(decimalpart, decimals));
  }

  public static final String getFirstPFline(Phase aphase) {
    double acell[] = Angles.getLattice(aphase);

    StringBuffer tmpstr = new StringBuffer("");
    for (int i = 0; i < 6; i++)
      tmpstr = tmpstr.append(getDoubleStringFormatted(acell[i], 5, 4));
    tmpstr = tmpstr.append(getIntStringFormatted(SpaceGroups.getLGNumberSiegfriedConv(aphase.getPointGroup()), 5))
            .append("    1");
    return tmpstr.toString();
//				new String("    1.0000    1.0000    1.0000   90.0000   90.0000   90.0000    7    1");
  }

  public static final String getFirstHHline(Phase aphase) {
    double acell[] = Angles.getLattice(aphase);

    StringBuffer tmpstr = new StringBuffer("");
    for (int i = 0; i < 6; i++)
      tmpstr = tmpstr.append(getDoubleStringFormatted(acell[i], 5, 4));
    return tmpstr.toString();
//				new String("    1.0000    1.0000    1.0000   90.0000   90.0000   90.0000");
  }

  public static final String checkPath(String folder, String filename) {
    while (filename.startsWith("./"))
      filename = filename.substring(2, filename.length());
    while (filename.startsWith("../")) {
      String tmp = folder.substring(0, folder.length() - 1);
      int index = tmp.lastIndexOf('/');
      if (index > 1) {
        filename = filename.substring(3, filename.length());
        folder = folder.substring(0, index + 1);
      } else if (index == 1) {
        filename = filename.substring(3, filename.length());
        folder = folder.substring(0, index);
      } else
        break;
    }
    return folder + filename;
  }

  public static final String filterPathForSpace(String path) {
    return replaceSubstringInString(path, "%20", " ");
  }

  public static final String replaceSubstringInString(String astring, String oldsubstring,
                                                      String newsubstring) {
    int startindex = astring.indexOf(oldsubstring);
    while (startindex >= 0) {

      StringBuffer tempstring = new StringBuffer("");
      if (startindex > 0)
        tempstring.append(astring.substring(0, startindex));
      tempstring.append(newsubstring);
      tempstring.append(astring.substring(startindex + oldsubstring.length(), astring.length()));

      astring = tempstring.toString();
      startindex = astring.indexOf(oldsubstring);
    }
    return astring;

  }

  public static final String replaceSubstringInStringIgnoreCase(String astring,
                                                                String oldsubstring,
                                                                String newsubstring) {
    int startindex = astring.toLowerCase().indexOf(oldsubstring.toLowerCase());
    if (startindex < 0)
      return astring;

    StringBuffer tempstring = new StringBuffer("");
    if (startindex > 0)
      tempstring.append(astring.substring(0, startindex));
    tempstring.append(newsubstring);
    tempstring.append(astring.substring(startindex + oldsubstring.length(), astring.length()));

    return tempstring.toString();
  }

  public static boolean checkTesting() {
//	System.out.println("check");
//		if (it.unitn.ing.rista.MaudApplet.fromApplet)
//			return true;
//	System.out.println("check further");
    return checkForFile("lucaTest");
  }

  public static boolean checkForFile(String fileToCheck) {
    File f1 = null;
/*    if (!it.unitn.ing.rista.MaudText.textonly && it.unitn.ing.rista.MaudApplet.fromApplet) { // || Constants.webStart) {
			try {
					f1 = new File((new URL(Constants.ourCodebase, fileToCheck)).getFile());
	  	} catch (IOException e) {
        e.printStackTrace();
	  	}
		} else*/
    f1 = new File(fileToCheck);

//	  System.out.println("Check for file: " + f1.getAbsolutePath());

    if (f1 != null && f1.exists())
      return true;

    return false;
  }

  public static boolean deleteFile(String fileToCheck) {
    File f1 = null;
/*    if (!it.unitn.ing.rista.MaudText.textonly && it.unitn.ing.rista.MaudApplet.fromApplet) { // || Constants.webStart) {
			try {
					f1 = new File((new URL(Constants.ourCodebase, fileToCheck)).getFile());
	  	} catch (IOException e) {
        e.printStackTrace();
	  	}
		} else*/
    f1 = new File(fileToCheck);

    if (f1 != null && f1.exists())
      return f1.delete();

    return false;
  }

  public static boolean moveFile(String fileToMove, String finalDestination) {
    File f1 =  new File(fileToMove);
    File f2 =  new File(finalDestination);

    if ((f1 != null && f1.exists()) && (f2 != null && f2.exists())) {
      f1.renameTo(f2);
      deleteFile(fileToMove);
      return true;
    }

    return false;
  }

  public static boolean checkForFolderOrCreateIt(String fileToCheck) {
    File f1 = null;
/*    if (!it.unitn.ing.rista.MaudText.textonly && it.unitn.ing.rista.MaudApplet.fromApplet) { // || Constants.webStart) {
			try {
					f1 = new File((new URL(Constants.ourCodebase, fileToCheck)).getFile());
	  	} catch (IOException e) {
        e.printStackTrace();
	  	}
		} else*/
    f1 = new File(fileToCheck);

    if (f1 != null && f1.isDirectory())
      return true;
    else if (f1 != null && !f1.exists()) {
      return f1.mkdirs();
    }

    return false;
  }

	public static String getPathToMaudJar(String maudJar) {
		String startingPath = getClasspath();
		String maudPath = "";
		if (startingPath == null || startingPath.indexOf("Maud.jar") < 1)
			maudPath = getUserDir();
		else {
      maudPath = startingPath.substring(0, startingPath.indexOf("Maud.jar") - 1);
      int indexClasspathSeparator = maudPath.lastIndexOf(Constants.pathSeparator);
//      System.out.println("index: " + indexClasspathSeparator);
      if (indexClasspathSeparator > -1)
        maudPath = maudPath.substring(indexClasspathSeparator + 1);
    }
		System.out.println("First version: " + maudPath);
		if (!(new File(maudPath + Constants.fileSeparator + maudJar)).exists()) {
			maudPath = System.getProperty("user.dir");
			System.out.println("Second version: " + maudPath);
			if (!(new File(maudPath + Constants.fileSeparator + maudJar)).exists()) {
				maudPath = System.getProperty("user.dir") + "/Maud.app/Contents/Resources/Java";
				System.out.println("Third version: " + maudPath);
			}
		}
		return maudPath;
	}

	public static String getClasspath() {
/*    Misc obj = new Misc();
    if (obj != null) {
      return obj.getClass().getProtectionDomain().getCodeSource().getLocation().getPath();
    }*/
    return System.getProperty("java.class.path");
  }

	public static ClassLoader getClassLoader() {
		Misc obj = new Misc();
		if (obj != null) {
			return obj.getClass().getClassLoader();
		}
		return null;
	}

/*  public static boolean checkMacOSXApp() {
//		if (it.unitn.ing.rista.MaudApplet.fromApplet)
//			return false;
    if (Constants.macosx) {
      if (getResource(Constants.imagefolder + "maud_logo.tif").toString().indexOf("Contents/Resources") != -1)
        return true;
    }
    return false;
  }*/

  public static boolean isRightMouseButton(java.awt.event.MouseEvent event) {
    if (Constants.macos || Constants.macosx)
      return event.isControlDown();
    else
      return event.isMetaDown();
  }

  public static short readShortLittleEndian(DataInputStream reader) throws IOException {
    // 2 bytes
    int low = reader.readByte() & 0xff;
    int high = reader.readByte() & 0xff;
    return (short) (high << 8 | low);
  }

  public static long readLongLittleEndian(DataInputStream reader) throws IOException {
    // 8 bytes
    long accum = 0;
    for (int shiftBy = 0; shiftBy < 64; shiftBy += 8)
      accum |= (reader.readByte() & 0xff) << shiftBy;
    return accum;
  }

  public static char readCharLittleEndian(DataInputStream reader) throws IOException {
    // 2 bytes
    int low = reader.readByte() & 0xff;
    int high = reader.readByte() & 0xff;
    return (char) (high << 8 | low);
  }

  public static char readCharOneByte(DataInputStream reader) throws IOException {
    // 1 bytes
    int low = reader.readByte() & 0xff;
    return (char) low;
  }

  public static int readIntLittleEndian(DataInputStream reader) throws IOException {
    // 4 bytes
    int accum = 0;
    for (int shiftBy = 0; shiftBy < 32; shiftBy += 8)
      accum |= (reader.readByte() & 0xff) << shiftBy;
    return accum;
  }

  public static double readDoubleLittleEndian(DataInputStream reader) throws IOException {
    long accum = 0;
    for (int shiftBy = 0; shiftBy < 64; shiftBy += 8)
      accum |= ((long) (reader.readByte() & 0xff)) << shiftBy;
    return Double.longBitsToDouble(accum);
  }

  public static float readFloatLittleEndian(DataInputStream reader) throws IOException {
    int accum = 0;
    for (int shiftBy = 0; shiftBy < 32; shiftBy += 8)
      accum |= (reader.readByte() & 0xff) << shiftBy;
    return Float.intBitsToFloat(accum);
  }

  public static byte readByteLittleEndian(DataInputStream reader) throws IOException {
    // 1 byte
    return reader.readByte();
  }

  public static final void swapVectorElements(Vector vect, int index1, int index2) {
    Object obj = vect.elementAt(index1);
    vect.setElementAt(vect.elementAt(index2), index1);
    vect.setElementAt(obj, index2);
    return;
  }

  public static final String[] readFormattedLine(String line, int[] digits) {
    int number = digits.length;
    return readFormattedLine(line, digits, number, -1);
  }

  public static final String[] readFormattedLine(String line, int[] digits, int number, int repeat) {

//      System.out.println( line );
    String[] data = new String[number * repeat];

	  if (digits != null) {

    int index = 0;
    int pointer = 0;
    int newpointer = 0;
    int maxpointer = 0;
    if (repeat == -1)
      repeat = 99999;
    for (int j = 0; j < number; j++)
      maxpointer += digits[j];


    for (int i = 0; i < repeat && newpointer + maxpointer <= line.length(); i++) {
      for (int j = 0; j < number; j++) {
        newpointer += digits[j];
        data[index] = line.substring(pointer, newpointer);
//        System.out.println(index + " " + data[index]);
        index++;
        pointer = newpointer;
      }
    }
	  } else {
		  // free format, we read just the number * repeat data
		  StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
		  for (int i = 0; i < number * repeat; i++)
			  data[i] = st.nextToken();
	  }

    return data;

  }

  public static String freeMemory() {
    System.out.println("Run finalization");
    System.runFinalization();
    System.out.println("Run garbage collector");
    System.gc();
    System.out.println("Check memory");
    long freeMem = Runtime.getRuntime().freeMemory();
    System.out.println("Free Memory: " + freeMem / 1024 + "K");
    long totMem = Runtime.getRuntime().totalMemory();
    System.out.println("Total Memory: " + totMem / 1024 + "K");
    return "Used Memory: " + (totMem - freeMem) / 1024 + "K";
  }

  public static DataInputStream getDataBufferedInputStream(String filename) {
    return getDataBufferedInputStream("", filename);
  }

  public static DataInputStream getDataBufferedInputStream(String folder, String filename) {
    InputStream in = getInputStream(folder, filename);
    if (in != null)
      return new DataInputStream(new BufferedInputStream(in));
    return null;
  }

  public static BufferedInputStream getBufferedInputStream(String folder, String filename) {
    return new BufferedInputStream(getInputStream(folder, filename));
  }

  /**
   * Opens the OS default web browser on <code>url</code>
   *
   * @param url The URL to send to the browser
   */
  public static boolean openBrowserOnURL( URL url )
  {
    if ( url == null )
    {
      return false;
    }
    String prefix = null;
    switch (Constants.osType) {
      case Constants.OsMac:
        prefix = MaudPreferences.getPref("macox.openBrowserCommand", "open /Applications/Safari.app") + " ";
      break;
      case Constants.OsWindoof:
        prefix = MaudPreferences.getPref("windows.openBrowserCommand", "cmd /c start") + " ";
      break;
      case Constants.OsLinux:
        prefix = prefix = MaudPreferences.getPref("linux.openBrowserCommand", "open Mozilla")
            + " "; // todo : check for Linux
      break;
      case Constants.OsUnix:
        prefix = prefix = MaudPreferences.getPref("unix.openBrowserCommand", "open Mozilla")
            + " "; // todo : check for Unix
      break;
      default: {

      }
    }
    System.out.println( "Opening " + url.toString() );
    try
    {
      Runtime.getRuntime().exec( prefix + url.toString() );
      return true;
    }
    catch ( IOException ex )
    {
      ex.printStackTrace();
    }
    return false;
  }

  public static void println() {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println();
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println();
      }
    }
  }

  public static void println(boolean x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
      }
    }
  }

  public static void println(char x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
      }
    }
  }

  public static void println(char[] x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
      }
    }
  }

  public static void println(double x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
      }
    }
  }

  public static void println(float x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
      }
    }
  }

  public static void println(int x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
      }
    }
  }

  public static void println(long x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
      }
    }
  }

  public static void println(Object x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
      }
    }
  }

  public static void println(String x) {
    switch (Constants.stdoutput) {
      case Constants.STANDARD:
//        checkOutput();
      case Constants.CONSOLE_WINDOW:
        System.out.println(x);
        System.out.flush();
        break;
      case Constants.NO_OUTPUT:
        break;
      case Constants.TO_FILE:
        break;
      default: {
        System.out.println(x);
        System.out.flush();
      }
    }
  }

  public static void warning(final String text) {
//    if (MaudText.textonly)
      System.out.println("Warning: " + text);
/*    else
      (new Thread() {
        public void run() {
          (new AttentionD(text)).setVisible(true);
        }
      }).start();*/
  }

  static boolean consoleStarted = false;

  public static void startConsole() {
	  switch (Constants.stdoutput) {
		  case Constants.STANDARD:
			  break;
		  case Constants.CONSOLE_WINDOW:
			  consoleStarted = true;
			  System.out.println("Starting new console!");
			  try {
				  Constants.outputConsole = new Console();
				  Constants.outputConsole.setVisible(true);
			  } catch (Exception e) {
				  e.printStackTrace();
			  }
			  break;
		  case Constants.NO_OUTPUT:
			  break;
		  case Constants.TO_FILE:
			  break;
		  default: {
		  }
	  }
  }

  public static void checkOutput() {
    if (!consoleStarted)
      startConsole();
  }

	public static void printMatrix(String s, double[][] b) {
		System.out.println(s);
		for (int i = 0; i < b.length; i++) {
			for (int j = 0; j < b[0].length; j++)
				System.out.print(" " + b[i][j]);
			System.out.println();
		}
	}

	public static void printMatrix(String s, double[] b) {
		System.out.println(s);
		for (int i = 0; i < b.length; i++) {
			System.out.print(" " + b[i]);
		}
		System.out.println();
	}

}
