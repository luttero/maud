package ij.plugin;

/*
***********************************************************************
*                                                                     *
* ESRF image format reader                                            *
*                                                                     *
* Written and maintained by Olof Svensson (svensson@esrf.fr)          *
*                                                                     *
* Petr Mikulik (mikulik@physics.muni.cz) has contributed code for     *
* reading EDF/EHF format headers                                      *
*                                                                     *
*                                                                     *
* Changes:                                                            *
*                                                                     *
* 12.10.2012   Luca Lutterotti                                        *
*              - added support for signed integer (Soleil)            *
*                                                                     *
* 25.02. 2008  Peter Cloetens                                         *
*              - close RandomAccessFile in after reading the header   *
*                file remained open after opening an EDF-image        *
*                with help of Stefan Schulze                          *
* 13.11. 2007  Peter Cloetens                                         *
*              - support DoubleValue as DataType (output PyMca)       *
*                image is converted to 32-bits Floating Point         *
* 18.11. 2006  Peter Cloetens                                         *
*              - support edf-files where first character header is not*
*                { (cfr cborel's problem)                             *
* 28.12. 2005  Peter Cloetens                                         *
*              EdfRead_ based on ESRF_Reader                          *
*              - make plugin recordable and callable as follows       *
*              in a plugin:                                           *
*              IJ.runPlugIn("EdfRead_", path);                        *
*              used in HandleExtraFileTypes                           *
*              - no selection EDF/EHF                                 *
*              seems incompatible with HandleExtraFileTypes           *
*              - remove write statements, logs in case of debug mode  *
*              write statements were perturbing Results window        *
*                                                                     *
* 29.11. 2001  Petr Mikulik                                           *
*              - support "UnsignedByte"                               *
*                                                                     *
* 21. 8. 2001  Petr Mikulik                                           *
*              - support "Float" as well as "FloatValue"              *
*              - support "UnsignedChar" and "SignedShort"             *
*              - bugfix for typo in reading "ByteOrder"               *
*              - added EdfRead_VERSION; show it in the about box      *
*                                                                     *
***********************************************************************
*/

import java.io.*;
import java.util.*;

import ij.*;
import ij.io.*;
import ij.gui.*;
import ij.plugin.PlugIn;


/** This plugin reads image formats used at the ESRF **/
public class EdfRead implements PlugIn {

  private static final String EdfRead_VERSION = "October 2012";

  // Supported types
	String[] types = {"EDF", "EHF"};
  String[] typesDescription = {"ESRF Data Format", "ESRF data Header Format"};

  // Default values for an image
  private int width = 512;
  private int height = 512;
  private int offset = 0;
  private int nImages = 1;
  private int gapBetweenImages = 0;
  private boolean whiteIsZero = false;
  private boolean intelByteOrder = false;
  private boolean openAll = false;
  private static String defaultDirectory = null;
  private int fileType = FileInfo.GRAY16_UNSIGNED;

  public void run(String arg)
  {
    String directory, fileName, type = "EDF";
    OpenDialog od;
    FileOpener fileOpener;
    File f;
    StringTokenizer st;

    // Show about box if called from ij.properties
	  if (arg.equals("about"))
    {
		  showAbout();
		  return;
    }

    // Open file
	  od = new OpenDialog("Open "+type+"...", arg);
	  directory = od.getDirectory();
	  fileName = od.getFileName();
	  if (fileName==null)
      return;
 	  //IJ.write("Opening "+type+" image "+directory+fileName);
	  // Otherwise this goes to the Results window
	  IJ.showStatus("Opening "+type+" image "+directory+fileName);
    f = new File(directory + fileName);

    // The following code is "borrowed" from ij.plugin.Raw
    FileInfo fileInfo = new FileInfo();
	  fileInfo.fileFormat = fileInfo.RAW;
	  fileInfo.fileName = fileName;
	  fileInfo.directory = directory;
	  fileInfo.width = width;
	  fileInfo.height = height;
	  fileInfo.offset = offset;
	  fileInfo.nImages = nImages;
	  fileInfo.gapBetweenImages = gapBetweenImages;
	  fileInfo.intelByteOrder = intelByteOrder;
	  fileInfo.whiteIsZero = whiteIsZero;
	  fileInfo.fileType = fileType ;

    // Try to get image size and no images from header
    if (type.equals("EDF") || type.equals("EHF"))
    {
      parseESRFDataFormatHeader(type, f, fileInfo);
    }
    // Leave the job to ImageJ for actually reading the image
	  fileOpener = new FileOpener(fileInfo);
	  fileOpener.open();
  }


  private void parseESRFDataFormatHeader(String type, File f, FileInfo fileInfo)
  {
	  RandomAccessFile in;
    char h;
    int headerSize, headerStart, noBrackets, i, iParam = 0;
    double dParam = 0.0;
    byte[] header;
    String key, param, token, headerString;

    try
    {
      in = new RandomAccessFile(f, "r");
      headerSize = 0;
      // to avoid problems when first character is not a bracket
      noBrackets = -1;
      do
      {
        headerStart = headerSize;
        in.seek(headerStart);
        do
        {
          h = getChar(in);
          headerSize += 1;
          if (h == '}') noBrackets--;
          else if (h== '{') 
          {
            if (noBrackets == -1)
            {
              // first bracket encountered
              noBrackets=1;
              headerStart=headerSize-1;
            }
            else noBrackets++;
          }
        } while (noBrackets != 0);
        //  Read header
        header = new byte[headerSize-headerStart+1];
        in.seek(headerStart);
        in.readFully(header);
        headerString = new String(header);
      } while ( type.equals("EHF") && (headerString.indexOf("EDF_DataBlockID") < 0) );
      // Close the file opened to read the header
      in.close();
    }
    catch (IOException ex)
    {
      IJ.write("IOException caught: "+ex);
      return;
    }
	  if (IJ.debugMode) IJ.log("ImportDialog: "+fileInfo);

    // Set offset
    fileInfo.offset = headerSize+1;

    // Extract information from header
    StringTokenizer st = new StringTokenizer ( headerString, ";" );
	  while (st.hasMoreTokens()) {
	    token = st.nextToken();
	    i = token.indexOf("=");
	    if (i<=0) continue;
	    key = token.substring(1,i-1).trim();
	    param = token.substring(i+1).trim();
      if (IJ.debugMode) IJ.log(key+": "+param);

      // For debugging - output to stdout is persistent:
      // System.out.println("DOING |"+key+"|: |" + param+"|");

	    try
      {
        iParam = Integer.valueOf(param).intValue();
		    dParam = Integer.valueOf(param).doubleValue();
	    }
	    catch(NumberFormatException numberformatexception) {};
	    if (key.equals("EDF_BinaryFileName"))
      {
        fileInfo.fileName = param;
        System.out.println("DEBUG: "+key+" = "+param);
        continue;
      }
	    if (key.equals("EDF_BinaryFilePosition"))
      {
        fileInfo.offset = iParam;
        continue;
      }
	    if (key.equals("Dim_1"))
      {
        fileInfo.width = iParam;
        continue;
      }
	    if (key.equals("Dim_2"))
      {
        fileInfo.height = iParam;
        continue;
      }
      if (key.equals("DataType"))
      {
		    if (param.equals("UnsignedLong") || param.equals("UnsignedInteger") || param.equals("SignedInteger"))
        {
		      fileInfo.fileType = fileInfo.GRAY32_INT;
        }
		    else if (param.equals("UnsignedShort"))
        {
		      fileInfo.fileType = fileInfo.GRAY16_UNSIGNED;
        }
		    else if (param.equals("Float") || param.equals("FloatValue"))
        {
                      fileInfo.fileType = fileInfo.GRAY32_FLOAT;
        }
		    else if (param.equals("Double") || param.equals("DoubleValue"))
        {
                      fileInfo.fileType = fileInfo.GRAY32_FLOAT;
        }
		    else if (param.equals("UnsignedByte") || param.equals("UnsignedChar"))
	{
		      fileInfo.fileType = fileInfo.GRAY8;
	}
		    else if (param.equals("SignedShort"))
	{
		      fileInfo.fileType = fileInfo.GRAY16_SIGNED;
	}
		    else
        {
          IJ.showMessage("WARNING: unknown data type " + param);
        }
		    continue;
      }
      if (key.equals("ByteOrder"))
      {
        fileInfo.intelByteOrder = param.equals("LowByteFirst");
      }
    }
    return;
  }

  int getShort(RandomAccessFile in) throws IOException
  {
	  int b1 = in.read();
	  int b2 = in.read();
	  if (intelByteOrder)
	    return ((b2 << 8) + b1);
	  else
	    return ((b1 << 8) + b2);
  }

  char getChar(RandomAccessFile in) throws IOException
  {
	  int b = in.read();
	  return (char)b;
  }

	void showAbout()
  {
    String message =
      "This plugin reads image formats commonly used the ESRF.\n"+
      "It can currently read the following formats:\n \n";
    for (int i=0; i<types.length; i++)
    {
      message += types[i]+":   "+typesDescription[i]+"\n";
    }
    message += " \n" +
      "This plugin is written and maintained by Olof Svensson, ESRF.\n"+
      "Please send suggestions or bug reports to svensson@esrf.fr.\n"+
      " \n" +
      "Petr Mikulik (mikulik@physics.muni.cz) has contributed code to\n" +
      "the EFD/EDH format header reader.\n \n" +
      "This is version " + EdfRead_VERSION + "\n";
    IJ.showMessage("About ESRF Edf+Hdf Reader...",message);
    }
}


// eof EdfRead.java

