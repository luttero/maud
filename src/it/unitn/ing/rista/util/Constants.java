/*
 * @(#)Constants.java created 1/01/1997 ?
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

package it.unitn.ing.rista.util;

import com.amd.aparapi.device.Device;
import com.amd.aparapi.device.OpenCLDevice;
import com.amd.aparapi.internal.opencl.OpenCLPlatform;
import it.unitn.ing.fortran.*;
import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.chemistry.*;

import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.net.*;
import java.io.*;
import java.util.*;
import java.util.Formatter;
import java.util.List;
import java.util.jar.*;
import java.lang.*;
import java.lang.reflect.*;

import it.unitn.ing.rista.diffr.rsa.*;
import it.unitn.ing.rista.interfaces.*;

import javax.swing.*;
import javax.swing.plaf.basic.BasicInternalFrameTitlePane;

/**
 * The Constants is a class providing general constants used by the program.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.53 $, $Date: 2006/12/04 14:30:15 $
 * @since JDK1.1
 */


public class Constants {

  public static final int kDefaultFontSize = 10;
//	public	static	final	Font	kDefaultFont			= new Font("Geneva", Font.PLAIN, kDefaultFontSize);

  public static final int kDefaultMargin = 20;

  public static final int kMousePositionHeight = 20;
  public static final int kMousePositionX = kDefaultMargin;
  public static final int kMousePositionY = kDefaultFontSize;

  public static final int kSceneXPosition = 0;
  public static final int kSceneYPosition = kDefaultMargin;
  public static final int kSceneWidth = 380;
  public static final int kSceneHeight = 400;

  public static final int kDefaultWidth = kSceneHeight + kDefaultMargin;
  public static final int kDefaultHeight = kSceneHeight + kDefaultMargin;
	public static final double MAX_VALUE_FOR_DERIVATIVE = 1.0E6;
	public static final double MULTIPLIER_FOR_DERIVATIVE = 1.0E-6;

	public static Color crystallite_color = null;
  public static float[] crystallite_mat_specular = new float[4];
  public static float[] crystallite_mat_shininess = new float[1];
  public static float[] lightpos = new float[4];
  public static float[] openglBackColor = {0.0f, 0.0f, 0.0f, 0.0f};

  public static boolean OpenGL = false;

  public static final int borderInside = 3;
  public static final int borderInsideMinimum = 0;
  public static final int borderInsideMaximum = 6;

  public static boolean newMaud = false;
  public static final int CONSOLE_WINDOW = 0;
	public static final int STANDARD = 1;
	public static final int NO_OUTPUT = 2;
  public static final int TO_FILE = 3;
  public static int stdoutput = CONSOLE_WINDOW;
	public static boolean textureOutput = false;
	public static boolean strainOutput = false;
	public static boolean structureFactorsOutput = false;


	public static boolean showProgressFrame = false;

	public static final int NONE_GRANULARITY = 0;
	public static final int COARSE_GRANULARITY = 1;
	public static final int MEDIUM_GRANULARITY = 2;
	public static final int FINE_GRANULARITY = 3;
	public static int threadingGranularity = MEDIUM_GRANULARITY;

//	public	static	final	Color	kDefaultBackgroundColor		= Color.white;
//	public	static	final	Color	kDefaultForegroundColor		= Color.black;

  /**
   * Planck's constant.
   */
  public static final double PLANCK = 6.6260755E-34;
  /**
   * Planck's constant in eV.
   */
  public static final double PLANCK_eV = 4.13566743E-15;
  /**
   * Planck's constant divided by 2*PI (defined).
   */
  public static final double H_BAR = PLANCK / (2.0 * Math.PI);
	public static final double BAR_H = 2.0 * Math.PI / PLANCK * 1.0E-10;
  /**
   * Speed of light in vacuo (exact).
   */
  public static final double SPEED_OF_LIGHT = 2.99792458E+8;
  /**
   * Conversion constant energy -> lambda -> energy (eV -> Angstrom)
   */
  public static final double ENERGY_WAVELENGTH = PLANCK_eV * SPEED_OF_LIGHT / 1.0E-10;
  /**
   * Permeability constant (exact).
   */
  public static final double PERMEABILITY = Math.PI * 4E-7;
  /**
   * Permittivity constant (defined).
   */
  public static final double PERMITTIVITY = 1.0 / (PERMEABILITY * SPEED_OF_LIGHT * SPEED_OF_LIGHT);
  /**
   * Gravitational constant.
   */
  public static final double GRAVITATION = 6.67259E-11;
  /**
   * Elementary charge.
   */
  public static final double CHARGE = 1.60217733E-19;
  /**
   * Electron rest mass.
   */
  public static final double ELECTRON_MASS = 9.1093897E-31;
  /**
   * Proton rest mass.
   */
  public static final double PROTON_MASS = 1.6726231E-27;
  /**
   * Neutron rest mass.
   */
  public static final double NEUTRON_MASS = 1.6749286E-27;
  /**
   * Avogadro's constant.
   */
  public static final double LAMBDA_SPEED_NEUTRON_CONV_REC = NEUTRON_MASS / PLANCK * 10E-8;   // in cm
  /**
   * Avogadro's constant.
   */
  public static final double AVOGADRO = 6.02214199E+23;

	public static final double AVOGADRO_LAMBDA2_CM = 6.02214199E+7;

	public static final double E_RADIUS_CM = 2.817940285E-13;

	/**
   * Molar gas constant.
   */
  public static final double MOLAR_GAS = 8.314510;
  /**
   * Boltzmann's constant (defined).
   */
  public static final double BOLTZMANN = MOLAR_GAS / AVOGADRO;
  /**
   * Stefan-Boltzmann constant.
   */
  public static final double STEFAN_BOLTZMANN = 5.67051E-8;
  /**
   * Rydberg constant (defined).
   */
  public static final double RYDBERG = SPEED_OF_LIGHT * PERMEABILITY * SPEED_OF_LIGHT * PERMEABILITY * SPEED_OF_LIGHT * ELECTRON_MASS * CHARGE * CHARGE / PLANCK * CHARGE / PLANCK * CHARGE / PLANCK / 8.0;
  /**
   * Fine structure constant (defined).
   */
  public static final double FINE_STRUCTURE = PERMEABILITY * SPEED_OF_LIGHT * CHARGE / PLANCK * CHARGE / 2.0;
  /**
   * Faraday constant.
   */
  public static final double FARADAY = 96485.309;
  /**
   * Magnetic flux quantum (defined).
   */
  public static final double FLUX_QUANTUM = PLANCK / (2.0 * CHARGE);
  /**
   * Bohr magneton (defined).
   */
  public static final double BOHR_MAGNETON = CHARGE / ELECTRON_MASS * H_BAR / 2.0;
  /**
   * Magnetic moment of electron.
   */
  public static final double ELECTRON_MOMENT = 9.2847701E-24;
  /**
   * Magnetic moment of proton.
   */
  public static final double PROTON_MOMENT = 1.41060761E-26;

	public static final double E_SCAT_FACTOR = PLANCK * PLANCK / (2.0 * Math.PI * ELECTRON_MASS * CHARGE) *
			1.0E20;
	public static final double E_SCAT_FACTOR_PI = E_SCAT_FACTOR / Math.PI;
	public static final double ENERGY_CONSTANT = 1.9569341E-6;
	
	public static final double ENERGY_LAMBDA = 12398.424121;
  public static final double I_ENERGY_LAMBDA = 1.0 / 12398.424121;


  public static final int FLOAT_FIELD = 12;
  public static final double TOOLERANCE_COORD = 1.0E-3;
  public static final double PI = Math.PI;
	public static final double PI_2 = PI / 2.0;
  public static final double I_PI = 1.0 / Math.PI;
  public static final double PI2 = 2.0 * PI;
	public static final double PI4 = 4.0 * Math.PI;
  public static final double PI_QUADRO = PI * PI;
  public static final double SQRTPI2 = Math.sqrt(PI2);
  public static final double DEGTOPI = PI / 180.0;
  public static final double PITODEG = 180.0 / PI;
  public static final double E_RADIUS = 2.81E-5;  // Angstrom
  public static final double LN2 = 6.9314718056E-01;
	public static final double LN2_2 = 3.4657359027997E-01;
  public static final double sqrtln2pi = 4.6971863935E-01;
  public static final double SQRT_OF_PILN2 = 1.475664626635606;
  public static final double sqrt2ln2 = 1.177410022515475;
  public static final double one_sqrt2ln2 = 0.849321800288019;
  public static final double sqrt2 = 1.41421356237;
	public static final double one_sqrt2 = 0.707106781186548;
  public static final double sqrt3 = 1.73205080757;
  public static final double mstraintoetilde = 1.253314137316; // Math.sqrt(Math.PI/2)
  public static final double integrationStepPF = 1.0;
  public static final double integrationStepPFR = integrationStepPF * DEGTOPI;
  public static final double pisimg = integrationStepPFR / (6.0 * PI);
  public static final double ODFresolution = 5.0;
  public static final double minimumdspace = 0.05;
  public static final double defaultWavelengthIndexing = 1.5405981;
//  public static final double startingStructureFactor = 1000.0;
  public static final double log10Conv = 1.0 / Math.log(10.0);
//  public static String filesfolder = null;
  public static String imagefolder = "images/";
  public static String iconfolder = imagefolder + "20x20/";
  public static String helpfolder = "help/";
  public static String libDir = "lib";
  public static final String pluginsDir = "/plugins/";
  public static final String programIcon = "Maud_ladybug_icon_32.gif";
  public static final String backupFile = "paramete.sav";
  public static final String language = "en";
  public static final String typeclass = "properties";
  public static String resultsFile = "results.txt";
  public static String userName = null;
  public static String startPath = "/";
  public static String maudReleaseBuilt = "$Revision: 2.80 $";
  public static String maudDateBuilt = "$Date: 2018/01/11 19:33:00 $";

  public static final double arg2PIover3 = PI2 / 3.;
  public static final double sinArg2PIover3 = Math.sin(arg2PIover3);
  public static final double cosArg2PIover3 = Math.cos(arg2PIover3);
  public static double maud_version = 2.80;
	public static boolean useOpenCL = false;
	public static Vector<OpenCLDevice> openClDevices= null;
	public static OpenCLDevice openclDevice = null;
	public static boolean nativeComputation = false;
  public static boolean grayShaded = false;
  public static boolean checkCIFinputInConsole = true;
  public static String pathToMaudJar = "";

  public static boolean windoze = false;
  public static boolean macos = false;
  public static boolean macosx = false;
  public static int osType = -1;
  public static final int OsWindoof = 0;
  public static final int OsMac = 1;
  public static final int OsUnix = 2;
  public static final int OsLinux = 3;

//  public static boolean ITS = false;

  public static final String lineSeparator = System.getProperty("line.separator");
  public static final String fileSeparator = System.getProperty("file.separator");
  public static final String pathSeparator = System.getProperty("path.separator");
//	public static boolean dataloaded = false;
//	public static String inetAddr = "127.0.0.1";
  //	public static boolean speedUp = true;
  public static boolean confirmation = true;

  public static boolean useNewAbsorption = true;

	public static boolean forceOverwrite = true;

  public static long tmpTime = 0l;

  public static final int COMPUTED = 0, EXPERIMENTAL = 1, UNITARY = 2;
  public static final double ENTIRE_RANGE = 1.0;

  public static String identifier[] = null;
  public static String classname[] = null;
  public static int numberofclasstype = 0;

//  public static final int ANGULAR_CALIBRATION_S = 21;
//  public static final int INTENSITY_CALIBRATION_S = 22;

  // parameters
  public static final int PARAMETER_CHANGED = 100;
  public static final int PARAMETER_ADDED = 98;
  public static final int PARAMETER_REMOVED = 99;
  public static final int BKG_FILE_CHANGED = 119;
  public static final int BKG_PARAMETER_CHANGED = 120;
  public static final int ANGULAR_CALIBRATION = 121;
  public static final int INTENSITY_CALIBRATION = 122;
  public static final int SAMPLE_ORIENTATION_CHANGED = 123;
  public static final int TEXTURE_CHANGED = 124;
  public static final int CELL_CHANGED = 125;
  public static final int STRAIN_CHANGED = 126;
  public static final int ATOM_POSITION_CHANGED = 127;
  public static final int FRAGMENT_POSITION_CHANGED = 128;
  public static final int RADIATION_WAVELENGTH_CHANGED = 129;
  public static final int PEAKS_PARAMETER_CHANGED = 131;
  public static final int INSTRUMENT_BROADENING = 132;
  public static final int SAMPLE_BROADENING = 133;
//  public static final int SAMPLE_Z_POSITION_CHANGED = 133;
  public static final int STRUCTURE_FACTOR_CHANGED = 134;
  public static final int THICKNESS_CHANGED = 135;
  public static final int PHASE_WEIGHT_CHANGED = 136;
  public static final int BEAM_INTENSITY_CHANGED = 137;
  public static final int ERROR_POSITION_CHANGED = 144;
	public static final int ERROR_2THETA_CHANGED = 138;
	public static final int THERMAL_SHIFT_CHANGED = 139;
  public static final int REFLECTIVITY_PARAMETER_CHANGED = 140;
  public static final int LORENTZ_POLARIZATION_CHANGED = 141;
  public static final int SHAPE_ABSORPTION_CHANGED = 142;
  public static final int TDS_CHANGED = 143;

  // functions

  public static final int PEAKS_POSITIONS_REFRESH = 1001;
  public static final int PHASE_QUANTITY_REFRESH = 1002;
  public static final int ABSORPTION_REFRESH = 1003;
  public static final int STRUCTURE_FACTORS_REFRESH = 1004;
  public static final int TEXTURE_REFRESH = 1005;
  public static final int STRAIN_REFRESH = 1006;
  public static final int LORENTZ_POLARIZATION_REFRESH = 1007;
  public static final int SCATTERING_FACTORS_REFRESH = 1008;

  // objects
  public static final int OBJECT_CHANGED = 200;
  public static final int OBJECT_ADDED = 198;
  public static final int OBJECT_REMOVED = 199;

  // strings
  public static final int STRING_CHANGED = 300;
  public static final int STRING_ADDED = 298;
  public static final int STRING_REMOVED = 299;

	public static final int PLOT_REFRESH = 2001;

	public static boolean testing = false;
  public static boolean testtime = false;
  public static boolean esquienabled = false;

  public static boolean webStart = false;
  public static URL ourCodebase = null;

  public static int maxNumberOfThreads = 1;
  public static int timeToWaitThreadsEnding = 30;
  public static int timeToWaitThreadsStarting = 3;
  public static boolean debugThreads = false;

  public static Vector sounds = null;
  public static int computationPriority = Thread.NORM_PRIORITY;
  public static boolean refreshTreePermitted = true;
  public static boolean textonly = false;
  public static boolean initialized = false;
  public static final String NONE = "None";
  public static boolean consoleShouldBeVisible = false;
  public static Console outputConsole = null;
  public static double STARTING_STRUCTURE_FACTOR = 1000.0;
  public static double MINIMUM_STRUCTURE_FACTOR = 1.0;
	//public static String filesJar;
	public static String maudJar = null;
	public static String imagesJar;
	public static String helpJar;
	public static String libDirname;
	public static float PIf = (float) PI;
	public static float PI_2f = (float) PI_2;
	public static float sqrt2f = (float) sqrt2;
	public static it.unitn.ing.fortran.Formatter ifmt = null;
	public static it.unitn.ing.fortran.Formatter ffmt = null;
	public static it.unitn.ing.fortran.Formatter efmt = null;

	public static String userHomeDirectory = "";
	public static String userDirectory = "";
  public static String libraryDirectory = "";
	public static String nativeLibraryDirectory = "";
	public static String cctbxNativeLibrary = "";
  public static String documentsDirectory = "";
	public static String examplesDirectory = "";
  public static String cachesDirectory = "";
  public static String applicationSupportDirectory = "";
	public static String logsDirectory = "";
	public static String startingLog = "startingLog";
  public static boolean sandboxEnabled = false;
  public static String startingAppDirectory = "";

	public static String refineIcon = "slot_machine_20.gif";

  public static String getVersion() {
    return Double.toString(maud_version);
  }

  public static String getFullVersion() {
    return maudReleaseBuilt;
  }

	public static void initForMacOS(boolean sandboxed) {
//		System.out.println("Init for OS X/macOS");
		macosx = true;
		osType = OsMac;
		String libDir = System.getProperty("LibraryDirectory");
		if (libDir != null && !libDir.equalsIgnoreCase("null")) {
			libraryDirectory = libDir + fileSeparator;
			cachesDirectory = System.getProperty("CachesDirectory") + fileSeparator + "com.radiographema.maud" + fileSeparator;
			applicationSupportDirectory = System.getProperty("ApplicationSupportDirectory") + fileSeparator;
			logsDirectory = libraryDirectory + "Logs" + fileSeparator;
			startingAppDirectory = System.getProperty("java.library.path") + fileSeparator;
			if (sandboxed)
				documentsDirectory = System.getProperty("DocumentsDirectory") + fileSeparator;
			else
				documentsDirectory = System.getProperty("DocumentsDirectory") + fileSeparator + "maud" + fileSeparator;
		}
	}

	public static void initForWindows(boolean sandboxed) {
//		System.out.println("Init for Windows");
		windoze = true;
		startPath = "//";
		osType = OsWindoof;
		computationPriority--; // the default one is too strong for windows
		startingLog = "startingLog";
		applicationSupportDirectory = userHomeDirectory + "AppData" + fileSeparator + "Local" +
				fileSeparator + "maud" + fileSeparator;
		libraryDirectory = applicationSupportDirectory + "lib" + fileSeparator;
		documentsDirectory = applicationSupportDirectory + "doc" + fileSeparator;
		cachesDirectory = applicationSupportDirectory + "caches" + fileSeparator;
		logsDirectory = libraryDirectory + "Logs" + fileSeparator;
	}

	public static void initForLinux(boolean sandboxed) {
///		System.out.println("Init for linux");
		osType = OsLinux;
		startingLog = "startingLog";
		applicationSupportDirectory = userHomeDirectory + "maud" + fileSeparator;
		libraryDirectory = applicationSupportDirectory + "lib" + fileSeparator;
		documentsDirectory = applicationSupportDirectory + "doc" + fileSeparator;
		cachesDirectory = applicationSupportDirectory + "caches" + fileSeparator;
		logsDirectory = libraryDirectory + "Logs" + fileSeparator;
	}

	public static void initConstants() {
		if (System.getProperty("SandboxEnabled") != null)
			sandboxEnabled = false; // System.getProperty("SandboxEnabled").equalsIgnoreCase("true");// (the String "true" or "false")

		maudReleaseBuilt = maudReleaseBuilt.substring(1, maudReleaseBuilt.length() - 1);
		maudDateBuilt = maudDateBuilt.substring(1, maudDateBuilt.length() - 1);

		maudReleaseBuilt = "version: " + maud_version + ", cvs update on " + maudDateBuilt +
				", " + maudReleaseBuilt;

		userName = System.getProperty("user.name");
		userHomeDirectory = System.getProperty("user.home") + Constants.fileSeparator;
		userDirectory = System.getProperty("user.dir") + Constants.fileSeparator;
		documentsDirectory = userHomeDirectory + "Documents" + fileSeparator + "maud" + fileSeparator;
		examplesDirectory = documentsDirectory + "examples" + fileSeparator;
		logsDirectory = userHomeDirectory + "Library" + fileSeparator + "Logs" + fileSeparator;

		libraryDirectory = userDirectory;
		cachesDirectory = System.getProperty("java.io.tmpdir");
		applicationSupportDirectory = userDirectory;

		String osName = System.getProperty("os.name");
//		System.out.println("System: " + osName);
		osType = OsUnix;
		if (osName != null && osName.indexOf("Mac OS X") != -1) {
			initForMacOS(sandboxEnabled);
		} else if (osName != null && osName.indexOf("Windows") != -1) {
			initForWindows(sandboxEnabled);
		} else if (osName != null && osName.indexOf("Mac") != -1) {
			initForMacOS(sandboxEnabled);
		} else if (osName != null && osName.indexOf("Linux") != -1) {
			initForLinux(sandboxEnabled);
		}

		File appSupFile = new File(applicationSupportDirectory);
		File libDirFile = new File(libraryDirectory);
		File docDirFile = new File(documentsDirectory);
		File examplesDirFile = new File(examplesDirectory);
		File cachesDirFile = new File(cachesDirectory);
		File logsDirFile = new File(logsDirectory);
		try {
			if (!appSupFile.exists())
				appSupFile.mkdirs();
			if (!libDirFile.exists())
				libDirFile.mkdirs();
			if (!docDirFile.exists())
				docDirFile.mkdirs();
			if (!examplesDirFile.exists())
				examplesDirFile.mkdirs();
			if (!cachesDirFile.exists())
				cachesDirFile.mkdirs();
			if (!logsDirFile.exists())
				logsDirFile.mkdirs();
		} catch (Exception e) {
			e.printStackTrace();
		}

		startingLog = logsDirectory + startingLog;

	  if (!textonly)
		  redirectConsoleOutputToFile(startingLog);
    if (initialized)
      return;
    initialized = true;

		System.out.println("User home: " + userHomeDirectory);
		System.out.println("Application folder: " + userDirectory);
		System.out.println("App support: " + applicationSupportDirectory);
		System.out.println("Library folder: " + libraryDirectory);
		System.out.println("Logs folder: " + logsDirectory);
		System.out.println("Caches folder: " + cachesDirectory);
		System.out.println("Documents folder: " + documentsDirectory);
		System.out.println("App starting folder: " + startingAppDirectory);

		sounds = new Vector(0, 1);

	  Properties p = System.getProperties();
	  Enumeration keys = p.keys();
	  while (keys.hasMoreElements()) {
		  String key = (String)keys.nextElement();
		  String value = (String)p.get(key);
		  System.out.println(key + ": " + value);
	  }

	  if (sandboxEnabled) { // Sandboxing for the moment is for OS X
		  System.out.println("Initialized sandboxing environment!");
	  } else {
		  System.out.println("No sandboxing!");
	  }

    String vers = System.getProperty("java.version");
//	  String classnamesFile = "files/classnames.ins";
	  pathToMaudJar = Misc.getPathToMaudJar("Maud.jar");
	  maudJar = pathToMaudJar + fileSeparator + "Maud.jar";
/*	  String fileToLoad = "/.maudpath_" + userName;
	  if (windoze)
		  fileToLoad = "/maudpath_" + userName;
	  else {
		  maudJar = "/" + maudJar;
		  pathToMaudJar = "/" + pathToMaudJar;
	  }*/

	  if (pathToMaudJar.startsWith("/."))
		  pathToMaudJar = pathToMaudJar.substring(1);
	  if (maudJar.startsWith("/."))
		  maudJar = maudJar.substring(1);
	  System.out.println("Maud.jar located at: " + maudJar);
	  System.out.println("Path to Maud jar: " + pathToMaudJar);

	  if (macosx) {
	  	nativeLibraryDirectory = pathToMaudJar + fileSeparator + "../../Frameworks" + fileSeparator;
	  	cctbxNativeLibrary = nativeLibraryDirectory + "libcctbxForMaud.dylib";
	  } else {
		  nativeLibraryDirectory = pathToMaudJar + fileSeparator;
		  if (windoze)
			  cctbxNativeLibrary = nativeLibraryDirectory + "libcctbxForMaud.dll";
		  else
			  cctbxNativeLibrary = nativeLibraryDirectory + "libcctbxForMaud.so";
	  }

/*	  String pathFile = pathToMaudJar + fileToLoad;
    if (sandboxEnabled)
      pathFile = applicationSupportDirectory + fileToLoad;*/

    String full_build_number = "Maud_full_build.number";
//    String reduced_build_number = new String("/Maud_reduced_build.number");
    BufferedReader buildNumbreader = null;
    try {
      buildNumbreader = Misc.getResourceReader(maudJar, full_build_number);
    } catch (Exception eee) {
      eee.printStackTrace();
    }
    if (buildNumbreader != null) {
      try {
        String line = "_";
        String date = "";
        int counter = 0;
        while (line != null && !line.startsWith("build")) {
          line = buildNumbreader.readLine();
          counter++;
          if (counter == 2) {
            date = line.substring(1, line.length());
          }
        }
        if (line != null) {
          String token = "0";
          StringTokenizer st = new StringTokenizer(line, " =,\t\r\n");
          if (st.hasMoreTokens())
            token = st.nextToken();
          if (st.hasMoreTokens())
            token = st.nextToken();
//				build_number = Integer.valueOf(token).intValue();
          maudReleaseBuilt += "build " + token + ", date " + date;
        }
        buildNumbreader.close();
      } catch (IOException io) {
        try {
          buildNumbreader.close();
        } catch (Exception ioe) {
        }
      }
    }

	  File[] jarFileList = null;
		File[] pluginsJarFileList = null;
	  libDirname = pathToMaudJar + fileSeparator + libDir + fileSeparator;
		System.out.println("Lib dir: " + libDirname);
	  try {
      File libDir = new File(libDirname);
	    if (!libDir.exists()) {
		    libDirname = pathToMaudJar;
		    libDir = new File(libDirname);
      }
      if (libDir.exists()) {
        addToClassPath(libDirname);
        jarFileList = libDir.listFiles(new FilenameFilter() {
          public boolean accept(File dir, String name) {
            if (name.endsWith(".jar") || name.endsWith(".zip"))
              return true;
            return false;
          }
        });
        for (int i = 0; i < jarFileList.length; i++) {
          addToClassPath(jarFileList[i]);
          System.out.println("Adding to classpath: " + jarFileList[i]);
        }
      } else {
	      System.out.println("Lib dir not found!");
      }
    } catch (IOException io) {
		  io.printStackTrace();
    }
//	  filesJar = libDirname + "Files.jar";
	  imagesJar = libDirname + "Images.jar";
	  helpJar = libDirname + "Help.jar";

//	  jarFileList = null;
    try {
/*      String pathtoplugins = Misc.getUserDir();
	    System.out.println("Get user dir: " + pathtoplugins);
      if (pathtoplugins.startsWith("."))
        pathtoplugins = pathToMaudJar;*/
      String pluginsDirname = applicationSupportDirectory + "plugins";
      File pluginsDir = new File(pluginsDirname);
	    System.out.println("Plugins dir: " + pluginsDirname);
      if (pluginsDir.exists()) {
        addToClassPath(pluginsDirname);
	      pluginsJarFileList = pluginsDir.listFiles(new FilenameFilter() {
          public boolean accept(File dir, String name) {
            if (name.endsWith(".jar") || name.endsWith(".zip"))
              return true;
            return false;
          }
        });
        for (int i = 0; i < pluginsJarFileList.length; i++) {
          addToClassPath(pluginsJarFileList[i]);
	        System.out.println("Adding to classpath (plugins): " + pluginsJarFileList[i]);
        }
      } else {
	      System.out.println("Plugins dir not found!");
      }
    } catch (IOException io) {
	    io.printStackTrace();
    }

/*      if (textonly) {
	      System.out.println("Checking " + pathFile + " exist!");
        if (Misc.checkForFile(pathFile))
          filesfolder = getUserPath(pathFile);
        else
          filesfolder = "";
	      System.out.println("Final path: " + filesfolder);
      } else if (!Misc.checkForFile(pathFile)) {
	      System.out.println("Need to initialize the program, first run");
        filesfolder = initializeMaud(pathToMaudJar, pathFile);
      } else {
//        if (sandboxEnabled)
//          filesfolder = fileSeparator + applicationSupportDirectory;
//        else
        filesfolder = getUserPath(pathFile);
      }*/

//    filesfolder = new String(Misc.getUserDir()+"/files/");

//	  System.out.println("Checking again for the default.par: " + filesfolder + "default.par");

//	    for (int i = 0; i < filesfolder.length() && i < pathToMaudJar.length(); i++)
//		    System.out.println(filesfolder.charAt(i) + " == " + pathToMaudJar.charAt(i) + " = " +
//				    (filesfolder.charAt(i) == pathToMaudJar.charAt(i)));
// Check again if installation was ok or something is not working anymore
		System.out.println("Loading Maud preferences");
		double version = MaudPreferences.getDouble("maud.version", 1.0);
		if (version < 2.63) {
			// first time with versioning
			System.out.println("Resetting Maud preferences");
			MaudPreferences.resetPreferences();
		}
		MaudPreferences.setPref("maud.version", maud_version);

		if (!Misc.checkForFile(documentsDirectory + "default.par") || version < 2.65)
			initializeMaud(pathToMaudJar, documentsDirectory);

    stdoutput = MaudPreferences.getInteger("console.output", stdoutput);
	  textureOutput = MaudPreferences.getBoolean("log_output.file_texture", textureOutput);
	  strainOutput = MaudPreferences.getBoolean("log_output.file_strains", strainOutput);
	  structureFactorsOutput = MaudPreferences.getBoolean("log_output.file_structure_factors", structureFactorsOutput);

/*    if (macosx) {
      String brushMetalLook = MaudPreferences.getPref("apple.awt.brushMetalLook", "false");
      String brushMetalRounded = MaudPreferences.getPref("apple.awt.brushMetalRounded", "false");
      System.setProperty("apple.awt.brushMetalLook", brushMetalLook);
      System.setProperty("apple.awt.brushMetalRounded", brushMetalRounded);
    }*/
    // load some constants
    checkMaudPreferences();
	  threadingGranularity = MaudPreferences.getInteger("parallel_processing.granularity(0-3)", MEDIUM_GRANULARITY);
    maxNumberOfThreads = MaudPreferences.getInteger("parallel_processing.threads_maxNumber",
        Runtime.getRuntime().availableProcessors());
    debugThreads = MaudPreferences.getBoolean("parallel_processing.threads_debug", false);
    STARTING_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.default_value",
        STARTING_STRUCTURE_FACTOR);
    MINIMUM_STRUCTURE_FACTOR = MaudPreferences.getDouble("structure_factors.minimum_value_for_extraction", 
        MINIMUM_STRUCTURE_FACTOR);

    if (!textonly) {
	    System.out.println("Loading Interface preferences");
      WindowPreferences.loadPreferences();
		  Misc.println("Activating the console!");
	    if (stdoutput == Constants.CONSOLE_WINDOW)
	      Misc.checkOutput();
	    setConsoleVisible();
		  AtomColorPreferences.loadPreferences();
	  }

	  System.out.println("Initial log output located in: " + startingLog);

	  useOpenCL = false; //MaudPreferences.getBoolean("opencl.enable", useOpenCL);
/*	  long openClDevicePreferred = MaudPreferences.getLong("opencl.preferredDeviceID", -1);
	  if (useOpenCL) {
		  openClDevices = new Vector<OpenCLDevice>();
		  boolean selected = false;
		  List<OpenCLPlatform> platforms = (new OpenCLPlatform()).getOpenCLPlatforms();
		  System.out.println("Machine contains " + platforms.size() + " OpenCL platforms");
		  int platformc = 0;
		  for (OpenCLPlatform platform : platforms) {
			  System.out.println("Platform " + platformc + "{");
			  System.out.println("   Name    : \"" + platform.getName() + "\"");
			  System.out.println("   Vendor  : \"" + platform.getVendor() + "\"");
			  System.out.println("   Version : \"" + platform.getVersion() + "\"");
			  List<OpenCLDevice> devices = platform.getOpenCLDevices();
			  System.out.println("   Platform contains " + devices.size() + " OpenCL devices");
			  int devicec = 0;
			  for (OpenCLDevice device : devices) {
				  System.out.println("   Device " + devicec + "{");
				  System.out.println("       ID                    : " + device.getDeviceId());
				  System.out.println("       Type                  : " + device.getType());
				  System.out.println("       GlobalMemSize         : " + device.getGlobalMemSize());
				  System.out.println("       LocalMemSize          : " + device.getLocalMemSize());
				  System.out.println("       MaxComputeUnits       : " + device.getMaxComputeUnits());
				  System.out.println("       MaxWorkGroupSizes     : " + device.getMaxWorkGroupSize());
				  System.out.println("       MaxWorkItemDimensions : " + device.getMaxWorkItemDimensions());
				  System.out.println("   }");
				  devicec++;

				  openClDevices.add(device);
				  if (device.getDeviceId() == openClDevicePreferred) {
					  selected = true;
					  openclDevice = device;
					  System.out.println("Preferred: " + openclDevice.getDeviceId());
				  }
				  if (!selected && device.getType() == Device.TYPE.GPU) {
					  openclDevice = device;
					  System.out.println("Selecting: " + openclDevice.getDeviceId());
				  }
			  }
			  System.out.println("}");
			  platformc++;
			  if (openclDevice != null)
			    MaudPreferences.setPref("opencl.preferredDeviceID", openclDevice.getDeviceId());
		  }
	  }*/
	  ParameterPreferences.loadPreferences();
    LastInputValues.loadPreferences();

	  System.out.println("Java version: " + vers);
	  if (vers.compareTo("1.7") < 0) {
		  System.out.println("!!!WARNING: Maud must be run with a " +
				  "1.7 or higher version of Java VM!!!");
	  }
	  System.out.println("Encoding: " + System.getProperty("file.encoding"));

	  System.out.println("Maud " + maudReleaseBuilt);
//	  System.out.println("pathToMaudJar: " + pathToMaudJar);
//	  System.out.println("Maud.jar exist? " + Misc.checkForFile(maudJar));
//	  System.out.println("Instruction file path: " + pathFile);
	  System.out.println(java.lang.System.getProperty("java.class.path"));
	  System.out.println("Lib dir: " + libDirname);

	  System.out.println("Check paths and libraries....");
    if (testing)
	    System.out.println("Use native computation: " + nativeComputation);
//    if (testing)
//      System.out.println("Use altivec computation: " + useAltivec);
    if (testing)
	    System.out.println("Maud jar: " + maudJar);
    if (testing) {
	    System.out.println("DEBUG: System: " + osName);
	    System.out.println("DEBUG: name: " + userName);
    }
//    if (testing)
//    System.out.println("DEBUG: loading openGl library");
//    System.out.println("If an error appears on trying to load the opengl library simply ignore it");

//    if (testing)
//      System.out.println("Use GL4Java rendering: " + OpenGL);
    if (!textonly) {
      if (testing)
	      System.out.println("DEBUG: reading 3D preferences");
      read3DPreferences();
    }
/*		if (testing)
      System.out.println("DEBUG: check SgInfo");
	  Phase.native_loaded = checkSgInfoLib();*/

    if (testing)
	    System.out.println("DEBUG: loading atom properties");
    try {
	    (new Thread() {
		    @Override
		    public void run() {
			    AtomInfo.loadAtomConstants();
			    XRayDataSqLite.loadEbelAndShellTables(true);
			    Sla33Constants.initConstants();
		    }
	    }).start();
    } catch (Exception e) {
    	e.printStackTrace(System.out);
    }

//	  Fmt.setMaximumFractionDigits(9);

	  try {
		  ifmt = new it.unitn.ing.fortran.Formatter("I6");
		  ffmt = new it.unitn.ing.fortran.Formatter("F12.4");
		  efmt = new it.unitn.ing.fortran.Formatter("E12.4");
	  } catch (InvalidFormatException e) {
		  e.printStackTrace(System.out);
	  }

	  BufferedReader reader;

    numberofclasstype = 0;

    Vector tmpString = new Vector(0, 100);
    Vector identString = new Vector(0, 100);

    //  String dummyIdentifier = (new BaseFactoryObject()).identifier;
    if (!textonly) {
      try {
	      if (testing)
        System.out.println("DEBUG jar: start loading music from classpath!");
        JarFile maudJarFile = new JarFile(maudJar);
        Enumeration jarEntries = maudJarFile.entries();
        while (jarEntries.hasMoreElements()) {
          String token = jarEntries.nextElement().toString();
          String entryName = new String("/" + token);
//          System.out.println(entryName);
          if (entryName.endsWith(".au") || entryName.endsWith(".rmf") || entryName.endsWith(".mid")
              || entryName.endsWith(".wav") || entryName.endsWith(".aif") || entryName.endsWith(".aiff"))
            sounds.add(maudJarFile + entryName);
        }
        jarEntries = null;
      } catch (IOException ioe) {
        ioe.printStackTrace(System.out);
      }

      if (MaudPreferences.getBoolean("startupMusic.enable", false))
        (new MusicPlayer(sounds)).start();
    }
/*	  try {
      reader = Misc.getResourceReader(maudJar, classnamesFile);
      try {
        String token;
        String linedata = reader.readLine();
        String ident;
        while (linedata != null) {

//            	System.out.println(linedata);
          StringTokenizer st = new StringTokenizer(linedata, " ,\t\r\n");

          while (st.hasMoreTokens()) {
            token = st.nextToken();
            if (token != null && !token.startsWith("#")) {
              try {
                if (testing)
                  System.out.println("Loading class: " + token);
                Class aclass = Class.forName(token);
                Constructor ctor = aclass.getConstructor(new Class[]{});
                BaseFactoryObject obj = (BaseFactoryObject) ctor.newInstance(new Object[]{});
                ident = new String(obj.identifier);
                tmpString.addElement(token);
                identString.addElement(ident);
                numberofclasstype++;
              } catch (Exception e) {
                if (testing) {
                  System.out.println("Class not loaded: " + token);
                  e.printStackTrace();
                }
              } catch (Error e) {
                System.out.println("Class not loaded (due to the following error): " + token);
                e.printStackTrace();
              }
            }
          }
          linedata = reader.readLine();
        }
      } catch (IOException e) {
        System.out.println("Error in loading the data file!");
        e.printStackTrace();
      }
      reader.close();
    } catch (IOException e) {
      System.out.println("File not found!");
      e.printStackTrace();
    }*/

		if (pluginsJarFileList == null)
			pluginsJarFileList = new File[0];
//		if (testing)
//			System.out.println("Scanning jars: " + (pluginsJarFileList.length + 1));
    if (pluginsJarFileList != null) {
      String[] allMaudJars = new String[1 + pluginsJarFileList.length]; // + 1];
      allMaudJars[0] = pathToMaudJar + fileSeparator + "Maud.jar";
	    for (int i = 0; i < pluginsJarFileList.length; i++) {
		    try {
			    allMaudJars[1 + i] = pluginsJarFileList[i].getCanonicalPath();
		    } catch (IOException e) {
			    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		    }
	    }

	    try {
      for (int j = 0; j < allMaudJars.length; j++) {
        if (testing)
          System.out.println("Loading from jar: " + allMaudJars[j]);
        Vector list = ClassScanner.getClassListFromJar(allMaudJars[j], "",
            "it.unitn.ing.rista.diffr.XRDcat");
        if (testing)
          System.out.println("List number: " + list.size());

        for (int i = 0; i < list.size(); i++) {
          String token = (String) list.get(i);
          if (testing)
            System.out.println("Loading class: " + token);
          try {
            Class aclass = Class.forName(token);
            Constructor ctor = aclass.getConstructor(new Class[]{});
            BaseFactoryObject obj = (BaseFactoryObject) ctor.newInstance(new Object[]{});
            String ident = new String(obj.identifier);
            tmpString.addElement(token);
            identString.addElement(ident);
            numberofclasstype++;
          } catch (Exception e) {
            if (testing) {
              System.out.println("Class not loaded: " + token);
              e.printStackTrace();
            }
          }
        }

      }
	    } catch (Exception e) {
		    e.printStackTrace(System.out);  //To change body of catch statement use File | Settings | File Templates.
	    }
    }

    if (numberofclasstype > 0) {
      identifier = new String[numberofclasstype];
      classname = new String[numberofclasstype];
      for (int i = 0; i < numberofclasstype; i++) {
        classname[i] = (String) tmpString.elementAt(i);
        identifier[i] = (String) identString.elementAt(i);
      }

    }

//	  Alfpack.talfpk();
  }

	public static void redirectConsoleOutputToFile(String filename) {
		try {
			File outFile = new File(filename);
			PrintStream newOut = new PrintStream(new FileOutputStream(outFile));
			System.setOut(newOut);
			System.setErr(newOut);
		} catch (FileNotFoundException e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

  public static void setConsoleVisible() {
    setConsoleVisible(MaudPreferences.getBoolean("console_window.visible", false));
  }

  public static void setConsoleVisible(boolean visible) {
    MaudPreferences.setPref("console_window.visible", visible);
    consoleShouldBeVisible = visible;
    if (outputConsole != null)
      outputConsole.setVisible(visible);
  }

  public static void checkMaudPreferences() {
//    esquienabled = Misc.checkForFile(pathToMaudJar + "/EsquiClient.jar");
//    if (Misc.checkTesting())
      testing = MaudPreferences.getBoolean("newFeatures.testing", testing);
    testtime = MaudPreferences.getBoolean("debug.computeTime", testtime);
//    useAltivec = MaudPreferences.getBoolean("debug.useG4Altivec", useAltivec);
	  nativeComputation = false;
	  OpenGL = false;

    useNewAbsorption = MaudPreferences.getBoolean("absorption.new_model", true);
    checkCIFinputInConsole = MaudPreferences.getBoolean("cifInput.check_outputInConsole", false);
  }

  public static void read3DPreferences() {

    BufferedReader in = Misc.getReader(documentsDirectory, "Properties.3D");
    if (in == null)
      in = Misc.getResourceReader(maudJar, "files/Properties.3D");
    String token;
    if (in != null) {
      try {

        // Crystallite rendering

        int[] color = new int[3];
        String line = in.readLine();
        while (line.startsWith("#"))
          line = in.readLine();
        StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
        for (int i = 0; i < 3; i++) {
          if (st.hasMoreTokens()) {
            token = st.nextToken();
            color[i] = Integer.valueOf(token).intValue();
          }
        }
        crystallite_color = new Color(color[0], color[1], color[2]);
        line = in.readLine();
        while (line.startsWith("#"))
          line = in.readLine();
        st = new StringTokenizer(line, " ,\t\r\n");
        for (int i = 0; i < 4; i++) {
          if (st.hasMoreTokens()) {
            token = st.nextToken();
            lightpos[i] = Float.valueOf(token).floatValue();
          }
        }
        line = in.readLine();
        while (line.startsWith("#"))
          line = in.readLine();
        st = new StringTokenizer(line, " ,\t\r\n");
        for (int i = 0; i < 4; i++) {
          if (st.hasMoreTokens()) {
            token = st.nextToken();
            crystallite_mat_specular[i] = Float.valueOf(token).floatValue();
          }
        }
        line = in.readLine();
        while (line.startsWith("#"))
          line = in.readLine();
        st = new StringTokenizer(line, " ,\t\r\n");
        for (int i = 0; i < 1; i++) {
          if (st.hasMoreTokens()) {
            token = st.nextToken();
            crystallite_mat_shininess[i] = Float.valueOf(token).floatValue();
          }
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

  public static Vector getClassList(String asuperclass) {
    Vector tmpString = new Vector(0, 1);
    if (numberofclasstype > 0) {
      for (int i = 0; i < numberofclasstype; i++) {
        if (Misc.areClassCompatibles(asuperclass, classname[i])) {
          try {
            Class aclass = Class.forName(classname[i]);
            Constructor ctor = aclass.getConstructor(new Class[]{});
            BaseFactoryObject obj = (BaseFactoryObject) ctor.newInstance(new Object[]{});
	          if (obj.identifier != BaseFactoryObject.noneID && !obj.identifier.toLowerCase().startsWith("disabled")) {
		          tmpString.addElement(new String(classname[i]));
		          tmpString.addElement(new String(obj.IDlabel));
		          tmpString.addElement(new String(obj.identifier));
	          } else {
		          if (Constants.testing)
			          System.out.println("Not valid subclass: " + classname[i]);
		          if (Constants.testing && obj.identifier.toLowerCase().startsWith("disabled")) {
		            tmpString.addElement(new String("disabled"));
		            tmpString.addElement(new String("----"));
		            tmpString.addElement(new String("----"));
		          }
	          }
          } catch (Exception e) {
            System.out.println("Class not loaded: " + classname[i]);
/*            tmpString.addElement(new String("disabled"));
            tmpString.addElement(new String("----"));
            tmpString.addElement(new String("----"));*/
          }
        }
      }

    }
    return tmpString;
  }

  public static void initializeMaud(String pathToMaudJar, String pathFile) {
    String folder = pathFile;
//    System.out.println("Starting folder : " + folder);
    try {
      String libAddition = "";
	    libAddition = "/lib/";
//			System.out.println(pathToMaudJar + webStartAddition + "Files.jar");
      JarFile filesJar = null;
      try {
        filesJar = new JarFile(pathToMaudJar + libAddition + "Files.jar");
//        System.out.println("Found at " + pathToMaudJar + libAddition + "Files.jar");
      } catch (Exception e) {
        try {
          filesJar = new JarFile(pathToMaudJar + "/Files.jar");
          System.out.println("Found at " + pathToMaudJar + "/Files.jar");
        } catch (Exception ie) {
          System.out.println("Maud files not inizialized. No one of the following files has been found:");
          System.out.println(pathToMaudJar + libAddition + "Files.jar");
          System.out.println(pathToMaudJar + "/Files.jar");
        }
      }
      Enumeration jarEntries = filesJar.entries();
//      System.out.println("Final folder : " + folder);
      while (jarEntries.hasMoreElements()) {
        String entryName = jarEntries.nextElement().toString();
        int index = entryName.lastIndexOf("/");
        String entryName1 = new String(entryName);
        if (index >= 0)
          entryName1 = entryName.substring(index + 1);
        if (entryName1.length() > 0 &&
            entryName1.indexOf("META-INF") == -1 &&
            entryName1.indexOf("MANIFEST") == -1 &&
            entryName1.indexOf("MYSELF.") == -1 &&
            entryName1.indexOf("Files") == -1 &&
		        entryName1.indexOf("files") == -1 &&
            entryName1.indexOf(".DS_Store") == -1) {
          if (forceOverwrite || !Misc.checkForFile(folder + entryName)) {
	          it.unitn.ing.rista.util.FFT obj = new it.unitn.ing.rista.util.FFT();
	          BufferedInputStream in = new BufferedInputStream((obj.getClass().getResource("/" + entryName)).openStream());
	          FileOutputStream out = new FileOutputStream(folder + entryName1);
            int mark = 0;
            int available = in.available();
            byte[] bytes = new byte[available];
            try {
              while (available > 0 && in.read(bytes, mark, available) != -1) {
                out.write(bytes);
                mark += available;
                available = in.available();
                if (available > 0)
                  bytes = new byte[available];
              }
            } catch (IOException io) {
              io.printStackTrace();
            } finally {
              try {
                if (in != null)
                  in.close();
                if (out != null)
                  out.close();
              } catch (IOException io1) {
                io1.printStackTrace();
              }
            }
          }
        }
      }
	    filesJar = null;
	    folder = examplesDirectory;

	    try {
		    filesJar = new JarFile(pathToMaudJar + libAddition + "Examples.jar");
//		    System.out.println("Found at " + pathToMaudJar + libAddition + "Examples.jar");
	    } catch (Exception e) {
		    try {
			    filesJar = new JarFile(pathToMaudJar + "/Examples.jar");
			    System.out.println("Found at " + pathToMaudJar + "/Examples.jar");
		    } catch (Exception ie) {
			    System.out.println("Maud examples not inizialized. No one of the following files has been found:");
			    System.out.println(pathToMaudJar + libAddition + "Examples.jar");
			    System.out.println(pathToMaudJar + "/Examples.jar");
		    }
	    }
	    jarEntries = filesJar.entries();
//      System.out.println("Final folder : " + folder);
	    while (jarEntries.hasMoreElements()) {
		    String entryName = jarEntries.nextElement().toString();
		    int index = entryName.lastIndexOf("/");
		    String entryName1 = new String(entryName);
		    if (index >= 0)
			    entryName1 = entryName.substring(index + 1);
		    if (entryName1.length() > 0 &&
				    entryName1.indexOf("META-INF") == -1 &&
				    entryName1.indexOf("MANIFEST") == -1 &&
				    entryName1.indexOf("MYSELF.") == -1 &&
				    entryName1.indexOf("Examples") == -1 &&
				    entryName1.indexOf("examples") == -1 &&
				    entryName1.indexOf(".DS_Store") == -1) {
			    if (forceOverwrite || !Misc.checkForFile(folder + entryName)) {
				    it.unitn.ing.rista.util.FFT obj = new it.unitn.ing.rista.util.FFT();
				    BufferedInputStream in = new BufferedInputStream((obj.getClass().getResource("/" + entryName)).openStream());
				    FileOutputStream out = new FileOutputStream(folder + entryName1);
				    int mark = 0;
				    int available = in.available();
				    byte[] bytes = new byte[available];
				    try {
					    while (available > 0 && in.read(bytes, mark, available) != -1) {
						    out.write(bytes);
						    mark += available;
						    available = in.available();
						    if (available > 0)
							    bytes = new byte[available];
					    }
				    } catch (IOException io) {
					    io.printStackTrace();
				    } finally {
					    try {
						    if (in != null)
							    in.close();
						    if (out != null)
							    out.close();
					    } catch (IOException io1) {
						    io1.printStackTrace();
					    }
				    }
			    }
		    }
	    }
    } catch (Exception ioe) {
      ioe.printStackTrace();
    }
  }
/*
  static boolean result = false;

  private static boolean acceptLicense() {
    JButton acceptButton = new JButton("I accept the license");
    result = false;
    final AttentionD attdlg = new AttentionD(null, "Maud License Agreement",
            "The computer program Maud by Luca Lutterotti is distributed free of charge,\n" +
            "and without any restrictions for non-commercial use except for the following rules:\n" +
            "- It is permitted to extend the program capabilities or modify them for your own purposes.\n" +
            "- It is not permitted to redistribute the program or parts of it either under the name Maud,\n" +
            "  or under another name, without prior consultation with and permission from the author Luca Lutterotti.\n" +
            "- Whenever you publish results obtained with the aid of program Maud, please acknowledge its use by\n" +
            "  citing one or more of these articles (depending on your subject):\n" +
            "- L. Lutterotti, \"Total pattern fitting for the combined size-strain-stress-texture determination in\n" +
            "  thin film diffraction\", Nuclear Inst. and Methods in Physics Research, B, 268, 334-340, 2010.\n" +
            "- L. Lutterotti, M. Bortolotti, G. Ischia, I. Lonardelli and H.-R. Wenk, \"Rietveld texture analysis\n" +
            "  from diffraction images\", Z. Kristallogr., Suppl. 26, 125-130, 2007.\n" +
            "- L. Lutterotti, D. Chateigner, S. Ferrari and J. Ricote, \"Texture, Residual Stress and Structural\n" +
            "  Analysis of Thin Films using a Combined X-Ray Analysis\", Thin Solid Films, 450, 34-41, 2004.\n" +
            "- L. Lutterotti, S. Matthies, H.-R. Wenk, A. J. Schultz and J. Richardson, \"Combined texture and structure\n" +
            "  analysis of deformed limestone from neutron diffraction spectra\", J. Appl. Phys., 81[2], 594-600, 1997.\n" +
            "\n" +
            "Anyone wishing to make a commercial use of the software should contact the author\n" +
            "to obtain information about licensing conditions.", true, acceptButton, new JButton("I do not agree"));
    acceptButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        result = true;
        attdlg.setVisible(false);
        attdlg.dispose();
      }
    });
    attdlg.setVisible(true);

    while (attdlg.isVisible()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException ie) {
      }

    }

    return result;

  }

  public static String getUserPath(String pathFile) {
    String folder = null;
	  BufferedReader reader = null;
      try {
	      InputStreamReader in = new InputStreamReader(new FileInputStream(pathFile), "UTF8");
	      reader = new BufferedReader(in);
        folder = reader.readLine();
        folder = Misc.filterFileName(folder);
	      System.out.println("Folder for files: " + folder);
      } catch (IOException io) {
        io.printStackTrace();
      } finally {
        try {
          reader.close();
        } catch (IOException io1) {
        }
      }
    return folder;
  }*/

/*	public static void reset() {
		mustLoad = false;
		if (testing)
		  mustLoad = true;
		dataloaded = false;
	}

	public static void datareset() {
		mustLoad = false;
		if (testing)
		  mustLoad = true;
	}

	public static void datatutorial() {
		if (!dataloaded)
			mustLoad = true;
		dataloaded = true;
	}*/

// to do: all this should become an installer for jogl native libraries

/*  public static boolean installJOGL() {
    try {
      if (macos || !MaudPreferences.getBoolean("openGl.installNativeOpenGl", true)) {
        return false;
      }

      Frame instFrame = new Frame("GL4Java Installation");
      boolean installIt = JQuestionDialog.getResponse(instFrame,
              "Install or update GL4Java (OpenGL drawing) ? (Not required)");
      if (installIt) {
        if (macosx)
          return installGl4JavaMacOSX();
//        else
//          GL4JInst.main(null);
        return false;
      } else {
        MaudPreferences.setPref("openGl.installNativeOpenGl", false);
      }
    } catch (Throwable e) {
      MaudPreferences.setPref("openGl.installNativeOpenGl", false);
    }
    return false;
  }*/

  private Constants() {
  }

  public static void addToClassPath(String s) throws IOException {
    File f = new File(s);
    if (!f.exists()) {
      return;
    }
    URL u = f.toURL();
    Class[] parameters = new Class[]{URL.class};
    URLClassLoader sysloader = (URLClassLoader) ClassLoader.getSystemClassLoader();
//    ClassLoader aCL = Thread.currentThread().getContextClassLoader();
//    URLClassLoader aUrlCL = new URLClassLoader(urls, sysloader);
    Class sysclass = URLClassLoader.class;

    try {
      Method method = sysclass.getDeclaredMethod("addURL", parameters);
      method.setAccessible(true);
      method.invoke(sysloader, new Object[]{u});
    } catch (Throwable t) {
      t.printStackTrace();
      throw new IOException("Error, could not add URL to system classloader");
    }//end try catch

  }//end method

  public static void addToClassPath(File f) throws IOException {
    if (!f.exists()) {
      return;
    }
    URL u = f.toURL();
    Class[] parameters = new Class[]{URL.class};
    URLClassLoader sysloader = (URLClassLoader) ClassLoader.getSystemClassLoader();
//    ClassLoader aCL = Thread.currentThread().getContextClassLoader();
//    URLClassLoader aUrlCL = new URLClassLoader(urls, sysloader);
    Class sysclass = URLClassLoader.class;

    try {
      Method method = sysclass.getDeclaredMethod("addURL", parameters);
      method.setAccessible(true);
      method.invoke(sysloader, new Object[]{u});
    } catch (Throwable t) {
      t.printStackTrace();
      throw new IOException("Error, could not add URL to system classloader");
    }//end try catch

  }//end method

  public static void close() {
    // LogSystem.closeLog();  // close the output log system for errors
	  if (!textonly && outputConsole != null)
		  outputConsole.setVisible(false);
  }

/*  public static void warnAboutThreads() {
    boolean warning = MaudPreferences.getBoolean("warning.threads_more_than_one", false);
    if (!warning)
      return;
    MaudPreferences.setPref("warning.threads_more_than_one", false);
    JButton okButton = new JButton("Use one thread");
    final AttentionD attdlg = new AttentionD(null,
        "Warning, using more than one thread in parallel is unsafe in this version of Maud, would you like to revert to one thread?", okButton);
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        attdlg.setVisible(false);
        attdlg.dispose();
        MaudPreferences.setPref("parallel_processing.threads_maxNumber", 1);
        Constants.maxNumberOfThreads = 1;
      }
    });
    attdlg.setVisible(true);
  }*/
}
