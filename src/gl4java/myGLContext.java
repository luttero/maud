/**
 * @(#) myGLContext.java
 */


package gl4java;

import java.lang.reflect.Method;
import java.security.AccessController;
import java.security.PrivilegedAction;

/**
 * Subclass of the base manager class for the OpenGL language mapping for Java !
 * To overcome the System.exit(1) problem.
 *
 * @version 	1.00, 21. January 2003
 * @author      Luca Lutterotti
 *
 */
public class myGLContext extends GLContext {

  public myGLContext() {
    super(null, null, null);
  }

  public final static boolean loadMyNativeLibraries(String gljLibName, String glLibName, String gluLibName) {
    return doLoadMyNativeLibraries(gljLibName, glLibName, gluLibName);
  }

// this is the infamous method

  static int myosType = 0;

  public final static boolean doLoadMyNativeLibraries(String gljLibName, String nativeGLLibName,
                                                      String nativeGLULibName) {
    String myjvmVendor = null, myjvmVersion = null;
    int myjvmVersionMajor = 0, myjvmVersionMinor = 0;
    boolean myisNetscapeJvm = false, myisMicrosoftJvm = false, myisIBMJvm = false;//, myuseMSJDirect = false;
    String myosName = null, myjniEXTsuff = null;

    if (libsLoaded) return true;

    if (gljClassDebug)
      System.out.println("GLContext.doLoadNativeLibraries will do it !");

    myjvmVendor = java.lang.System.getProperty("java.vendor");
    myjvmVersion = java.lang.System.getProperty("java.version");

    if (gljClassDebug) {
      System.out.println("jvm vendor: " + myjvmVendor);
      System.out.println("jvm version: " + myjvmVersion);
    }

    int i0 = 0;
    int i1 = myjvmVersion.indexOf(".", i0);
    String strhlp = null;
    if (i1 > 0) {
      strhlp = myjvmVersion.substring(i0, i1);
      try {
        myjvmVersionMajor = Integer.valueOf(strhlp).intValue();
      } catch (Exception e) {
        System.out.println("Not a number: " + strhlp + " (" + myjvmVersion + ")");
      }
    }
    i0 = i1 + 1;
    i1 = myjvmVersion.indexOf(".", i0);
    if (i1 < 0)
      i1 = myjvmVersion.length(); // no 2nd dot, no bug version number

    if (0 < i0 && i0 < i1) {
      strhlp = myjvmVersion.substring(i0, i1);
      try {
        myjvmVersionMinor = Integer.valueOf(strhlp).intValue();
      } catch (Exception e) {
        System.out.println("Not a number: " + strhlp + " (" + myjvmVersion + ")");
      }
    }

    if (gljClassDebug) {
      System.out.println("jvm version (parsed): " + "major: " + myjvmVersionMajor + ", minor: " + myjvmVersionMinor);
    }

    myisNetscapeJvm = myjvmVendor != null && myjvmVendor.indexOf("Netscape") >= 0;
    myisMicrosoftJvm = myjvmVendor != null && myjvmVendor.indexOf("Microsoft") >= 0;
    myisIBMJvm = myjvmVendor != null && myjvmVendor.indexOf("IBM") >= 0;

    // Determine the OS
    myosName = System.getProperty("os.name").toLowerCase();
    if (myosName.startsWith("wind"))
      myosType = OsWindoof;
    else if (myosName.startsWith("darwin") || myosName.startsWith("mac os x"))
      myosType = OsMacX;
    else if (myosName.startsWith("mac os"))
      myosType = OsMac9;
    else // oops - lets guess unix/x11 :-)
      myosType = OsX11;

    if (myjvmVersionMajor >= 2 || (myjvmVersionMajor == 1 && myjvmVersionMinor >= 4)) {
      myjniEXTsuff = "14";
    } else if (myjvmVersionMajor == 1 && myjvmVersionMinor >= 3 && !myisIBMJvm && !myisMicrosoftJvm) {
      myjniEXTsuff = "13";
    } else if (myjvmVersionMajor == 1 && myjvmVersionMinor >= 2) {
      myjniEXTsuff = "12";
    } else {
      myjniEXTsuff = "";
    }

    if (nativeGLLibName == null) {
      if (myosType == OsWindoof)
        nativeGLLibName = defNativeGLLibWin32;
      else if (myosType == OsMac9)
        nativeGLLibName = defNativeGLLibMacOS9;
      else if (myosType == OsMacX)
        nativeGLLibName = defNativeGLLibMacOSX;
      else
        nativeGLLibName = defNativeGLLibX11;
    }

    if (nativeGLULibName == null) {
      if (myosType == OsWindoof)
        nativeGLULibName = defNativeGLULibWin32;
      else if (myosType == OsMac9)
        nativeGLULibName = defNativeGLULibMacOS9;
      else if (myosType == OsMacX)
        nativeGLULibName = defNativeGLULibMacOSX;
      else
        nativeGLULibName = defNativeGLULibX11;
    }

    if (gljLibName == null)
      gljLibName = defGljLib + myjniEXTsuff;

    String[] libNames = null;

    if ((myosType == OsWindoof) && (myisMicrosoftJvm)) {
      // JDirect loads the GL libraries automatically,
      // so we don't have to.
      libNames = new String[2];
      libNames[0] = gljLibName;
      libNames[1] = defGljMSWinLib;
//      myuseMSJDirect = true;
    } else {
      // For MAC, Win32+SunJVM, Unices ...
      libNames = new String[1];
      libNames[0] = gljLibName;
//      myuseMSJDirect = false;
    }

    final String f_libNames[] = libNames;

    if (myisNetscapeJvm) {
      System.out.println("Netscape JVM try to get Privileges");
      try {
        Class privmgr = Class.forName("netscape.security.PrivilegeManager");
        Class[] parameterTypes = new Class[1];
        parameterTypes[0] = Class.forName("java.lang.String");
        Method m = privmgr.getMethod("enablePrivilege", parameterTypes);
        Object args[] = new Object[1];
        args[0] = (new String("UniversalLinkAccess"));
        m.invoke(privmgr, args);

//		netscape.security.PrivilegeManager.enablePrivilege("UniversalLinkAccess");

        System.out.println("Netscape-Privilege: enabled UniversalLinkAccess priv.");
      } catch (Exception ex) {
        System.out.println("Not enabled Netscape-Privilege: UniversalLinkAccess priv.");
      }
    }

    boolean ok;

    if (myjvmVersionMajor >= 2 || (myjvmVersionMajor == 1 && myjvmVersionMinor >= 2)) {
      Boolean ook = (Boolean) AccessController.doPrivileged(new PrivilegedAction() {
        public Object run() {
          // load libs
          int libNumber = 0;
//          String _libName = null;
          boolean libLoaded[] = new boolean[f_libNames.length];

          for (libNumber = 0; libNumber < f_libNames.length; libNumber++)
            libLoaded[libNumber] = false;

          for (libNumber = 0; libNumber < f_libNames.length; libNumber++) {
            do {
              try {
                System.loadLibrary(f_libNames[libNumber]);
                libLoaded[libNumber] = true;
                if (gljClassDebug) {
                  System.out.println("loaded native library: " + f_libNames[libNumber]);
                }
              } catch (UnsatisfiedLinkError e) {
                System.out.println("Sorry, can't find the library: " + f_libNames[libNumber] + "\n" + e);
                f_libNames[libNumber] = null; // stop trying ... :-(
              }
            } while (libLoaded[libNumber] == false && f_libNames[libNumber] != null);
          }
          for (libNumber = 0; libNumber < f_libNames.length; libNumber++)
            if (libLoaded[libNumber] == false)
              return new Boolean(false);
//					try {
//						if (useJAWT()) {
//							if (!loadJAWT()) {
//								System.err.println("ERROR while loading jawt.dll/libjawt.so");
//								return new Boolean(false);
//							}
//						}
//						return new Boolean(GLContext.loadNativeLibraries(null, null, null));
//					} catch (Throwable te) {
//						if (myosType != OsMacX)
//							return new Boolean(false);
//					}
          return new Boolean(true);
        }
      });
      ok = ook.booleanValue();
    } else {
      // load libs
      int libNumber = 0;
//      String _libName = null;
      boolean libLoaded[] = new boolean[f_libNames.length];

      for (libNumber = 0; libNumber < f_libNames.length; libNumber++)
        libLoaded[libNumber] = false;

      for (libNumber = 0; libNumber < f_libNames.length; libNumber++) {
        do {
          try {
            System.loadLibrary(f_libNames[libNumber]);
            libLoaded[libNumber] = true;
            if (gljClassDebug) {
              System.out.println("loaded native library: " + f_libNames[libNumber]);
            }
          } catch (UnsatisfiedLinkError e) {
            System.out.println("Sorry, can't find the library:" + f_libNames[libNumber] + "\n" + e);

            f_libNames[libNumber] = null; // stop trying ... :-(
          }
        } while (libLoaded[libNumber] == false && f_libNames[libNumber] != null);
      }
      ok = true;
      for (libNumber = 0; libNumber < f_libNames.length; libNumber++)
        if (libLoaded[libNumber] == false) {
          ok = false;
          break;
        }
    }

    if (ok && myosType != OsMacX) { // Mac OS X version is a little bit older
      if (gljClassDebug)
        System.out.println("fetching GL/GLU functions ...");
      if (gljFetchGLFunctions(nativeGLLibName, nativeGLULibName, true)) {
        if (gljClassDebug)
          System.out.println("fetched GL/GLU functions succesfully !");
        libsLoaded = true;
      } else {
        System.out.println("GL4Java-ERROR: can't fetch GL/GLU functions !");
      }

    } else if (ok) {
      libsLoaded = true;
    }

// here is what we need to eliminate
//	if(!libsLoaded)
//		System.exit(1);

    return libsLoaded;
  }

}

