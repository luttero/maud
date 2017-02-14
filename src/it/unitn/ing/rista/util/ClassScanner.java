/*
 * @(#)ClassScanner.java created Mar 10, 2006 Casalino
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import java.io.*;
import java.net.URL;
import java.net.JarURLConnection;
import java.util.jar.*;
import java.util.zip.*;
import java.util.Enumeration;
import java.util.Vector;
import java.awt.*;


/**
 * The ClassScanner is a class to find subclasses of a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @since JDK1.1
 */

public class ClassScanner {

  /**
   * Display all the classes inheriting or implementing a given
   * class in the currently loaded packages.
   *
   * @param tosubclassname the name of the class to inherit from
   */
  public static void find(String tosubclassname) {
    try {
      Class tosubclass = Class.forName(tosubclassname);
      Package [] pcks = Package.getPackages();
      for (int i = 0; i < pcks.length; i++) {
        find(pcks[i].getName(), tosubclass);
      }
    } catch (ClassNotFoundException ex) {
      System.err.println("Class " + tosubclassname + " not found!");
    }
  }

  /**
   * Display all the classes inheriting or implementing a given
   * class in a given package.
   *
   * @param pckname        the fully qualified name of the package
   * @param tosubclassname the name of the class to inherit from
   */
  public static Vector getList(String pckname, String tosubclassname) {
    Vector aList = new Vector(0, 1);
    try {
      Class tosubclass = Class.forName(tosubclassname);
      Package [] pcks = Package.getPackages();
      for (int i = 0; i < pcks.length; i++) {
        System.out.println("Scan ->" + pcks[i].getName());
        if (pcks[i].getName().startsWith(pckname)) {
          Vector tmpList = getClasses(pcks[i].getName(), tosubclass);
          for (int j = 0; j < tmpList.size(); j++)
            aList.add(tmpList.get(j));
        }
      }
    } catch (ClassNotFoundException ex) {
      System.err.println("Class " + tosubclassname + " not found!");
    }
    return aList;
  }

  /**
   * Display all the classes inheriting or implementing a given
   * class in a given package.
   *
   * @param pckname        the fully qualified name of the package
   * @param tosubclassname the name of the class to inherit from
   */
  public static void find(String pckname, String tosubclassname) {
    try {
      Class tosubclass = Class.forName(tosubclassname);
      find(pckname, tosubclass);
    } catch (ClassNotFoundException ex) {
      System.err.println("Class " + tosubclassname + " not found!");
    }
  }

  /**
   * Display all the classes inheriting or implementing a given
   * class in a given package.
   *
   * @param pckgname   the fully qualified name of the package
   * @param tosubclass the Class object to inherit from
   */
  public static Vector getClasses(String pckgname, Class tosubclass) {
    Vector list = new Vector(0, 1);
    // Code from JWhich
    // ======
    // Translate the package name into an absolute path
    String name = new String(pckgname);
    if (!name.startsWith("/")) {
      name = "/" + name;
    }
    name = name.replace('.', '/');

    // Get a File object for the package
    URL url = ClassScanner.class.getResource(name);
    // URL url = tosubclass.getResource(name);
    // URL url = ClassLoader.getSystemClassLoader().getResource(name);
//    System.out.println(name + "->" + url);

    // Happens only if the jar file is not well constructed, i.e.
    // if the directories do not appear alone in the jar file like here:
    //
    //          meta-inf/
    //          meta-inf/manifest.mf
    //          commands/                  <== IMPORTANT
    //          commands/Command.class
    //          commands/DoorClose.class
    //          commands/DoorLock.class
    //          commands/DoorOpen.class
    //          commands/LightOff.class
    //          commands/LightOn.class
    //          ClassScanner.class
    //
    if (url == null) return list;

    File directory = new File(url.getFile());

    // New code
    // ======
    if (directory.exists()) {
      // Get the list of the files contained in the package
      String [] files = directory.list();
      for (int i = 0; i < files.length; i++) {

        // we are only interested in .class files
        if (files[i].endsWith(".class")) {
          // removes the .class extension
          String classname = files[i].substring(0, files[i].length() - 6);
          try {
            // Try to create an instance of the object
//            Object o = Class.forName(pckgname + "." + classname).newInstance();
            if (tosubclass.isAssignableFrom(Class.forName(pckgname + "." + classname))) {
              list.add(classname);
//              System.out.println(classname);
            }
          } catch (ClassNotFoundException cnfex) {
            System.err.println(cnfex);
          } catch (NullPointerException np) {
            System.err.println(np);
          } catch (ExceptionInInitializerError ip) {
            System.err.println(ip);
          }
        }
      }
    } else {
      try {
        // It does not work with the filesystem: we must
        // be in the case of a package contained in a jar file.
        JarURLConnection conn = (JarURLConnection) url.openConnection();
        String starts = conn.getEntryName();
        JarFile jfile = conn.getJarFile();
        Enumeration e = jfile.entries();
        while (e.hasMoreElements()) {
          ZipEntry entry = (ZipEntry) e.nextElement();
          String entryname = entry.getName();
          if (entryname.startsWith(starts)
              && (entryname.lastIndexOf('/') <= starts.length())
              && entryname.endsWith(".class")) {
            String classname = entryname.substring(0, entryname.length() - 6);
            if (classname.startsWith("/"))
              classname = classname.substring(1);
            classname = classname.replace('/', '.');
            try {
              // Try to create an instance of the object
//              Object o = Class.forName(classname).newInstance();
              if (tosubclass.isAssignableFrom(Class.forName(classname))) {
                list.add(classname);
                System.out.println(classname.substring(classname.lastIndexOf('.') + 1));
              }
            } catch (ClassNotFoundException cnfex) {
              System.err.println(cnfex);
            } catch (NullPointerException np) {
              System.err.println(np);
            } catch (ExceptionInInitializerError ip) {
              System.err.println(ip);
            }
          }
        }
      } catch (IOException ioex) {
        System.err.println(ioex);
      }
    }
    return list;
  }

  /**
   * Display all the classes inheriting or implementing a given
   * class in a given package.
   *
   * @param pckgname   the fully qualified name of the package
   * @param tosubclass the Class object to inherit from
   */
  public static void find(String pckgname, Class tosubclass) {
    // Code from JWhich
    // ======
    // Translate the package name into an absolute path
    String name = new String(pckgname);
    if (!name.startsWith("/")) {
      name = "/" + name;
    }
    name = name.replace('.', '/');

    // Get a File object for the package
    URL url = ClassScanner.class.getResource(name);
    // URL url = tosubclass.getResource(name);
    // URL url = ClassLoader.getSystemClassLoader().getResource(name);
    System.out.println(name + "->" + url);

    // Happens only if the jar file is not well constructed, i.e.
    // if the directories do not appear alone in the jar file like here:
    //
    //          meta-inf/
    //          meta-inf/manifest.mf
    //          commands/                  <== IMPORTANT
    //          commands/Command.class
    //          commands/DoorClose.class
    //          commands/DoorLock.class
    //          commands/DoorOpen.class
    //          commands/LightOff.class
    //          commands/LightOn.class
    //          ClassScanner.class
    //
    if (url == null) return;

    File directory = new File(url.getFile());

    // New code
    // ======
    if (directory.exists()) {
      // Get the list of the files contained in the package
      String [] files = directory.list();
      for (int i = 0; i < files.length; i++) {

        // we are only interested in .class files
        if (files[i].endsWith(".class")) {
          // removes the .class extension
          String classname = files[i].substring(0, files[i].length() - 6);
          try {
            // Try to create an instance of the object
//            Object o = Class.forName(pckgname + "." + classname).newInstance();
            if (tosubclass.isAssignableFrom(Class.forName(pckgname + "." + classname))) {
              System.out.println(classname);
            }
          } catch (ClassNotFoundException cnfex) {
            System.err.println(cnfex);
          } catch (NullPointerException np) {
            System.err.println(np);
          } catch (ExceptionInInitializerError ip) {
            System.err.println(ip);
          }
        }
      }
    } else {
      try {
        // It does not work with the filesystem: we must
        // be in the case of a package contained in a jar file.
        JarURLConnection conn = (JarURLConnection) url.openConnection();
        String starts = conn.getEntryName();
        JarFile jfile = conn.getJarFile();
        Enumeration e = jfile.entries();
        while (e.hasMoreElements()) {
          ZipEntry entry = (ZipEntry) e.nextElement();
          String entryname = entry.getName();
          if (entryname.startsWith(starts)
              && (entryname.lastIndexOf('/') <= starts.length())
              && entryname.endsWith(".class")) {
            String classname = entryname.substring(0, entryname.length() - 6);
            if (classname.startsWith("/"))
              classname = classname.substring(1);
            classname = classname.replace('/', '.');
            try {
              // Try to create an instance of the object
//              Object o = Class.forName(classname).newInstance();
              if (tosubclass.isAssignableFrom(Class.forName(classname))) {
                System.out.println(classname.substring(classname.lastIndexOf('.') + 1));
              }
            } catch (ClassNotFoundException cnfex) {
              System.err.println(cnfex);
            } catch (NullPointerException np) {
              System.err.println(np);
            } catch (ExceptionInInitializerError ip) {
              System.err.println(ip);
            }
          }
        }
      } catch (IOException ioex) {
        System.err.println(ioex);
      }
    }
  }


  public static Vector getClassListFromJar(String jarName, String pckname, String subclass) {
//    System.out.println(jarName);
    Vector list = new Vector(0, 1);
    try {
      Class tosubclass = Class.forName(subclass);
      JarFile filesJar = null;
      try {
        filesJar = new JarFile(jarName);
      } catch (Exception e) {
        return list;
      }
      Enumeration jarEntries = filesJar.entries();
      while (jarEntries.hasMoreElements()) {
        String classname = jarEntries.nextElement().toString();
//        System.out.println(classname);
        if (classname.endsWith(".class") && classname.indexOf('$') == -1) {
          classname = classname.substring(0, classname.length() - 6);
          if (classname.startsWith("/"))
            classname = classname.substring(1);
          classname = classname.replace('/', '.');
          int index = classname.lastIndexOf('.');
          String thepackage = "";
          if (index > 0) {
            thepackage = classname.substring(0, index);
          }
//          System.out.println(classname + " : " + thepackage + " : " + pckname + " : " + subclass);
          if (thepackage.startsWith(pckname) && tosubclass.isAssignableFrom(Class.forName(classname))) {
            list.add(classname);
//            System.out.println(classname.substring(classname.lastIndexOf('.') + 1));
          }
        }
      }
      jarEntries = null;
    } catch (Exception ioe) {
      ioe.printStackTrace();
    }

    return list;
  }

}
