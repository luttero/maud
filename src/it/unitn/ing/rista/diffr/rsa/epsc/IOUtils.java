package it.unitn.ing.rista.diffr.rsa.epsc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

/**
 * Utility class for handling file I/O,
 * managing Fortran-style I/O units.
 */
public class IOUtils {

    public static PrintWriter writer11 = null;
  public static PrintWriter writer12 = null;
  public static PrintWriter writer13 = null;
  public static PrintWriter writer14 = null;
    public static PrintWriter writer15 = null;
    public static PrintWriter writer16 = null;
    public static PrintWriter writer17 = null;
    public static PrintWriter writer18 = null;
    public static PrintWriter writer19 = null;
    public static PrintWriter writer20 = null;
//    public static PrintWriter writer22;
    public static PrintWriter writer77 = null;

    public static String path = "";

    public static void setPath(String apath) {
      path = apath;
    }
    
/**
     * The scanner for FORTRAN unit 1.
     * This is opened by dataCrystal and RE-USED by crssVoce.
     * It is REPLACED by other routines that open unit 1.
     */
    public static Scanner scanner1; // For reading unit 1

/**
     * Initializes all static writers.
     * Scanners must be initialized by the routines that use them.
     */
    public static void initializeIO() {
        try {
            writer11 = new PrintWriter(new FileWriter(path + "epsc1.out"), true);
          writer12 = new PrintWriter(new FileWriter(path + "epsc2.out"), true);
//          writer13 = new PrintWriter(new FileWriter(path + "epsc3.out"), true);
          writer14 = new PrintWriter(new FileWriter(path + "epsc4.out"), true);
            writer15 = new PrintWriter(new FileWriter(path + "epsc5.out"), true); // Or "EPSC15.OUT"
            writer16 = new PrintWriter(new FileWriter(path + "epsc6.out"), true); // Or "EPSC16.OUT"
            writer17 = new PrintWriter(new FileWriter(path + "epsc7.out"), true);
            writer18 = new PrintWriter(new FileWriter(path + "epsc8.out"), true); // Or "EPSC18.OUT"
            writer19 = new PrintWriter(new FileWriter(path + "epsc9.out"), true);
            writer20 = new PrintWriter(new FileWriter(path + "epsc10.out"), true); // Guessed filename
//            writer22 = new PrintWriter(new FileWriter("tex_default.out"), true); // Default name
            writer77 = new PrintWriter(new FileWriter(path + "auxiliar.out"), true);

            // DO NOT INITIALIZE scanner1 here anymore.
            // data_crystal will do it.
            
        } catch (IOException e) {
            System.err.println("Failed to initialize writers: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Closes all managed writers and scanners.
     */
    public static void closeIO() {
        if (writer11 != null) writer11.close();
      if (writer12 != null) writer12.close();
      if (writer13 != null) writer13.close();
      if (writer14 != null) writer14.close();
        if (writer15 != null) writer15.close();
        if (writer16 != null) writer16.close();
        if (writer17 != null) writer17.close();
        if (writer18 != null) writer18.close();
        if (writer19 != null) writer19.close();
        if (writer20 != null) writer20.close();
//        if (writer22 != null) writer22.close();
        if (writer77 != null) writer77.close();

        // This will close the LAST scanner that was opened on unit 1
        if (scanner1 != null) scanner1.close(); // Still close it
    }
    
    /**
     * Prints a 6x6 matrix (using 1-based indexing) to a PrintWriter.
     * Mimics Fortran format (6d12.4).
     * @param out The PrintWriter (e.g., writer11).
     * @param title A title to print before the matrix.
     * @param matrix The 7x7 array (using indices 1..6).
     */
    public static void printMatrix(PrintWriter out, String title, double[][] matrix) {
        if (out == null) return;
        out.println();
        out.println(title);
        for (int i = 1; i <= 6; i++) {
            for (int j = 1; j <= 6; j++) {
                // Ensure matrix and its rows are not null before access
                if (matrix != null && matrix[i] != null && matrix[i].length > j) {
                    out.printf("%12.4f", matrix[i][j]);
                } else {
                    out.printf("%12s", "null"); // Handle null/short arrays
                }
            }
            out.println();
        }
    }

    public static Vector<String> stringArrayFromLine(String line) {
      Vector<String> sArray = new Vector<>();
      StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
      while (st.hasMoreTokens())
        sArray.add(st.nextToken());
      return sArray;
    }
}