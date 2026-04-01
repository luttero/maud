package gov.lanl.epsc4;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;
import java.util.Scanner;

/**
 * Utility class for handling file I/O,
 * managing Fortran-style I/O units.
 */
public class IOUtils {

    /**
     * The scanner for FORTRAN unit 1.
     * This is opened by dataCrystal and RE-USED by crssVoce.
     * It is REPLACED by other routines that open unit 1.
     */
    public static Scanner scanner1;

    public static PrintWriter writer11, writer12, writer13, writer14, writer15;
    public static PrintWriter writer16, writer17, writer18, writer19, writer20;
    public static PrintWriter writer22, writer77;

    /**
     * Initializes all static writers.
     * Scanners must be initialized by the routines that use them.
     */
    public static void initializeIO() {
        try {
            writer11 = new PrintWriter(new FileWriter("epsc1.out"), true);
            // Route unit 12 to the console (System.out)
            writer12 = new PrintWriter(System.out, true); 
            writer13 = new PrintWriter(new FileWriter("epsc3.out"), true);
            writer14 = new PrintWriter(new FileWriter("epsc4.out"), true);
            writer15 = new PrintWriter(new FileWriter("epsc5.out"), true);
            writer16 = new PrintWriter(new FileWriter("epsc6.out"), true);
            writer17 = new PrintWriter(new FileWriter("epsc7.out"), true);
            writer18 = new PrintWriter(new FileWriter("epsc8.out"), true);
            writer19 = new PrintWriter(new FileWriter("epsc9.out"), true);
            writer20 = new PrintWriter(new FileWriter("epsc10.out"), true);
            writer22 = new PrintWriter(new FileWriter("tex_default.out"), true);
            writer77 = new PrintWriter(new FileWriter("auxiliar.out"), true);
        } catch (IOException e) {
            System.err.println("Failed to initialize writers: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * Closes all managed writers and scanners.
     */
    public static void closeIO() {
        if (scanner1 != null) scanner1.close();
        
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
        if (writer22 != null) writer22.close();
        if (writer77 != null) writer77.close();
    }

    /**
     * Prints a 6x6 matrix (using 1-based indexing) to a PrintWriter.
     * Mimics Fortran format (6d12.4).
     *
     * @param out    The PrintWriter (e.g., writer11).
     * @param matrix The 7x7 array (using indices 1..6).
     */
    public static void printMatrix(PrintWriter out, double[][] matrix) {
        if (out == null) return;
        for (int i = 1; i <= 6; i++) {
            out.printf(Locale.US, " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                    matrix[i][1], matrix[i][2], matrix[i][3],
                    matrix[i][4], matrix[i][5], matrix[i][6]);
        }
    }

    /**
     * Prints a 6x6 matrix (using 1-based indexing) to a PrintWriter
     * with a title.
     *
     * @param out    The PrintWriter (e.g., writer11).
     * @param title  A title to print before the matrix.
     * @param matrix The 7x7 array (using indices 1..6).
     */
    public static void printMatrix(PrintWriter out, String title, double[][] matrix) {
        if (out == null) return;
        out.println();
        out.println(title);
        printMatrix(out, matrix);
    }

    /**
     * Prints a 6x1 vector (using 1-based indexing) to a PrintWriter.
     *
     * @param out    The PrintWriter (e.g., writer14).
     * @param vector The 7-element array (using indices 1..6).
     */
    public static void printVector(PrintWriter out, double[] vector) {
        if (out == null) return;
        out.printf(Locale.US, " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                vector[1], vector[2], vector[3],
                vector[4], vector[5], vector[6]);
    }

    /**
     * Prints a 6x1 vector from a grain's 2D array [component][grain_id].
     *
     * @param out    The PrintWriter (e.g., writer14).
     * @param matrix The 2D array (e.g., stcs).
     * @param ng     The grain index to print.
     */
    public static void printGrainVector(PrintWriter out, double[][] matrix, int ng) {
        if (out == null) return;
        out.printf(Locale.US, " %12.4e %12.4e %12.4e %12.4e %12.4e %12.4e%n",
                matrix[1][ng], matrix[2][ng], matrix[3][ng],
                matrix[4][ng], matrix[5][ng], matrix[6][ng]);
    }
}
