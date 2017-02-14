// To construct a new class to load a different data format just modify this file
// and save it under a new filename as it is specified in the following.
// You need to modify the file following the suggestions that start with
// "user modification".
// Important: this works only for ASCII files.
// In java the double slash indicate a comment after the double slash up
// to the end of the line
// Others comments are between the symbol "/*" and the other symbol "*/" that
// represents the end of the comment

/*
 * @(#)VeneziaDataFile.java 04/02/1998 Trento
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

// the package name

package it.unitn.ing.rista.diffr.data;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.Misc;

import java.io.*;
import java.lang.*;
import java.util.*;

/*
 * The VeneziaDataFile is a class to load the Venezia data format
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

/* user modification: change the class name as you prefer; my convention is
   to use *DataFile where the * correspond to the special format. In this case
   is VeneziaDataFile. Put something like MyformatDataFile.
   The same class name must be used as name of the file adding a .java
   as extension. In this case the file name is VeneziaDataFile.java
*/

public class VeneziaDataFile extends DiffrDataFile {

  public VeneziaDataFile(XRDcat aobj, String alabel) {
    super(aobj, alabel);

    // user modification: put here the extension for the file format
    // you want to use. In this case this format is used when the
    // data file has the extension .dat

    identifier = ".dat";
  }

  public VeneziaDataFile() {
    // user modification: put here the extension for the file format
    // you want to use. In this case this format is used when the
    // data file has the extension .dat

    identifier = ".dat";
  }

/* This routine is called when the user load a file having the extension
   specified by the variable identifier.
   The method is responsible for loading the data from the file and put
   anything where it is needed.
   To load data you can use the following method:

      String astring = reader.readLine();
              this will read an entire line from the ASCII file and
              put all the line in the String variable "astring"
              Next time you call the reader.readLine() method it will
              read the next line in the file.

   Then you have to parse the data in the astring variable. To do that you
   can use the class StringTokenizer:

        StringTokenizer st = new StringTokenizer(astring, " ,\t\r\n");
              this create a StringTokenizer (a sort of object able to
              recognize the different entries or data using the specified
              type of data separator) that recognize as data separator
              a space (" "), a tab ("\t"), a carriage return ("\r") or
              a line feed ("\n").
   After creating a StringTokenizer for the astring variable you can
   call the nextToken() method to read from the StringTokenizer object
   the next String up to the next encounter of a separator. Example:

        String anotherstring = st.nextToken();
              this will read a partial string from the last reading point
              in the StringTokenizer object to the next separator encountered.
              The string is loaded in the variable anotherstring.

   There is another convenient method in StringTokenizer:
   hasMoreToken()
   it will return true if the StringTokenizer object has more data in it
   or false otherwise. Example of use:

              boolean moredata = st.hasMoreToken();

   The data you read using readLine() and nextToken() is in String format;
   if you need to transform it in integer, floating or double you can use the
   following methods:

         int i = Integer.valueOf(s).intValue();
              transform the string s in the integer i
         double d = Double.valueOf(s).doubleValue();
              transform the string s in the double d

   In this class you need to provide a value for the following variables (defined
   elsewhere); not all variable need really a value, only the ones labeled required:

   String title;                  required, the title of the data
   int datasetsNumber;                required, number of data
   double measurementstep;        measurement step (only if the step is constant)
   double startingvalue;          not required, starting value for 2Theta or d-spacing
   double radiation;              not required
   boolean constantstep;          boolean value equal to true if the measuring step is
                                  constant by default is true
   boolean dspacingbase;          boolean value equal to true if data is in d-spacing
                                  or false if in 2Theta; by default is false.

   What you need to do also is:

   init all the vectors that will contain the data, this can be done by calling
   initData(datasetsNumber);
   after you have specified the number of Data in the variable datasetsNumber and before to
   put the intensity values (and 2Theta or d-spacing values)

   put the intensity, 2Theta or d-spacing and statistical weight values using:

   setXData(i, x);       this put the 2Theta or d value "x" (a double) at the index "i"
                         (an integer)
   setYData(i, intensity);  this put the intensity "intensity" (a double) at the index "i"
                            (an integer)
   setWeight(i, wgt);      specify the statistical weight "wgt" (a double) for the data
                           at the index "i" (an integer).

   Just have a look on the following java code for the Venezia format example:
   the Venezia format consist of two different variants: one column of data or two columns

                               ------ one column -------
   title(first line)
   number_of_data scanning_step starting_2Theta radiation_wavelength(dummy) number_of_columns
   intensity(1)
   intensity(2)
   .....
   intensity(number_of_data)

                               ------ two column -------

   title
   number_of_data scanning_step(dummy) starting_2Theta(dummy) radiation_wavelength(dummy) 2
   2Theta(1) intensity(1)
   2Theta(2) intensity(2)
   .....
   2Theta(number_of_data) intensity(number_of_data)

   The datafile alzrc.dat is an example of the Venezia data format with one column
*/

  public boolean readallSpectra() {

    boolean loadSuccessfull = false;
    boolean tmpB = isAbilitatetoRefresh;
    isAbilitatetoRefresh = false;
    BufferedReader reader = getReader();
    if (reader != null) {
      try {

// Variable definitions

        String token;
        String line;
        StringTokenizer st;

// user modification: do not modify before this line
// we read the Venezia format

// the data is in 2Theta

        dspacingbase = false;

        // read the entire first line and use it as title

        title = reader.readLine();

        line = reader.readLine();    // read the second line

// now the program read from the String line the values reported above

// initialize the StringTokenizer object from the String line

        st = new StringTokenizer(line, " ,\t\r\n");

// read from the StringTokenizer the first data (the number_of_data) and convert it
// in an integer to be putted in the variable datasetsNumber.

        token = st.nextToken();
        datanumber = Integer.valueOf(token).intValue();

// read from the same StringTokenizer the next data etc.

        measurementstep = Double.valueOf(token = st.nextToken()).doubleValue();
        startingvalue = Double.valueOf(token = st.nextToken()).doubleValue();
        radiation = Double.valueOf(token = st.nextToken()).doubleValue();

// in Java variable can be defined directly where you need them as here for
// columnnumber

        int columnnumber = Integer.valueOf(token = st.nextToken()).intValue();

        initData(datanumber);        // initialize all the vectors specifying the
        // dimension equal to the number of Data
        // It is necessary before to load the intensities
        // and coordinates (2Theta or d-spacing)


// now we read the data, in this case one column (only intensities) or two columns
// (2Theta_or_d-spacing Intensity)

        int i;
        switch (columnnumber) {
          case 2:                          // two columns
            constantstep = false;           // the step may be is not constant
            i = 0;
            while (i < datanumber) {

// for any line we read only the first two values; the first is the
// x coordinate (2Theta or d-spacing) and the second is the intensity

              line = reader.readLine();
              st = new StringTokenizer(line, " ,\t\r\n");

              while (st.hasMoreTokens()) {
// The x value is readed and putted where it is needed using the method
// setXData(int index, double value)

                setXData(i, Double.valueOf(st.nextToken()).doubleValue());

// For the intensity we put it directly in the vector of the intensities
// using the method setYData(int index, double value)

                setYData(i, Double.valueOf(st.nextToken()).doubleValue());

// now we compute the statistical weight for the intensity; in this case
// is equal to the inverse of the square root of the intensity

                double tmpweight = Math.sqrt(intensity[i]);
                if (tmpweight != 0.0)
                  setWeight(i, 1.0 / tmpweight);
                else
                  setWeight(i, 1.0);  // Intensity is zero or less, the statical weight is
                // forced to 1.
                i++;
              }
            }
            break;    // case 2 end here
          default:  // the column has the value 1 or different from 2
            {
              constantstep = true;         // We assume a constant step
              i = 0;
              while (i < datanumber) {
                line = reader.readLine();
                st = new StringTokenizer(line, " ,\t\r\n");

// only one column, the first, is used; the x value is computed from the
// starting value of 2Theta or d-spacing and from the measurement step
                while (st.hasMoreTokens()) {
                  setXData(i, startingvalue + measurementstep * i);
                  double intensityValue = Double.valueOf(st.nextToken()).doubleValue();
                  setYData(i, intensityValue);
                  double tmpweight = Math.sqrt(intensity[i]);
                  if (tmpweight != 0.0)
                    setWeight(i, 1.0 / tmpweight);
                  else
                    setWeight(i, 1.0);
                  i++;
                }
              }
            }
        }
        loadSuccessfull = true;

// user modification: do not modify after this line

      } catch (Exception e) {
        e.printStackTrace();
        System.out.println("Error in loading the data file! Try to remove this data file");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
    }
    isAbilitatetoRefresh = tmpB;
    return loadSuccessfull;
  }

}
