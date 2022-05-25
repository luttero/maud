/*
 * @(#)FdtTransformToMBin.java created 23/10/2001 Le Mans
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.data;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.lang.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.awt.*;


/**
 *  The FdtTransformToMBin is a class to read the binary FDT datafile of the INEL banana
 *  detector and transform it reduced in MBin (Maud binaries) to compact data.
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class FdtTransformToMBin extends Object {

  static boolean dontCare = true;

  FdtTransformToMBin() {
  }

  public static void readallSpectra(String datafilename) {

    String[] folderandfilename = Misc.getFolderandName(datafilename);
    DataInputStream reader = getDataBufferedInputStream(datafilename);
    if (reader != null) {

      int index = folderandfilename[1].toLowerCase().indexOf(".fdt");
      String outdatafilename = folderandfilename[0];
      if (index > 0)
        outdatafilename += folderandfilename[1].substring(0, index) + ".mbi";
      else
        return;
      DataOutputStream writer = getDataBufferedOutputStream(outdatafilename);

      try {

// Header 16448 bytes
        short TailFDT = Misc.readShortLittleEndian(reader); // *1024 = size of the block of data

        int actualnumberofdata = TailFDT * 1024;

        short nb_scan = Misc.readShortLittleEndian(reader); // number of blocks

        int[] indexSkip = getRangeSkipData(actualnumberofdata, nb_scan);

        float[] xvalues = new float[indexSkip[1] - indexSkip[0]];
        int[] yvalues = new int[indexSkip[1] - indexSkip[0]];
        for (int k = indexSkip[0]; k < indexSkip[1]; k++)
          xvalues[k - indexSkip[0]] = k;

        byte Commentaires1[] = new byte[80];
        reader.read(Commentaires1);  // comments
        byte Commentaires2[] = new byte[80];
        reader.read(Commentaires2);  // comments                        164
        float Tilt = Misc.readFloatLittleEndian(reader); // not used
        float Azim = Misc.readFloatLittleEndian(reader); // not used    172
        int DataN = Misc.readIntLittleEndian(reader); // not used       176
        boolean Sectors = reader.readBoolean(); // not used
        int Number = Misc.readIntLittleEndian(reader); // not used
        byte Sample[] = new byte[20];
        reader.read(Sample); // not used
        byte Plane[] = new byte[9];
        reader.read(Plane); // not used                                 210
        byte date1[] = new byte[20];
        reader.read(date1); // date of file creation
        reader.read(date1); // date of file creation, second time?      250
        byte Meas_Type[] = new byte[6];
        reader.read(Meas_Type); // not used                             256
        boolean bkgMeas = reader.readBoolean(); // not used
        double WL = Misc.readDoubleLittleEndian(reader); // wavelength  265
        byte Volt[] = new byte[9];
        reader.read(Volt); // voltage
        byte Curr[] = new byte[9];
        reader.read(Curr); // current
        byte ColW[] = new byte[6];
        reader.read(ColW); // not used
        byte ColH[] = new byte[6];
        reader.read(ColH); // not used
        byte DetW[] = new byte[6];
        reader.read(DetW); // not used
        byte DetH[] = new byte[6];
        reader.read(DetH); // not used                                  307
        boolean FullOK = reader.readBoolean(); // true if the whole channel domain is used,
// if not, limits are the 2 following
        int bytesToSkip = 15885;
        if (!FullOK) {
          short Limlow = Misc.readShortLittleEndian(reader); // low channel limit if FullOK is true
          short Limhigh = Misc.readShortLittleEndian(reader); // high channel limit if FullOK is true
          bytesToSkip -= 4;
        }
//                        312

        StringBuffer calchar = new StringBuffer();
        for (int i = 0; i < 255; i++)
          calchar.append(Misc.readCharOneByte(reader)); // calibration filename

        String calName = calchar.toString();
        index = calName.toLowerCase().lastIndexOf(".cal");
        if (index > 0)
          calName = calName.substring(0, index) + ".cal";
        String calibrationFile = Misc.filterFileName(calName);

        DataInputStream calreader = getDataBufferedInputStream(calibrationFile);

        boolean calibrated = false;

        if (calreader == null) {
          index = calibrationFile.lastIndexOf("/");
          calibrationFile = calibrationFile.substring(index + 1);
          calreader = getDataBufferedInputStream(folderandfilename[0], calibrationFile);
        }
        if (calreader == null) {  // still we don't find the calibration file, asking to the user
          calibrationFile = Utility.browseFilename(new Frame(),
                  "Choose the calibration file for the INEL fdt datafile");
          calibrationFile = Misc.filterFileName(calibrationFile);
          calreader = getDataBufferedInputStream(calibrationFile);
        }
        if (calreader != null) {
          try {
            short rien1 = Misc.readShortLittleEndian(calreader); // not used = 25675
            double Xlambda = Misc.readDoubleLittleEndian(calreader); // wavelength
            double Averr = Misc.readFloatLittleEndian(calreader); // case polynomial (no more used)
            double[] A1 = new double[11];
            for (int ij = 0; ij < 11; ij++)
              A1[ij] = Misc.readDoubleLittleEndian(calreader); 	// array of parameter for the polynomial
// (no more used)
            short rien2 = Misc.readShortLittleEndian(calreader); // not used = 32156
            short ICSPL = Misc.readShortLittleEndian(calreader); // if 0 then polynomial,
// if 1 then spline calibration
            short NV = Misc.readShortLittleEndian(calreader);    // number of experimental points used
// for spline calibration

            if (NV > 100) NV = 100; // maximum 100
            double[] Xpeak = new double[100];
            int mult16 = 0;
            for (int ij = 0; ij < 100; ij++) {
              mult16++;
              if (mult16 == 16) {
                mult16 = 0;
                Misc.readShortLittleEndian(calreader);
              }
              Xpeak[ij] = Misc.readDoubleLittleEndian(calreader); 	// array of peak position of standard
// in channel units, each multiple of 16, read Integer[2] of unused
            }
            double[][] ASPL = new double[4][100];
//            mult16 = 0;
            for (int ij = 0; ij < 100; ij++) {
              for (int kl = 0; kl < 4; kl++) {
                mult16++;
                if (mult16 == 16) {
                  mult16 = 0;
                  Misc.readShortLittleEndian(calreader);
                }
                ASPL[kl][ij] = Misc.readDoubleLittleEndian(calreader);
              }
// double array [1 to 4,1 to 100] of spline parameters.
// Each multiple of 16, read Integer[2].
// The column ASPL[1,i] is the 2theta values corresponding to the theoretical
// position of the peak position of standard.
            }
            calibrated = true;
            for (int k = indexSkip[0]; k < indexSkip[1]; k++) {
              xvalues[k - indexSkip[0]] = (float) getSplineCalibration(0.001 * (k + 1), Xpeak, ASPL, NV);
            }

          } catch (IOException ioe) {
            ioe.printStackTrace();
            System.out.println("Not able to load the calibration file: " + calName);
          }
          try {
            calreader.close();
          } catch (IOException ioe) {
          }
        } else
          System.out.println("Not able to find the calibration file: " + calName);



//        16444 still to read as we readed only to shorts
//        reader.skipBytes(16444); // reading the rest of the header
//        15881 to read
        reader.skipBytes(bytesToSkip); // reading the rest of the header

        boolean toSave = true;
//        int numberspectra = (indexSkip[3] - indexSkip[2]);
//        if (indexSkip[4] > 0)
//          numberspectra = (int) (1.0 * (indexSkip[4] - 1) * numberspectra / indexSkip[4]);

        float chiStatus = anglesAndSkip[0];
        float phiStatus = anglesAndSkip[3];
        for (int i = 0; i < nb_scan; i++) {

          byte dateFDT[] = new byte[14];
          reader.read(dateFDT); // date and time of the block
          float tacq = Misc.readFloatLittleEndian(reader); // acquisition time in seconds

          double twotheta = 0.0;
          for (int j = 0; j < actualnumberofdata; j++) {
            int scan = Misc.readIntLittleEndian(reader);
            if (j >= indexSkip[0] && j < indexSkip[1])
              yvalues[j - indexSkip[0]] = scan;
          }

          float ValMot[] = new float[7];
          ValMot[0] = Misc.readFloatLittleEndian(reader); // theta/temperature
          ValMot[1] = Misc.readFloatLittleEndian(reader); // Phi
          ValMot[2] = Misc.readFloatLittleEndian(reader); // 2theta/rampe
          ValMot[3] = Misc.readFloatLittleEndian(reader); // Chi
          ValMot[4] = Misc.readFloatLittleEndian(reader); // Tr. X
          ValMot[5] = Misc.readFloatLittleEndian(reader); // Tr. Y
          ValMot[6] = Misc.readFloatLittleEndian(reader); // Tr. Z
          float humid = Misc.readFloatLittleEndian(reader); // humidity
          float weight_factor = Misc.readFloatLittleEndian(reader); // weight factor
          byte lab_sc[] = new byte[10];  // 1 byte more to arrive at end
          reader.read(lab_sc); // label of the scan

          if (Math.abs(ValMot[2] + 23.8) < 0.1) {
            System.out.println("2theta offset is probably wrong: " + ValMot[2] + ", we reset it to zero!");
            ValMot[2] = 0;
          }
          if (toSave) {
            for (int k = indexSkip[0]; k < indexSkip[1]; k++)
              xvalues[k - indexSkip[0]] += ValMot[2];
            writeInOutput(writer, (short) (indexSkip[1] - indexSkip[0]), xvalues);
            toSave = false;
          }

          if (dontCare) {
            writeInOutput(writer, ValMot[0], ValMot[3], ValMot[1], yvalues);
          } else {
          if (ValMot[3] <= anglesAndSkip[0] + 0.0001) {
            if (chiStatus != anglesAndSkip[0]) {
              System.out.println("Setting chi to: " + anglesAndSkip[0]);
              System.out.println("Chi readed: " + ValMot[3]);
              if (Math.abs(ValMot[3] + 24.364) > .0001) // otherwise mistake in an fdt file
                chiStatus = anglesAndSkip[0];
              else
                System.out.println("This chi is wrong, we keep the last one!");
            }
          }
          if (ValMot[3] > chiStatus + 0.0001)
            chiStatus += anglesAndSkip[2];
          if (ValMot[1] <= anglesAndSkip[3] + 0.0001) {
            if (phiStatus != anglesAndSkip[3]) {
              System.out.println("Resetting phi to (and of girdle reached?): " + anglesAndSkip[3]);
              System.out.println("Phi readed: " + ValMot[1]);
              phiStatus = anglesAndSkip[3];
            }
          }
          if (ValMot[1] > phiStatus + 0.0001)
            phiStatus += anglesAndSkip[5];

          if ((ValMot[1] <= anglesAndSkip[4] + 0.0001) && (ValMot[3] <= anglesAndSkip[1] + 0.0001))
            if ((Math.abs(ValMot[3] - chiStatus) < 0.0001) && (Math.abs(ValMot[1] - phiStatus) < 0.0001))
              writeInOutput(writer, ValMot[0], ValMot[3], ValMot[1], yvalues);
        }
        }

      } catch (Exception e) {
        e.printStackTrace();
        System.out.println("Error in loading the data file! Try to remove this data file");
      }
      try {
        reader.close();
      } catch (IOException e) {
      }
      try {
        writer.flush();
        writer.close();
      } catch (IOException e) {
      }
    }
  }

  public static void writeInOutput(DataOutputStream writer, short numberdata, float[] xvalues) {
    try {
      writer.writeShort(MBinDataFile.version);
      writer.writeShort(numberdata);
//      writer.writeShort(numberspectra);

      for (int i = 0; i < numberdata; i++)
        writer.writeFloat(xvalues[i]);
    } catch (IOException e) {
      e.printStackTrace();
      System.out.println("Error on writing data, xvalues");
    }
  }

  public static void writeInOutput(DataOutputStream writer, float omega, float chi, float phi, int[] yvalues) {
    short numberdata = (short) yvalues.length;
    try {
      writer.writeFloat(omega);
      writer.writeFloat(chi);
      writer.writeFloat(phi);
      for (int i = 0; i < numberdata; i++)
        writer.writeInt(yvalues[i]);
    } catch (IOException e) {
      e.printStackTrace();
      System.out.println("Error on writing data, xvalues");
    }
  }

  public static double getSplineCalibration(double XVal, double[] Xpeak, double[][] ASPL, short NV) {
    double xvalue = 0.0;
    if (NV != 0) {
      if (XVal < Xpeak[0])
        xvalue = ASPL[0][0] + ASPL[1][0] * (XVal - Xpeak[0]);
      else {
        if (XVal > Xpeak[NV - 1])
          xvalue = ASPL[0][NV - 1] + ASPL[1][NV - 1] * (XVal - Xpeak[NV - 1]);
        else {
          int itc = -1;
          do {
            itc++;
          } while (!((XVal >= Xpeak[itc]) && (XVal <= Xpeak[itc + 1])));
          double XR = XVal - Xpeak[itc];
          xvalue = ASPL[3][itc];
          int jtc = 2;
          do {
            xvalue = XR * xvalue + ASPL[jtc][itc];
            jtc--;
          } while (jtc >= 0);
        }
      }
    }
    return xvalue;
  }

  public static DataInputStream getDataBufferedInputStream(String filename) {
    return getDataBufferedInputStream("", filename);
  }

  public static DataInputStream getDataBufferedInputStream(String folder, String filename) {
    InputStream in = Misc.getInputStream(folder, filename);
    if (in != null)
      return new DataInputStream(new BufferedInputStream(in));
    return null;
  }

  public static DataOutputStream getDataBufferedOutputStream(String filename) {
    return getDataBufferedOutputStream("", filename);
  }

  public static DataOutputStream getDataBufferedOutputStream(String folder, String filename) {
    OutputStream out = Misc.getOutputStream(folder, filename);
    if (out != null)
      return new DataOutputStream(new BufferedOutputStream(out));
    return null;
  }

  static int[] indexAndSkip = new int[2];
  static float[] anglesAndSkip = new float[6];

  public static int[] getRangeSkipData(int datanumber, int spectranumber) {
    indexAndSkip[0] = 0;
    indexAndSkip[1] = datanumber;
    anglesAndSkip[0] = 0f;
    anglesAndSkip[1] = 90.0f;
    anglesAndSkip[2] = 5.0f;
    anglesAndSkip[3] = 0f;
    anglesAndSkip[4] = 359.9f;
    anglesAndSkip[5] = 5.0f;
    dontCare = true;
    (new rangeAndSkipD()).setVisible(true);
    return indexAndSkip;
  }

  public static class rangeAndSkipD extends myJDialog {

    JTextField[] textFields = new JTextField[8];

    public rangeAndSkipD() {
      super(new Frame(), "Datafile loading options", true);

      Container c1 = getContentPane();
      c1.setLayout(new BorderLayout(6, 6));

      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      c1.add(BorderLayout.NORTH, jp);

      JTextArea textArea = new JTextArea();
      JScrollPane scrollarea = new JScrollPane(textArea);

      jp.add(scrollarea);
      textArea.setText("To reduce memory consumption it is possible to specify a\nreduced range for data or skip spectra in loading");

      JPanel tfPanel = new JPanel();
      tfPanel.setLayout(new GridLayout(0, 2, 3, 3));
      c1.add(BorderLayout.CENTER, tfPanel);
      tfPanel.add(new JLabel("First channel: "));
      tfPanel.add(textFields[0] = new JTextField(15));
      tfPanel.add(new JLabel("Last channel:  "));
      tfPanel.add(textFields[1] = new JTextField(15));
      tfPanel.add(new JLabel("Starting Chi:"));
      tfPanel.add(textFields[2] = new JTextField(15));
      tfPanel.add(new JLabel("Ending Chi  : "));
      tfPanel.add(textFields[3] = new JTextField(15));
      tfPanel.add(new JLabel("Chi step    :"));
      tfPanel.add(textFields[4] = new JTextField(15));
      tfPanel.add(new JLabel("Starting Phi:"));
      tfPanel.add(textFields[5] = new JTextField(15));
      tfPanel.add(new JLabel("Ending Phi  : "));
      tfPanel.add(textFields[6] = new JTextField(15));
      tfPanel.add(new JLabel("Phi step    :"));
      tfPanel.add(textFields[7] = new JTextField(15));

      jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      c1.add(BorderLayout.SOUTH, jp);
      JButton button2 = new JButton("I don't care.");
      jp.add(button2);
      button2.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          dontCare = true;
          dispose();
        }
      });
      JButton jbok = new JButton("Cut it!");
      jp.add(jbok);
      jbok.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          dontCare = false;
          getResult();
          dispose();
        }
      });
      setData();
      pack();

    }

    public void setData() {
      for (int i = 0; i < 2; i++)
        textFields[i].setText(Integer.toString(indexAndSkip[i]));
      for (int i = 0; i < 6; i++)
        textFields[i + 2].setText(Float.toString(anglesAndSkip[i]));
    }

    public void getResult() {
      for (int i = 0; i < 2; i++)
        indexAndSkip[i] = Integer.valueOf(textFields[i].getText()).intValue();
      for (int i = 0; i < 6; i++)
        anglesAndSkip[i] = Float.valueOf(textFields[i + 2].getText()).floatValue();
    }
  }
}


