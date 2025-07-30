/*
 * @(#)SumImagesByRules.java created Feb 7, 2025 Casalino
 *
 * Copyright (c) 2025 Luca Lutterotti All Rights Reserved.
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
package ij.plugin;

import ij.gui.*;
import ij.io.*;
import ij.*;
import ij.measure.Calibration;
import ij.plugin.frame.Recorder;
import ij.process.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.awt.image.ColorModel;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.io.StringNumber;
import it.unitn.ing.rista.util.*;

import javax.swing.*;

import static ij.IJ.*;

/**
 * The SumImagesByRules is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Feb 7, 2025 20:45:12 PM $
 * @since JDK1.1
 */

public class SumImagesByRules implements PlugIn {

  public void run(final String string) {
    if (string.equals("about"))
      showAbout();
    else {
      (new Thread() {
        public void run() {
          AnglesSelectDialog dialog = new AnglesSelectDialog("Sum images for the selected angle", IJ.getInstance());
          dialog.showDialog();
        }
      }).start();
    }
  }

  public String[] items = {"2Theta", "Omega", "Chi", "Phi", "Energy", "all"};

  public void sumImages(File[] files, int angleIndex) {
    Vector<File> filesV = new Vector<>(files.length, 10);
    Vector<double[]> anglesV = new Vector<>(files.length, 10);
    for (int i = 0; i < files.length; i++) {
      double[] angles = StringNumber.checkAngles(files[i].getName());
      anglesV.addElement(angles);
      filesV.addElement(files[i]);
    }
    while (sumFirstImage(filesV, anglesV, angleIndex));
  }

  public boolean sumFirstImage(Vector<File> filesV, Vector<double[]> anglesV, int angleIndex) {
    if (filesV.size() < 1)
      return false;
    try {
      int anglesNumber = items.length;
      boolean[] sumAngle = new boolean[anglesNumber];
      for (int i = 0; i < anglesNumber; i++) {
        if (i == angleIndex)
          sumAngle[i] = true;
        else
          sumAngle[i] = false;
      }

      String nameToSave = filesV.elementAt(0).getCanonicalPath();
      ImagePlus imp = IJ.openImage(nameToSave);
//      Vector<Integer> removeV = new Vector<>(filesV.size());
//      removeV.addElement(0);
      if (imp != null) {
        float[][] buffer = null;
        ImageProcessor ip = imp.getChannelProcessor();
        int width = ip.getWidth();
        int height = ip.getHeight();
        float[][] pixels = ip.getFloatArray();
          buffer = new float[width][height];
          for (int j = 0; j < width; j++)
            for (int k = 0; k < height; k++)
              buffer[j][k] = pixels[j][k];
        imp.close();
        double[] anglesFirst = anglesV.elementAt(0);
//        System.out.println(filesV.elementAt(0).getName());
        filesV.remove(0);
        anglesV.remove(0);
        for (int i = 0; i < filesV.size(); i++) {
          imp = IJ.openImage(filesV.elementAt(i).getCanonicalPath());
          ip = imp.getChannelProcessor();
          int width_n = ip.getWidth();
          int height_n = ip.getHeight();
          boolean toSum = true;
          if (!sumAngle[sumAngle.length - 1]) {
            for (int j = 0; j < anglesNumber; j++) {
              toSum = toSum && (sumAngle[j] || MoreMath.areSimilar(anglesV.elementAt(i)[j], anglesFirst[j]));
//            System.out.println(j + " " + toSum + " " + sumAngle[j] + " " + anglesV.elementAt(i)[j] + " == " + anglesFirst[j]);
            }
          }
          if (toSum) {
  //          System.out.println(" sum with " + filesV.elementAt(i).getName());
            if (width_n == width && height_n == height) {
              pixels = ip.getFloatArray();
              for (int j = 0; j < width; j++)
                for (int k = 0; k < height; k++)
                  buffer[j][k] += pixels[j][k];
//              removeV.add(i);
            }
            filesV.remove(i);
            anglesV.remove(i);
            i--;
          }
        }
//        System.out.println(" Save image " + nameToSave);
        saveImage(nameToSave, buffer);
      } else {
        filesV.remove(0);
        anglesV.remove(0);
      }
/*      for (int i = removeV.size() - 1; i >= 0; i--) {
        filesV.remove(removeV.elementAt(i));
        anglesV.remove(removeV.elementAt(i));
      }*/

    } catch (Exception e) {
      System.out.println("Some files do not contain a picture or some errors happened, check the error trace following!");
      e.printStackTrace();
      return false;
    }

    return true;
  }


  /**
   * Opens the image. Displays it if 'show' is
   * true. Returns an ImagePlus object if successful.
   */
  public void saveImage(String nameAndPath, float[][] buffer) {
    int dotLocation = nameAndPath.lastIndexOf(".");
    String filename1 = nameAndPath.substring(0, dotLocation);

    filename1 = filename1 + "_sum.tif";
    ImageProcessor ip = new FloatProcessor(buffer);
    ImagePlus imp = new ImagePlus(filename1, ip);
    Calibration cal = imp.getCalibration();
//    if (cal.pixelWidth <= 0.0 || cal.pixelHeight <= 0.0) {
      cal.pixelWidth = MaudPreferences.getDouble("pixelDetector.pixelWidth", 0.075);
      cal.pixelHeight = MaudPreferences.getDouble("pixelDetector.pixelHeight", 0.075);
//    }
//    if (cal.getUnit() == null)
      cal.setUnit("mm");
      imp.setCalibration(cal);
    FileSaver fileSaver = new FileSaver(imp);
    fileSaver.saveAsTiff(filename1);
  }

  void showAbout() {
    IJ.showMessage
        ("About Sum Images by rules...",
            "Read all images from a directory and sum them based on rules.");
  }

  class AnglesSelectDialog extends Dialog implements ActionListener {

    Choice thisChoice = null;
    private GridBagLayout grid;
    private GridBagConstraints c;
    private boolean macro;
    private String macroOptions;
    protected Component theLabel;
    protected Button cancel, okay;
    protected int angleSelected;
    protected int y = 0;
    protected File[] imageFiles = null;

    public AnglesSelectDialog(String title, Frame parent) {
        super(parent, title, true);
        grid = new GridBagLayout();
        c = new GridBagConstraints();
        setLayout(grid);
        macroOptions = Macro.getOptions();
        macro = macroOptions != null;
    }

    /** Displays this dialog box. */
    public void showDialog() {

      addChoice("Angles to sum: ", items, items[3]);
      new FileDrop(this, files -> {
        // handle file drop
        imageFiles = files;
        okay.setLabel("     Sum     ");
      }); // end FileDrop.Listener

      if (macro) {
        //IJ.write("showDialog: "+macroOptions);
        dispose();
        return;
      }
      Panel buttons = new Panel();
      buttons.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 0));
      cancel = new Button("Cancel");
      cancel.addActionListener(this);
      buttons.add(cancel);
      okay = new Button("Drop files here");
      okay.addActionListener(this);
      buttons.add(okay);
      new FileDrop(this, files -> {
        // handle file drop
        imageFiles = files;
        okay.setLabel("     Sum     ");
      }); // end FileDrop.Listener
      c.gridx = 0;
      c.gridy = y;
      c.anchor = GridBagConstraints.EAST;
      c.gridwidth = 2;
      c.insets = new Insets(15, 0, 0, 0);
      grid.setConstraints(buttons, c);
      add(buttons);
      if (Constants.macosx)
        setResizable(false);
      pack();
      GUI.center(this);
      setVisible(true);
//    IJ.wait(250); // work around for Sun/WinNT bug
    }

    /** Adds a popup menu.
     * @param label	the label
     * @param items	the menu items
     * @param defaultItem	the menu item initially selected
     */
    public void addChoice(String label, String[] items, String defaultItem) {
      theLabel = new Label(label);
      c.gridx = 0;
      c.gridy = y;
      c.anchor = GridBagConstraints.EAST;
      c.gridwidth = 1;
      c.insets = new Insets(0, 0, 5, 0);
      grid.setConstraints(theLabel, c);
      add(theLabel);
      thisChoice = new Choice();
      for (int i = 0; i < items.length; i++)
        thisChoice.addItem(items[i]);
      thisChoice.select(defaultItem);
      c.gridx = 1;
      c.gridy = y;
      c.anchor = GridBagConstraints.WEST;
      grid.setConstraints(thisChoice, c);
      add(thisChoice);
      y++;
    }

    public void actionPerformed(ActionEvent e) {
      if (e.getSource() == okay) {
        angleSelected = thisChoice.getSelectedIndex();
        sumImages(imageFiles, angleSelected);
      }
      setVisible(false);
      dispose();
    }

  }

}
