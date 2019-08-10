/*
 * @(#)PoleFigureOutput.java created 27/10/1999 Berkeley
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.diffr.rta;

import java.lang.*;
import java.io.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

/**
 *  The PoleFigureOutput is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:57 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PoleFigureOutput {

  String outputFormat = null;
  Phase phase = null;
  String filename = null;
  BufferedWriter PFwriter = null;
  String title1 = null;
  String title2 = null;
  double resolution = 5.0;
  int alphamax = 73;
  int old1387max = 1387;

  public PoleFigureOutput(String outputFormat, String filename, Phase aphase) {
    this.outputFormat = outputFormat;
    phase = aphase;
    this.filename = filename;
  }

  public PoleFigureOutput(String filename, Phase aphase) {
    phase = aphase;
    this.filename = filename;
  }

  public void openOutput() {

    PFwriter = Misc.getWriter(filename);

    title1 = new String(phase.toXRDcatString() + ": experimental pole figure, ");
    title2 = new String(phase.toXRDcatString() + ": recalculated pole figure, ");

  }

  public void write(int h, int k, int l, double[] polf, boolean experimental) {

    StringBuffer tmp = null;

    if (experimental)
      tmp = new StringBuffer(title1);
    else
      tmp = new StringBuffer(title2);

    tmp = tmp.append(" ").append(Integer.toString(h)).
            append(",").append(Integer.toString(k)).
            append(",").append(Integer.toString(l));
    int bufflength = tmp.length();
    for (int i = 0; i < 79 - bufflength; i++)
      tmp = tmp.append(" ");

    String commentLine = new String(tmp.toString().substring(0, 79) + "#");

    try {
      PFwriter.write(commentLine);
      for (int i = 0; i < 5; i++)
        PFwriter.write(Constants.lineSeparator);

//			new String("    1.0000    1.0000    1.0000   90.0000   90.0000   90.0000    7    1");
      PFwriter.write(Misc.getFirstPFline(phase));
      PFwriter.write(Constants.lineSeparator);

      String firstline = new String(" " + Misc.getIntStringFormatted(h, 3) +
              Misc.getIntStringFormatted(k, 3) +
              Misc.getIntStringFormatted(l, 3) +
              "   .0 90.0" + Misc.getDoubleStringFormatted(resolution, 3, 1) +
              "   .0360.0" + Misc.getDoubleStringFormatted(resolution, 3, 1) +
              " 1 1");
      PFwriter.write(firstline);
      PFwriter.write(Constants.lineSeparator);
    } catch (IOException io) {
    }

    int until18 = 0;
    int skip73 = 0;

    for (int ny = 0; ny < old1387max; ++ny) {
      if (++skip73 != alphamax) {
        if (polf[ny] < 0.0)
          polf[ny] = 0.0;
        int imh = (int) (polf[ny] * 100.000001);

        try {
          if (until18 == 0)
            PFwriter.write(" ");
          PFwriter.write(Misc.getIntStringFormatted(imh, 4));
          if (++until18 >= 18) {
            until18 = 0;
            PFwriter.write(Constants.lineSeparator);
          }
        } catch (IOException io) {
        }
      } else
        skip73 = 0;
    }
    if (until18 != 0)
      try {
        PFwriter.write(Constants.lineSeparator);
      } catch (IOException io) {
      }
    try {
      PFwriter.write(Constants.lineSeparator);
    } catch (IOException io) {
    }

  }

  public void closeOutput() {
    try {
      PFwriter.write(Constants.lineSeparator);
      PFwriter.flush();
      PFwriter.close();
    } catch (IOException io) {
    }
  }

  public void computeAndWrite(Reflection reflex, Texture textureModel) {

    double[] polf;
    double[][] alphabeta = new double[2][old1387max];
    int old19 = 19;

    int ij = 0;
    for (int i = 0; i < old19; i++) {
      for (int j = 0; j < alphamax; j++) {

        alphabeta[0][ij] = resolution * Constants.DEGTOPI * i;
        alphabeta[1][ij++] = resolution * Constants.DEGTOPI * j;
      }
    }
    polf = textureModel.computeTextureFactor(phase, alphabeta, reflex);
    write(reflex.getH(), reflex.getK(), reflex.getL(), polf, false);
  }

  public void computeAndWrite() {
    openOutput();
    Texture textureModel = phase.getActiveTexture();
    int hklnumber = phase.gethklNumber();
    int maxPFs = MaudPreferences.getInteger("textureOutput.maxPFsBeartexFormat", 100);
    if (hklnumber > maxPFs) hklnumber = maxPFs;
    ProgressFrame prF = null;
    if (!Constants.textonly && Constants.showProgressFrame)
      try {
          prF = new ProgressFrame(hklnumber);
      } catch (NullPointerException npe) {
        System.out.println("Not able to create frame, MacOSX display sleep bug?");
      }
    printf("Saving pole figures...            ", prF);
    for (int i = 0; i < hklnumber; i++) {
      Reflection reflex = phase.getReflex(i);
      computeAndWrite(reflex, textureModel);
      if (prF != null)
        prF.increaseProgressBarValue();
      printf("Saving pole figure: " + Integer.toString(reflex.getH()) + " " + Integer.toString(reflex.getK()) +
              " " + Integer.toString(reflex.getL()), prF);
    }
    if (prF != null) {
      prF.setVisible(false);
      prF.dispose();
    }
    closeOutput();
  }

	public void computeAndWrite(Vector<Reflection> reflList) {
		openOutput();
		Texture textureModel = phase.getActiveTexture();
		int hklnumber = reflList.size();
		for (int i = 0; i < hklnumber; i++)
			computeAndWrite(reflList.elementAt(i), textureModel);
		closeOutput();
	}
	
  public void printf(String message, ProgressFrame prF) {
    if (prF != null)
      prF.setProgressText(message);
    else
      System.out.println(message);
  }

}
