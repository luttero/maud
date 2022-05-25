/*
 * @(#)SitePosition.java created 01/01/1997 Mesiano
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

package it.unitn.ing.rista.diffr;

/**
 * The SitePosition is a class
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:55 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class SitePosition extends java.lang.Object {
  //insert class definition here
  public double sign[] = new double[9];
  public double constant[] = new double[3];

  public SitePosition(double signx1, double signy1, double signz1, double constant1,
                      double signx2, double signy2, double signz2, double constant2,
                      double signx3, double signy3, double signz3, double constant3) {
    sign[0] = signx1;
    sign[1] = signy1;
    sign[2] = signz1;
    constant[0] = constant1;
    sign[3] = signx2;
    sign[4] = signy2;
    sign[5] = signz2;
    constant[1] = constant2;
    sign[6] = signx3;
    sign[7] = signy3;
    sign[8] = signz3;
    constant[2] = constant3;
  }

  public double getcoord(int index, double xf[]) {
    double x = constant[index] + sign[index * 3 + 0] * xf[0]
            + sign[index * 3 + 1] * xf[1]
            + sign[index * 3 + 2] * xf[2];
    while (x >= 1.0) {
      x -= 1.0;
    }
    while (x < 0.0) {
      x += 1.0;
    }
    return x;
  }

  public double getcoordNoCheck(int index, double xf[]) {
    int index3 = index * 3;
    double x = constant[index] + sign[index3++] * xf[0]
            + sign[index3++] * xf[1]
            + sign[index3] * xf[2];
    return x;
  }

	public double[][] getRotationMatrix() {
		double[][] Rx = new double[3][3];
		int k = 0;
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < 3; j++)
				Rx[i][j] = sign[k++];
		return Rx;
	}

	public double[] getTranslationVector() {
		double[] Tx = new double[3];
		int k = 0;
		for (int i = 0; i < 3; i++)
			Tx[i] = constant[k++];
		return Tx;
	}

	public String getx() {
    return new String(translate(sign[0], "x") + translate(sign[1], "y") + translate(sign[2], "z") +
            translateconstant(constant[0]));
  }

  public String gety() {
    return new String(translate(sign[3], "x") + translate(sign[4], "y") + translate(sign[5], "z") +
            translateconstant(constant[1]));
  }

  public String getz() {
    return new String(translate(sign[6], "x") + translate(sign[7], "y") + translate(sign[8], "z") +
            translateconstant(constant[2]));
  }

  public String getx_i() {
    return new String(translateconstant(constant[0]) +
        translate(sign[0], "x") + translate(sign[1], "y") + translate(sign[2], "z"));
  }

  public String gety_i() {
    return new String(translateconstant(constant[1]) +
        translate(sign[3], "x") + translate(sign[4], "y") + translate(sign[5], "z"));
  }

  public String getz_i() {
    return new String(translateconstant(constant[2]) +
        translate(sign[6], "x") + translate(sign[7], "y") + translate(sign[8], "z"));
  }

  public static String translate(double asign, String alabel) {
    if (asign == 0.0)
      return "";
    else if (asign == 1.0)
      return new String("+" + alabel);
    else if (asign == -1.0)
      return new String("-" + alabel);
    else if (asign > 0.0)
      return new String("+" + (new Float(asign)).toString() + " " + alabel);
    else
      return new String("-" + (new Float(asign)).toString() + " " + alabel);
  }

  public static String translateconstant(double aconstant) {
    if (aconstant == 0.0)
      return "";
    else if (aconstant > 0.0)
      return new String("+" + (new Float(aconstant)).toString());
    else
      return new String("-" + (new Float(aconstant)).toString());
  }
}
