/*
 * @(#)PseudoVoigt.java created 01/01/1997 Mesiano
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

import it.unitn.ing.rista.jpvm.*;

/**
 * The PseudoVoigt is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.6 $, $Date: 2006/07/20 13:39:07 $
 * @since JDK1.1
 */

public class PseudoVoigt implements jpvmObject {

  static double divhwhm = 1.0;
  static double dgx = 1.0;
  static double dcx = 1.0;
  static double cutValue = 80.0;
  static double maxValue = 1E90;

  public PseudoVoigt() {
  }

/*
	private Gaussian() {
	}

	public static double getY(double hwhm, double dx) {
		double value = dx / hwhm;
		value *= value;
		if (value > 30.0)
			return 0.0;
		return Constants.sqrtln2pi * Math.exp(- Constants.LN2 * value) / hwhm;
	}


	private Cauchy() {
	} */

	public static double getIntensity(double intensity, double dx, double hwhm, double eta) {
    double dgx = intensity * (1.0 - eta) * Constants.sqrtln2pi / hwhm;
    double dcx = intensity * eta / (Math.PI * hwhm);
    dx /= hwhm;
    dx *= dx;
    double f = 0.0;
    if (dx > cutValue)
      f += dcx / (1.0 + dx);
    else
      f += dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
    return f;
	}


  public static double getY(double dx, double hwhm, double dgx, double dcx) {
    dx /= hwhm;
    dx *= dx;
    if (dx > cutValue)
      return dcx / (1.0 + dx);
    return dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
  }


  public static double getY(double eta, double hwhm, double dx) {
    return eta * Cauchy.getY(hwhm, dx) + (1.0 - eta) * Gaussian.getY(hwhm, dx);
  }

  public static double[] getY(double intensity, double position, double eta, double hwhm, double[] x) {
    int last = x.length;
    double dx;
    divhwhm = 1.0 / hwhm;
    dgx = intensity * (1.0 - eta) * Constants.sqrtln2pi * divhwhm;
    dcx = intensity * eta / (Constants.PI * hwhm);

    for (int i = 0; i < last; i++) {
      dx = (position - x[i]) * divhwhm;
      dx *= dx;
      if (dx > maxValue)
        x[i] = 0.0;
      else if (dx > cutValue) {
        x[i] = dcx / (1.0 + dx);
      } else {
        x[i] = dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
      }
    }
    return x;
  }

  public static void setCharacteristics(double intensity, double eta, double hwhm) {
    divhwhm = 1.0 / hwhm;
    dgx = intensity * (1.0 - eta) * Constants.sqrtln2pi * divhwhm;
    dcx = intensity * eta / (Constants.PI * hwhm);
  }

  public static double getY(double dx) {
    dx *= divhwhm;
    dx *= dx;
    if (dx > maxValue)
      return 0.0;
    if (dx > cutValue)
      return dcx / (1.0 + dx);
    return dcx / (1.0 + dx) + dgx * Math.exp(-Constants.LN2 * dx);
  }

  public static double getY(double intensity, double position, double eta, double hwhm, double x) {
    return intensity * getY(eta, hwhm, position - x);
  }

  public static void main(String args[]) {
    (new PseudoVoigt()).execute(null);
    System.exit(0);
  }

  public void execute(String[] args) {
    try {
      jpvmEnvironment jpvm;
//      if (args == null)
        jpvm = new jpvmEnvironment();
//      else
//        jpvm = new jpvmEnvironment(args);

//      jpvmTaskId myTaskId = jpvm.pvm_mytid();

      // Get my parent's task id...
      jpvmTaskId parent = jpvm.pvm_parent();

      boolean isRunning = true;

      while (isRunning) {
//		System.out.println("waiting for messages...");
        jpvmMessage msg = jpvm.pvm_recv(jpvmDaemon.sendPVTag);
//		System.out.println("received, computing");

        int peaknumber = msg.buffer.upkint();
        if (peaknumber == -1) {
          isRunning = false;
        } else {
          double intensity = msg.buffer.upkdouble();
          double position = msg.buffer.upkdouble();
          double eta = msg.buffer.upkdouble();
          double hwhm = msg.buffer.upkdouble();
          int rootdim = msg.buffer.upkint();
          double[] xcoor = new double[rootdim];
          msg.buffer.unpack(xcoor, rootdim, 1);

          xcoor = getY(intensity, position, eta, hwhm, xcoor);

          jpvmBuffer buf = new jpvmBuffer();
          buf.pack(peaknumber);
          buf.pack(rootdim);
          buf.pack(xcoor, rootdim, 1);
//		System.out.println("sending results...");
          jpvm.pvm_send(buf, parent, jpvmDaemon.recvPVTag);
        }

      }
      jpvm.pvm_exit();

    } catch (jpvmException ie) {
      ie.printStackTrace();
    }
  }

}
