/*
 * @(#)PoleFigure.java created 3/10/1998 TUHH, Harburg-Hamburg
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

package it.unitn.ing.rista.diffr;

import java.util.*;
import java.lang.*;

import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.rta.*;

/**
 *  The PoleFigure is a class
 *
 *
 * @version $Revision: 1.5 $, $Date: 2006/01/19 14:45:54 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PoleFigure extends XRDcat {
  protected static String[] diclistc = {};
  protected static String[] diclistcrm = {};

  protected static String[] classlistc = {};

  int numberOfData = 0;

  public static int nfismax = (int) (360.0 / Constants.integrationStepPF + 1.00001);
  public static double phonstepeps = .005;
  public static double pi2deg = Constants.PI / 180.0;

  public PoleFigure(XRDcat obj, String alabel) {
    super(obj, alabel);
    initXRD();
  }

  public PoleFigure(XRDcat afile) {
    this(afile, "Pole figure x");
  }

  public void initConstant() {
    Nstring = 0;
    Nstringloop = 0;
    Nparameter = 0;
    Nparameterloop = 0;
    Nsubordinate = 0;
    Nsubordinateloop = 0;
  }

  public void initDictionary() {
    for (int i = 0; i < totsubordinateloop; i++)
      diclist[i] = diclistc[i];
    for (int i = 0; i < totsubordinateloop; i++)
      diclistRealMeaning[i] = diclistcrm[i];
    for (int i = 0; i < totsubordinateloop - totsubordinate; i++)
      classlist[i] = classlistc[i];
  }

  public void initParameters() {
    super.initParameters();
  }

  public int getTotalNumberOfData() {
    return numberOfData;
  }

  public double getYData(int j, int i) {
    return 1.0;
  }

  public static final double calculatePF(double[][][] f, double phoninp, double theta, double phi,
                                         double sthi, double cthi,
                                         double fhir, int inv, double resolution) {
/* Local variables */
    double ffak, pfak, gams, bets;
    int nfis, nfiy, ntfs;
    int nals1, i;
    double s;
    int nbgam;
    double pinpf;
    int ntety, ivorz, nb;
    double sn;
    int ny;
    double ca2, cb2, sa2, g2r, gam;
    int nga, nal;
    double als;
    int iswitch;

//    int alphamax = (int) (360.0 / resolution + 1.00001);
//    int betamax = alphamax / 2 + 1;
//    int old2701max = alphamax * betamax;

    double pi5g = resolution * Constants.DEGTOPI;
    double pi25g = pi5g / 2.;
//    double pi75g = pi25g * 3.;

/*     Calculation of a complete reduced pole figure */
/*     Normalization */
/*     INPUT FIO given in the whole G-space OUTPUT POLREF=FS */

    double phonstep = phoninp + phonstepeps;

    double fs = 0.;

/* Projection thread loop, Simpson integration */

    cb2 = cthi;
    g2r = Constants.PI - fhir;
    boolean checkL13 = false;
    boolean nextCheck = false;
    double cr = Math.cos(theta * Constants.DEGTOPI);
    double sr = Math.sin(theta * Constants.DEGTOPI);
    do {
      while (g2r < 0.) {
        g2r += Constants.PI2;
      }
      for (nfis = 0; nfis < nfismax; nfis++) {
        double ang = nfis * Constants.integrationStepPFR;
        ca2 = -Math.cos(ang);
        sa2 = Math.sin(ang);
//				for (ntety = 1; ntety <= 19; ++ntety) {
        double[] angles = Uwimvuo.g20g100_(ca2, sa2, cb2, sthi, cr, sr);
        angles[0] += phi * Constants.DEGTOPI;
        angles[2] += g2r;
        while (angles[1] >= Constants.PI2)
          angles[1] -= Constants.PI2;
        while (angles[1] < 0)
          angles[1] += Constants.PI2;
        if (angles[1] >= Constants.PI) {
          angles[1] = Constants.PI2 - angles[1];
          angles[0] += Constants.PI;
          angles[2] += Constants.PI;
        }
        while (angles[0] >= Constants.PI2)
          angles[0] -= Constants.PI2;
        while (angles[0] < 0)
          angles[0] += Constants.PI2;
        while (angles[2] >= Constants.PI2) {
          angles[2] -= Constants.PI2;
        }
        while (angles[2] < 0) {
          angles[2] += Constants.PI2;
        }
        nal = (int) ((angles[0] + pi25g) / pi5g + .000001);
        nb = (int) ((angles[1] + pi25g) / pi5g + .000001);
        nga = (int) ((angles[2] + pi25g) / pi5g + .000001);
        ffak = f[nal][nb][nga];
        if (ffak > phonstep) {
          ffak = (ffak - phoninp) * Constants.pisimg;
//				ffak *= pisimg;
          if (!(nfis == 0 || nfis == nfismax - 1)) {
            if (MoreMath.powint(-1, nfis + 1) < 0)
              ffak *= 2.;
            else
              ffak *= 4.;
          }
          fs += ffak;
        }
      }
      fs += phoninp;
      if (inv == 1 || nextCheck) {
        checkL13 = true;
      } else {
        nextCheck = true;
        cb2 = -cb2;
        g2r -= Constants.PI;
      }
    } while (!checkL13); /*goto L13; */

    if (inv != 1) {
      fs /= 2.;
    }
/*                                          Normalization to PINPF */
//		System.out.println(fs);
    return fs;
  } /* calpolo_ */

}
