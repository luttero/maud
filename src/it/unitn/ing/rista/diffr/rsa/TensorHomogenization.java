/*
 * @(#)TensorHomogenization.java created Apr 19, 2006 Mesiano
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

package it.unitn.ing.rista.diffr.rsa;

import it.unitn.ing.rista.diffr.rta.Uwimvuo;
import it.unitn.ing.rista.diffr.Phase;
import it.unitn.ing.rista.awt.Utility;
import it.unitn.ing.rista.comp.MarqardLeastSquares;
import it.unitn.ing.rista.interfaces.SimpleFunction;
import it.unitn.ing.rista.util.MoreMath;
import it.unitn.ing.rista.util.Misc;

import Jama.Matrix;
import Jama.EigenvalueDecomposition;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.BufferedReader;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * The TensorHomogenization is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2006/07/20 14:06:04 $
 * @since JDK1.1
 */

public class TensorHomogenization implements SimpleFunction {

  public static boolean debug = true;
  int rank = 0;
  double[][] tensor = null;
  double[][][] result = null;
  double[][][] odf = null;
  public static int ARITHMETIC = 0;
  public static int GEOMETRIC = 1;
  public static int VOIGT_STIFFNESS = 0;
  public static int REUSS_COMPLIANCE = 1;
  public static int REUSS_STIFFNESS = 2;
  public static int VOIGT_COMPLIANCE = 3;
  public static int HILL_STIFFNESS = 4;
  public static int HILL_COMPLIANCE = 5;
  public static int GEO_STIFFNESS = 6;
  public static int GEO_COMPLIANCE = 7;
  public static String[] tensor2Label = {"Arithmetic", "Geometric"};
  public static String[] tensor4Label = {"Voigt (stiffness)", "Reuss (compliance)",
      "Reuss (stiffness)", "Voigt (compliance)",
      "Hill (stiffness)", "Hill (compliance)",
      "Geo (stiffness)", "Geo (compliance)"};
  public static int[] tensor4IndexRead = {0, 2, 4, 6};
  private Phase phase = null;

  public static double[][] defaultTensor = {{168.4, 121.4, 121.4, 0, 0, 0},
      {121.4, 168.4, 121.4, 0, 0, 0},
      {121.4, 121.4, 168.4, 0, 0, 0},
      {0, 0, 0, 75.5, 0, 0},
      {0, 0, 0, 0, 75.5, 0},
      {0, 0, 0, 0, 0, 75.5}};

  public static double[][] defaultTensor2 = {{1.0E-5, 0, 0}, {0, 1.0E-5, 0}, {0, 0, 1.0E-5}};

  public TensorHomogenization(double[][] tensor, double[][][] odf) {
    setTensor(tensor);
    if (odf != null)
      setODF(odf);
  }

	public TensorHomogenization(Phase aphase) {

    double[][][] fio = new double[i73][i37][i73];
    for (int nga = 0; nga < i73; nga++)
      for (int nb = 0; nb < i37; nb++)
        for (int na = 0; na < i73; na++)
          fio[na][nb][nga] = aphase.getActiveTexture().getODF(na * pi5, nb * pi5, nga * pi5);
    phase = aphase;
//    setTensor(aphase.getTensor());
    setODF(fio);
  }


  public TensorHomogenization(double[][] tensor) {
    setTensor(tensor);
  }

  public TensorHomogenization(double[][][] odf) {
    setODF(odf);
  }

  public TensorHomogenization() {
  }

  public void setTensor(double[][] tensor) {
    this.tensor = tensor;
    if (tensor.length == 3) {
      // first rank
      rank = 1;
    } else if (tensor.length == 6) {
      // second rank
      rank = 2;
    }
//    if (phase != null)
//      phase.setTensor(tensor);
  }

  public void setODF(double[][][] odf) {
    this.odf = odf;
  }

  public void homogenize() {
    if (odf == null)
      odf = generateRandomODF();
    switch (rank) {
      case 1:
        result = tensor2(tensor, odf);
        break;
      case 2:
        result = tensor4(tensor, odf);
        break;
      default: {
      }
    }
  }

  public void homogenizeGEO() {
    if (odf == null)
      odf = generateRandomODF();
    switch (rank) {
      case 1:
        result = tensor2(tensor, odf);
        break;
      case 2:
        result = tensorGEO4(tensor, odf);
        break;
      default: {
      }
    }
  }

  public void homogenizeModel(int model) {
    if (odf == null)
      odf = generateRandomODF();
    switch (rank) {
      case 1:
        result = tensor2(tensor, odf, model);
        break;
      case 2:
        result = tensor4(tensor, odf, model);
        break;
      default: {
      }
    }
  }

  public double[][] getHomogenizedTensor(int index) {
    if (result != null)
      return result[index];
    return null;
  }

  public String getHomogenizedLabel(int index) {
    switch (rank) {
      case 1:
        return tensor2Label[index];
      case 2:
        return tensor4Label[index];
      default: {
      }
    }
    return null;
  }

  public String getGEOResults() {
    StringBuffer text = new StringBuffer("");
    text.append("Homogenization of tensor:\n");
    for (int i = 0; i < tensor.length; i++) {
      for (int j = 0; j < tensor[i].length; j++) {
        text.append(Float.toString((float) tensor[i][j]));
        text.append(" ");
      }
      text.append("\n");
    }
    text.append("\n");
    text.append(getHomogenizedLabel(GEO_STIFFNESS));
    text.append("\n");
    double[][] ten = getHomogenizedTensor(0);
    for (int i = 0; i < ten.length; i++) {
      for (int j = 0; j < ten[i].length; j++) {
        text.append(Float.toString((float) ten[i][j]));
        text.append(" ");
      }
      text.append("\n");
    }
    text.append("\n");
    return text.toString();
  }

  public String getModelResults(int model) {
    StringBuffer text = new StringBuffer("");
    text.append("Homogenization of tensor:\n");
    for (int i = 0; i < tensor.length; i++) {
      for (int j = 0; j < tensor[i].length; j++) {
        text.append(Float.toString((float) tensor[i][j]));
        text.append(" ");
      }
      text.append("\n");
    }
    text.append("\n");
    text.append(getHomogenizedLabel(model));
    text.append("\n");
    double[][] ten = getHomogenizedTensor(0);
    for (int i = 0; i < ten.length; i++) {
      for (int j = 0; j < ten[i].length; j++) {
        text.append(Float.toString((float) ten[i][j]));
        text.append(" ");
      }
      text.append("\n");
    }
    text.append("\n");
    return text.toString();
  }

  public String getResults() {
    StringBuffer text = new StringBuffer("");
    text.append("Homogenization of tensor:\n");
    for (int i = 0; i < tensor.length; i++) {
      for (int j = 0; j < tensor[i].length; j++) {
        text.append(Float.toString((float) tensor[i][j]));
        text.append(" ");
      }
      text.append("\n");
    }
    text.append("\n");
	  int ranklength = 2;
	  switch (rank) {
		  case 1:
			  ranklength = tensor2Label.length;
			  break;
		  case 2:
			  ranklength = tensor4Label.length;
			  break;
		  default: {
		  }
	  }
    for (int k = 0; k < ranklength; k++) {
      text.append(getHomogenizedLabel(k));
      text.append("\n");
      double[][] ten = getHomogenizedTensor(k);
      for (int i = 0; i < ten.length; i++) {
        for (int j = 0; j < ten[i].length; j++) {
          text.append(Float.toString((float) ten[i][j]));
          text.append(" ");
        }
        text.append("\n");
      }
      text.append("\n");
    }
    return text.toString();
  }

  static int itend = 5 /* superhill number of iterations */;
  static final int i37 = 37;
  static final int i73 = 73;

  static final double pi5 = Math.PI / 36.0;
  static final double pi25 = Math.PI / 72.0;
  static final double picon = 8.0 * Math.PI * Math.PI;
  static final double machep = Math.pow(2.0, -52.0);
  static final double w2 = Math.sqrt(2.0);

  public static double[][][] generateRandomODF() {
    double[][][] fio = new double[i73][i37][i73];
    for (int nga = 0; nga < i73; nga++)
      for (int nb = 0; nb < i37; nb++)
        for (int na = 0; na < i73; na++)
          fio[na][nb][nga] = 1.0f;
    return fio;
  }

  /**
   * Homogenize a first rank tensor using a rondom ODF
   *
   * @param ten2 the tensor 3x3 to be homogenized
   * @return a tensor x 2 containing the arithmetic and geometric mean, first index = 0:
   *                      arithmetic, = 1: geometric
   */
  public static double[][][] tensor2(double[][] ten2) {
    return tensor2(ten2, generateRandomODF());
  }

  /**
   * Homogenize a second rank tensor using a rondom ODF
   *
   * @param e0 the tensor 6x6 to be homogenized
   * @return a tensor x 12 containing all the omogenizations
   */
  public static double[][][] tensor4(double[][] e0) {
    return tensor4(e0, generateRandomODF());
  }

  static double[][][] ftt = new double[i73][i37][i73];

  public static double[][][] tensor2(double[][] ten2, double[][][] fio) {
/*
C       PROGRAM GEO2.FOR                              S.M.,Dresden,Nov.94
C
C    Origin -> GEOMT2 , february 1993, Universite de Metz, LM2P
C
C    The program determines the arithmetic and geometric mean of a symmetric
C    tensor of second rank for a given orientation distribution.
C
C    Used input version of the ODF:  WIMV Standard FORMAT
C
C    INPUT    of sample and crystal syymmetry and of the 6 tensor data by
C             the control file on unit 5 or by hand.
C
C    OUTPUT : EGEOM(3,3)      File GEO2.LST
*/

    int[] mi = {0, 0, 0, 1, 1, 2, 1, 2, 2};
    int[] mj = {0, 1, 2, 1, 2, 2, 0, 0, 1};

    double[] cosw = new double[i73], sinw = new double[i73];
    double ca, cb, sa, sb, cga, arg, sga, sum, fcon, suma;
    double[] eigw0 = new double[3];
    double[] eiglog = new double[3];
    double[] eigwl = new double[3];
    double[][] e02 = new double[3][3];
    double[][] hloge0 = new double[3][3];
    double[][] hlogea = new double[3][3];
    double[][] egeom = new double[3][3];
    double[][] p0 = new double[3][3];
    double[][] ps = new double[3][3];
    double[][] g = new double[3][3];
    double[][] w = new double[3][3];
    double[][] ea = new double[3][3];
    double[][] wwarim = new double[9][9];
    double[][][] tensorHomo = new double[2][3][3];

    fttcon1(fio, ftt);

    for (int i = 0; i < i73; i++) {
      arg = pi5 * i;
      cosw[i] = Math.cos(arg);
      sinw[i] = Math.sin(arg);
//      ifiw[i] = 5 * i;
    }
    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
        e02[i][j] = ten2[i][j];
//s        e02[j][i] = e02[i][j];
      }
    }
    if (eisrs1(e02, eigw0, p0) == 1) // problem
	    return tensorHomo;
    for (int i = 0; i < 3; i++) {
      eiglog[i] = Math.log(eigw0[i]);
    }
    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
        sum = 0.0;
        for (int k = 0; k < 3; k++) {
          sum += p0[i][k] * p0[j][k] * eiglog[k];
        }
        hloge0[i][j] = sum;
//s        hloge0[j][i] = sum;
      }
    }
    for (int kw = 0; kw < 9; kw++) {
      for (int kws = 0; kws < 9; kws++) {
        wwarim[kw][kws] = 0.0;
      }
    }
    for (int nga = 0; nga < i73; nga++) {
      sga = sinw[nga];
      cga = cosw[nga];
      for (int nb = 0; nb < i37; nb++) {
        sb = sinw[nb];
        cb = cosw[nb];
        for (int na = 0; na < i73; na++) {
          fcon = ftt[na][nb][nga];
          if (fcon > 0.0) {
            sa = sinw[na];
            ca = cosw[na];
            g[0][0] = ca * cb * cga - sa * sga;
            g[0][1] = sa * cb * cga + ca * sga;
            g[0][2] = -sb * cga;
            g[1][0] = -ca * cb * sga - sa * cga;
            g[1][1] = -sa * cb * sga + ca * cga;
            g[1][2] = sb * sga;
            g[2][0] = ca * sb;
            g[2][1] = sa * sb;
            g[2][2] = cb;
            for (int kw = 0; kw < 9; kw++) {
              int i = mi[kw];
              int j = mj[kw];
              for (int kws = 0; kws < 9; kws++) {
                int is = mi[kws];
                int js = mj[kws];
                wwarim[kw][kws] += g[is][i] * g[js][j] * fcon;
              }
            }
          }
        }
      }
    }
    for (int kw = 0; kw < 9; kw++) {
      int i = mi[kw];
      int j = mj[kw];
      sum = 0.0;
      suma = 0.0;
      for (int kws = 0; kws < 9; kws++) {
        int is = mi[kws];
        int js = mj[kws];
        sum += wwarim[kw][kws] * hloge0[is][js];
        suma += wwarim[kw][kws] * e02[is][js];
      }
      hlogea[i][j] = sum;
//s      hlogea[j][i] = sum;
      ea[i][j] = suma;
//s      ea[j][i] = suma;
      tensorHomo[0][i][j] /*s= tensorHomo[0][j][i]*/ = suma;
    }
    if (eisrs1(hlogea, eigwl, ps) == 1) // problem
	    return tensorHomo;
//    for (int i = 0; i < 3; i++) {
//    }
    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
        sum = 0.0;
        for (int k = 0; k < 3; k++)
          sum += ps[i][k] * ps[j][k] * Math.exp(eigwl[k]);
        egeom[i][j] = sum;
//s        egeom[j][i] = sum;
        tensorHomo[1][i][j] /*s= tensorHomo[1][j][i]*/ = sum;
      }
    }
    return tensorHomo;
  }

  public static double[][][] tensorGEO2(double[][] ten2, double[][][] fio) {
    return tensor2(ten2, fio, GEOMETRIC);
  }


  public static double[][][] tensor2(double[][] ten2, double[][][] fio, int model) {
/*
C       PROGRAM GEO2.FOR                              S.M.,Dresden,Nov.94
C
C    Origin -> GEOMT2 , february 1993, Universite de Metz, LM2P
C
C    The program determines the arithmetic and geometric mean of a symmetric
C    tensor of second rank for a given orientation distribution.
C
C    Used input version of the ODF:  WIMV Standard FORMAT
C
C    INPUT    of sample and crystal syymmetry and of the 6 tensor data by
C             the control file on unit 5 or by hand.
C
C    OUTPUT : EGEOM(3,3)      File GEO2.LST
*/

    int[] mi = {0, 0, 0, 1, 1, 2, 1, 2, 2};
    int[] mj = {0, 1, 2, 1, 2, 2, 0, 0, 1};

    double[] cosw = new double[i73], sinw = new double[i73];
    double ca, cb, sa, sb, cga, arg, sga, sum, fcon, suma;
    double[] eigw0 = new double[3];
    double[] eiglog = new double[3];
    double[] eigwl = new double[3];
    double[][] e02 = new double[3][3];
    double[][] hloge0 = new double[3][3];
    double[][] hlogea = new double[3][3];
    double[][] egeom = new double[3][3];
    double[][] p0 = new double[3][3];
    double[][] ps = new double[3][3];
    double[][] g = new double[3][3];
    double[][] w = new double[3][3];
    double[][] ea = new double[3][3];
    double[][] wwarim = new double[9][9];
    double[][][] tensorHomo = new double[1][3][3];

    fttcon1(fio, ftt);

    for (int i = 0; i < i73; i++) {
      arg = pi5 * i;
      cosw[i] = Math.cos(arg);
      sinw[i] = Math.sin(arg);
//      ifiw[i] = 5 * i;
    }
    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
        e02[i][j] = ten2[i][j];
//s        e02[j][i] = e02[i][j];
      }
    }
    if (eisrs1(e02, eigw0, p0) == 1) // problem
	    return tensorHomo;
    for (int i = 0; i < 3; i++) {
      eiglog[i] = Math.log(eigw0[i]);
    }
    for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
        sum = 0.0;
        for (int k = 0; k < 3; k++) {
          sum += p0[i][k] * p0[j][k] * eiglog[k];
        }
        hloge0[i][j] = sum;
//s        hloge0[j][i] = sum;
      }
    }
    for (int kw = 0; kw < 9; kw++) {
      for (int kws = 0; kws < 9; kws++) {
        wwarim[kw][kws] = 0.0;
      }
    }
    for (int nga = 0; nga < i73; nga++) {
      sga = sinw[nga];
      cga = cosw[nga];
      for (int nb = 0; nb < i37; nb++) {
        sb = sinw[nb];
        cb = cosw[nb];
        for (int na = 0; na < i73; na++) {
          fcon = ftt[na][nb][nga];
          if (fcon > 0.0) {
            sa = sinw[na];
            ca = cosw[na];
            g[0][0] = ca * cb * cga - sa * sga;
            g[0][1] = sa * cb * cga + ca * sga;
            g[0][2] = -sb * cga;
            g[1][0] = -ca * cb * sga - sa * cga;
            g[1][1] = -sa * cb * sga + ca * cga;
            g[1][2] = sb * sga;
            g[2][0] = ca * sb;
            g[2][1] = sa * sb;
            g[2][2] = cb;
            for (int i = 0; i < 3; i++)
              for (int is = 0; is < 3; is++)
                w[i][is] = g[is][i];
            for (int kw = 0; kw < 9; kw++) {
              int i = mi[kw];
              int j = mj[kw];
              for (int kws = 0; kws < 9; kws++) {
                int is = mi[kws];
                int js = mj[kws];
                wwarim[kw][kws] += w[i][is] * w[j][js] * fcon;
              }
            }
          }
        }
      }
    }
      for (int kw = 0; kw < 9; kw++) {
        int i = mi[kw];
        int j = mj[kw];
        sum = 0.0;
        suma = 0.0;
        for (int kws = 0; kws < 9; kws++) {
          int is = mi[kws];
          int js = mj[kws];
          sum += wwarim[kw][kws] * hloge0[is][js];
          suma += wwarim[kw][kws] * e02[is][js];
        }
        hlogea[i][j] = sum;
//s        hlogea[j][i] = sum;
        ea[i][j] = suma;
//s        ea[j][i] = suma;
        tensorHomo[0][i][j] /*= tensorHomo[0][j][i]*/ = suma;
      }
    if (model == GEOMETRIC) {
      if (eisrs1(hlogea, eigwl, ps) == 1) // problem
	      return tensorHomo;
//    for (int i = 0; i < 3; i++) {
//    }
      for (int i = 0; i < 3; i++) {
        for (int j = i; j < 3; j++) {
          sum = 0.0;
          for (int k = 0; k < 3; k++)
            sum += ps[i][k] * ps[j][k] * Math.exp(eigwl[k]);
          egeom[i][j] = sum;
          egeom[j][i] = sum;
          tensorHomo[0][i][j] = tensorHomo[0][j][i] = sum;
        }
      }
    }
    return tensorHomo;
  }

  public static double[][][] tensor4(double[][] e0, double[][][] fio) {
/*
c
c!!! This version for BEARTEX is simplified. It always assumes that input
c!!! are stiffnesses. It calculates first stiffnesses, then compliances
c
C       PROGRAM GEO4ELA.FOR                                S.M. Nov.94
C
C    Origin -> Geot4efp.for S.Matthies, march 1993, Universite de Metz, LM2P
C
C    The program determines the geometric mean and other "means" basing on
C    the arithmetic mean algorithm :
C
C    arithmetic mean                           "Voigt"
C    arithmetic mean of the inverse tensor     "REUSS"
C    HILL approximation                        "HILL "
C    SUPERHILL mean                            "SUPERHILL"
C    Geometric mean                            "GEOM"
C
C    for the elasticity tensors (i.e. twice symmetric tensors of rank 4) for
C    a given orientation distribution.
C
C    The whole calculating cycle is repeated for the inverse INPUT data E0.
C                                                            *****
C    Used INPUT version of the ODF (IRANDOM=O) :  WIMV Standard FORMAT
C         *****
C    (i.e. 5 degree grid and special choice of the elementary region)
C    (ALPHA,BETA,GAMMA)-version of the EULERian angles
C    GAMMA SECTIONS ---see SUBROUTINE ODFINP---
C
C    For IRANDOM=1 random ODF f(g)=1 is assumed -  ODF-INPUT not necessary
C
C    For IRED=1 a reduced ODF of type f~(g) not necessary >0 can be used
C
C    INPUT of sample and crystal symmetry by IGA,IGB (for IRANDOM=0 only)
C    *****
C
C    Code numbers IGA IGB :
C
C    Sample  symmetry              D2 C2 C1
C    Crystal symmetry  O  T  D4 C4 D2 C2 C1 D6 C6 D3 C3
C    Code number       7  6   5  4  3  2  1 11 10  9  8
C
C    Input of the true values (! not renormalized by Voigt or Wooster schemes)
C    *****
C    E0(i1i2,j1j2) (i,j=1,2,3) of the elastic tensor components (stiffness or
C    compliance) using the Voigt Index Code :
C
C    I   1  2  3  4  5  6  enlarged code  7  8  9
C
C    i1  1  2  3  2  3  1                 3  1  2
C
C    i2  1  2  3  3  1  2                 2  3  1
C
C       If E (E0) corresponds to the stiffness C so EINV means compliance S
C       and reverse
C
C    REAL INPUT : Only the 21 numbers (upper triangle)
C         *****
C
C                 E0(I,J)  I=1,6 ; J=I,6
C
C       Below an example for an E0-DATA FILE is given. True, not modified
C     values !!!  22 strokes (Textstroke FORMAT A70)  :
C
C       TEXT stroke, e.g. Copper,stiffness
c       168.4           E0(1,1)
c       121.4           E0(1,2)
c       121.4           E0(1,3)
c       0.              E0(1,4)
c       0.              E0(1,5)
c       0.              E0(1,6)
c       168.4           E0(2,2)
c       121.4           E0(2,3)
c       0.              E0(2,4)
c       0.              E0(2,5)
c       0.              E0(2,6)
c       168.4           E0(3,3)
c       0.              E0(3,4)
c       0.              E0(3,5)
c       0.              E0(3,6)
c       75.5            E0(4,4)
c       0.              E0(4,5)
c       0.              E0(4,6)
c       75.5            E0(5,5)
c       0.              E0(5,6)
c       75.5            E0(6,6)
C
C    OUTPUT : FILE GEOT4ELA.LST.
C
C    It contains the means in form of (6X6 T-unreduced matrices), e.g.
C    EGEOM(6,6) a.s.o. Using these data the transformation to a (9x9)
C    schema is trivial.
C    (I,J=7,8,9 correspond to 4,5,6). The (6*6) data can also be used
C    to create Voigt or WOOSTER modified elastic matrix elements.
*/
    int[] mi = {0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 1, 2, 3,
        4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5};
    int[] mj = {0, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5, 0, 0, 0,
        0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4};
    int is, js;
    double cga, fak, sga, fcon;
    double[] cosw = new double[i73], sinw = new double[i73];
    double[][] e0s = new double[6][6];
    double[][] esa = new double[6][6];
    double[][] ea = new double[6][6];
    double[][] e0inv = new double[6][6];
    double[][] e0invs = new double[6][6];
    double[][] einvsa = new double[6][6];
    double[][] einvsain = new double[6][6];
    double[][] einva = new double[6][6];
    double[][] einvainv = new double[6][6];
    double[][] hloge0 = new double[6][6];
    double[][] hlogea = new double[6][6];
    double[][] ehill = new double[6][6];
    double[][] egeom = new double[6][6];
    double[][] egeoms = new double[6][6];
    double[] eigw0 = new double[6];
    double[] eigwl = new double[6];
    double[] eiglog = new double[6];
    double[] eigwin = new double[6];
    double[][] p0 = new double[6][6];
    double[][] ps = new double[6][6];
    double[][] wwarim = new double[36][36];
    double[][] w = new double[6][6];
    double[][][] tensorHomo = new double[12][6][6];

    fttcon1(fio, ftt);

    for (int i = 0; i < i73; i++) {
      double arg = pi5 * i;
      cosw[i] = Math.cos(arg);
      sinw[i] = Math.sin(arg);
//      ifiw[i] = i * 5;
    }
    int icyc = -1;
    do {
      icyc++;
      if (icyc == 1) {
        print();
        print("************************************");
        print(" Repetition cycle with E0INV for E0");
        print("************************************");
      }
      for (int igreek = 0; igreek < 6; igreek++) {
        for (int jgreek = igreek; jgreek < 6; jgreek++) {
          fak = 1.0;
          if (igreek > 2) fak *= w2;
          if (jgreek > 2) fak *= w2;
          e0s[igreek][jgreek] = e0[igreek][jgreek] * fak;
          e0s[jgreek][igreek] = e0s[igreek][jgreek];
          e0[jgreek][igreek] = e0[igreek][jgreek];
        }
      }
      print("    E0-matrix :", e0);
      print("    E0S-matrix :", e0s);
      if (eisrs1(e0s, eigw0, p0) == 1) // problem
	      return tensorHomo;
      print(" EIGENVALUES of E0S and P0-matrix  (P0-1)*E0S*P0 =", eigw0);
      print("  MATRIX P0", p0);
      for (int i = 0; i < 6; i++)
        eiglog[i] = Math.log(eigw0[i]);
      for (int i = 0; i < 6; i++)
        eigwin[i] = 1.0 / eigw0[i];
      for (int i = 0; i < 6; i++) {
        for (int j = i; j < 6; j++) {
          e0invs[i][j] = 0.0;
          for (int k = 0; k < 6; k++)
            e0invs[i][j] += p0[i][k] * p0[j][k] * eigwin[k];
          e0invs[j][i] = e0invs[i][j];
        }
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          fak = 1.0;
          if (i > 2) fak = w2;
          if (j > 2) fak *= w2;
          e0inv[i][j] = e0invs[i][j] / fak;
        }
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          hloge0[i][j] = 0.0;
          for (int k = 0; k < 6; k++)
            hloge0[i][j] += p0[i][k] * p0[j][k] * eiglog[k];
          hloge0[j][i] = hloge0[i][j];
        }
      }
      if (icyc != 1) {
        for (int kw = 0; kw < 21; kw++) {
          for (int kws = 0; kws < 36; kws++) {
            wwarim[kw][kws] = 0.0;
          }
        }
//        if (irandom != 0)
//          phon = 1.0001;
        double[][] wwarir = warir(0.0);
//        if (phon < 1.0) {
          for (int nga = 0; nga < i73; nga++) {
            sga = sinw[nga];
            cga = cosw[nga];
            for (int nb = 0; nb < i37; nb++) {
              double sb = sinw[nb];
              double g9 = cosw[nb];   // cb
              for (int na = 0; na < i73; na++) {
                fcon = ftt[na][nb][nga];
                if (fcon != 0.0) {

                  double sa = sinw[na];
                  double ca = cosw[na];
                  double g1 = ca * g9 * cga - sa * sga;
                  double g2 = sa * g9 * cga + ca * sga;
                  double g3 = -sb * cga;
                  double g4 = -ca * g9 * sga - sa * cga;
                  double g5 = -sa * g9 * sga + ca * cga;
                  double g6 = sb * sga;
                  double g7 = ca * sb;
                  double g8 = sa * sb;
//                  double g9 = cb;

                  w[0][0] = g1 * g1;
                  w[0][1] = g4 * g4;
                  w[0][2] = g7 * g7;
                  w[0][3] = w2 * g4 * g7;
                  w[0][4] = w2 * g1 * g7;
                  w[0][5] = w2 * g1 * g4;

                  w[1][0] = g2 * g2;
                  w[1][1] = g5 * g5;
                  w[1][2] = g8 * g8;
                  w[1][3] = w2 * g5 * g8;
                  w[1][4] = w2 * g2 * g8;
                  w[1][5] = w2 * g2 * g5;

                  w[2][0] = g3 * g3;
                  w[2][1] = g6 * g6;
                  w[2][2] = g9 * g9;
                  w[2][3] = w2 * g6 * g9;
                  w[2][4] = w2 * g3 * g9;
                  w[2][5] = w2 * g3 * g6;

                  w[3][0] = w2 * g2 * g3;
                  w[3][1] = w2 * g5 * g6;
                  w[3][2] = w2 * g8 * g9;
                  w[3][3] = g5 * g9 + g6 * g8;
                  w[3][4] = g3 * g8 + g2 * g9;
                  w[3][5] = g2 * g6 + g3 * g5;

                  w[4][0] = w2 * g1 * g3;
                  w[4][1] = w2 * g4 * g6;
                  w[4][2] = w2 * g7 * g9;
                  w[4][3] = g6 * g7 + g4 * g9;
                  w[4][4] = g1 * g9 + g3 * g7;
                  w[4][5] = g3 * g4 + g1 * g6;

                  w[5][0] = w2 * g1 * g2;
                  w[5][1] = w2 * g4 * g5;
                  w[5][2] = w2 * g7 * g8;
                  w[5][3] = g4 * g8 + g5 * g7;
                  w[5][4] = g2 * g7 + g1 * g8;
                  w[5][5] = g1 * g5 + g2 * g4;

                  for (int kw = 0; kw < 21; kw++) {
                    int i = mi[kw];
                    int j = mj[kw];
                    for (int kws = 0; kws < 36; kws++)
                      wwarim[kw][kws] += w[i][mi[kws]] * w[j][mj[kws]] * fcon;
                  }
                }
              }
            }
          }
//        } else phon = 1.0;
/*        for (int kw = 0; kw < 21; kw++) {
//          int i = mi[kw];
//          int j = mj[kw];
          for (int kws = 0; kws < 36; kws++)
            wwarim[kw][kws] += wwarir[kw][kws] * phon;
        }*/
      }
      for (int kw = 0; kw < 21; kw++) {
        int i = mi[kw];
        int j = mj[kw];
        esa[i][j] = 0.0;
        for (int kws = 0; kws < 36; kws++) {
          is = mi[kws];
          js = mj[kws];
          esa[i][j] += wwarim[kw][kws] * e0s[is][js];
        }
        esa[j][i] = esa[i][j];
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          fak = 1.0;
          if (i > 2) fak = w2;
          if (j > 2) fak *= w2;
          ea[i][j] = esa[i][j] / fak;
          tensorHomo[icyc][i][j] = ea[i][j];
        }
      }
      for (int kw = 0; kw < 21; kw++) {
        int i = mi[kw];
        int j = mj[kw];
        einvsa[i][j] = 0.;
        for (int kws = 0; kws < 36; kws++) {
          is = mi[kws];
          js = mj[kws];
          einvsa[i][j] += wwarim[kw][kws] * e0invs[is][js];
        }
        einvsa[j][i] = einvsa[i][j];
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          fak = 1.0;
          if (i > 2) fak = w2;
          if (j > 2) fak *= w2;
          einva[i][j] = einvsa[i][j] / fak;
        }
      }
      if (eisrs1(einvsa, eigw0, p0) == 1) // problem
	      return tensorHomo;
//      for (int i = 0; i < 6; i++) {
//      }
      for (int i = 0; i < 6; i++)
        eigwin[i] = 1.0 / eigw0[i];
      for (int i = 0; i < 6; i++) {
        for (int j = i; j < 6; j++) {
          einvsain[i][j] = 0.0;
          for (int k = 0; k < 6; k++)
            einvsain[i][j] += p0[i][k] * p0[j][k] * eigwin[k];
          einvsain[j][i] = einvsain[i][j];
        }
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          fak = 1.0;
          if (i > 2) fak = w2;
          if (j > 2) fak *= w2;
          einvainv[i][j] = einvsain[i][j] / fak;
          tensorHomo[2 + icyc][i][j] = einvainv[i][j];
        }
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          ehill[i][j] = (ea[i][j] + einvainv[i][j]) / 2.0;
          tensorHomo[4 + icyc][i][j] = ehill[i][j];
        }
      }
/*      if (itend != 0) {
        double[][][] shill = suphill(ea, einva, itend);
        for (int i = 0; i < 6; i++) {
          for (int j = 0; j < 6; j++) {
            tensorHomo[6 + icyc][i][j] = shill[0][i][j];
            tensorHomo[8 + icyc][i][j] = shill[1][i][j];
          }
        }
      }*/
      for (int kw = 0; kw < 21; kw++) {
        int i = mi[kw];
        int j = mj[kw];
        hlogea[i][j] = 0.0;
        for (int kws = 0; kws < 36; kws++) {
          is = mi[kws];
          js = mj[kws];
          hlogea[i][j] += wwarim[kw][kws] * hloge0[is][js];
        }
        hlogea[j][i] = hlogea[i][j];
      }
      if (eisrs1(hlogea, eigwl, ps) == 1) // problem
	      return tensorHomo;
//      for (int i = 0; i < 6; i++) {
//      }
      for (int igreek = 0; igreek < 6; igreek++) {
        for (int jgreek = igreek; jgreek < 6; jgreek++) {
          egeoms[igreek][jgreek] = 0.0;
          for (int k = 0; k < 6; k++)
            egeoms[igreek][jgreek] += ps[igreek][k] * ps[jgreek][k] * Math.exp(eigwl[k]);
          egeoms[jgreek][igreek] = egeoms[igreek][jgreek];
        }
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          fak = 1.0;
          if (i > 2) fak = w2;
          if (j > 2) fak *= w2;
          egeom[i][j] = egeoms[i][j] / fak;
          tensorHomo[6 + icyc][i][j] = egeom[i][j];
        }
      }
      if (icyc == 1)
        return tensorHomo;
      for (int i = 0; i < 6; i++)
        System.arraycopy(e0inv[i], 0, e0[i], 0, 6);

//      print("e0 final", e0);
    } while (icyc < 1);
    return tensorHomo;
  }

  public static double[][][] tensorGEO4(double[][] e0, double[][][] fio) {
/*
c
c!!! This version for BEARTEX is simplified. It always assumes that input
c!!! are stiffnesses. It calculates first stiffnesses, then compliances
c
C       PROGRAM GEO4ELA.FOR                                S.M. Nov.94
C
C    Origin -> Geot4efp.for S.Matthies, march 1993, Universite de Metz, LM2P
C
C    The program determines the geometric mean and other "means" basing on
C    the arithmetic mean algorithm :
C
C    arithmetic mean                           "Voigt"
C    arithmetic mean of the inverse tensor     "REUSS"
C    HILL approximation                        "HILL "
C    SUPERHILL mean                            "SUPERHILL"
C    Geometric mean                            "GEOM"
C
C    for the elasticity tensors (i.e. twice symmetric tensors of rank 4) for
C    a given orientation distribution.
C
C    The whole calculating cycle is repeated for the inverse INPUT data E0.
C                                                            *****
C    Used INPUT version of the ODF (IRANDOM=O) :  WIMV Standard FORMAT
C         *****
C    (i.e. 5 degree grid and special choice of the elementary region)
C    (ALPHA,BETA,GAMMA)-version of the EULERian angles
C    GAMMA SECTIONS ---see SUBROUTINE ODFINP---
C
C    For IRANDOM=1 random ODF f(g)=1 is assumed -  ODF-INPUT not necessary
C
C    For IRED=1 a reduced ODF of type f~(g) not necessary >0 can be used
C
C    INPUT of sample and crystal symmetry by IGA,IGB (for IRANDOM=0 only)
C    *****
C
C    Code numbers IGA IGB :
C
C    Sample  symmetry              D2 C2 C1
C    Crystal symmetry  O  T  D4 C4 D2 C2 C1 D6 C6 D3 C3
C    Code number       7  6   5  4  3  2  1 11 10  9  8
C
C    Input of the true values (! not renormalized by Voigt or Wooster schemes)
C    *****
C    E0(i1i2,j1j2) (i,j=1,2,3) of the elastic tensor components (stiffness or
C    compliance) using the Voigt Index Code :
C
C    I   1  2  3  4  5  6  enlarged code  7  8  9
C
C    i1  1  2  3  2  3  1                 3  1  2
C
C    i2  1  2  3  3  1  2                 2  3  1
C
C       If E (E0) corresponds to the stiffness C so EINV means compliance S
C       and reverse
C
C    REAL INPUT : Only the 21 numbers (upper triangle)
C         *****
C
C                 E0(I,J)  I=1,6 ; J=I,6
C
C       Below an example for an E0-DATA FILE is given. True, not modified
C     values !!!  22 strokes (Textstroke FORMAT A70)  :
C
C       TEXT stroke, e.g. Copper,stiffness
c       168.4           E0(1,1)
c       121.4           E0(1,2)
c       121.4           E0(1,3)
c       0.              E0(1,4)
c       0.              E0(1,5)
c       0.              E0(1,6)
c       168.4           E0(2,2)
c       121.4           E0(2,3)
c       0.              E0(2,4)
c       0.              E0(2,5)
c       0.              E0(2,6)
c       168.4           E0(3,3)
c       0.              E0(3,4)
c       0.              E0(3,5)
c       0.              E0(3,6)
c       75.5            E0(4,4)
c       0.              E0(4,5)
c       0.              E0(4,6)
c       75.5            E0(5,5)
c       0.              E0(5,6)
c       75.5            E0(6,6)
C
C    OUTPUT : FILE GEOT4ELA.LST.
C
C    It contains the means in form of (6X6 T-unreduced matrices), e.g.
C    EGEOM(6,6) a.s.o. Using these data the transformation to a (9x9)
C    schema is trivial.
C    (I,J=7,8,9 correspond to 4,5,6). The (6*6) data can also be used
C    to create Voigt or WOOSTER modified elastic matrix elements.
*/
    int[] mi = {0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 1, 2, 3,
        4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5};
    int[] mj = {0, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5, 0, 0, 0,
        0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4};
    int is, js;
    double cga, fak, sga, fcon;
    double[] cosw = new double[i73], sinw = new double[i73];
    double[][] e0s = new double[6][6];
    double[][] hloge0 = new double[6][6];
    double[][] hlogea = new double[6][6];
    double[][] egeom = new double[6][6];
    double[][] egeoms = new double[6][6];
    double[] eigw0 = new double[6];
    double[] eigwl = new double[6];
    double[] eiglog = new double[6];
    double[][] p0 = new double[6][6];
    double[][] ps = new double[6][6];
    double[][] wwarim = new double[36][36];
    double[][] w = new double[6][6];
    double[][][] tensorHomo = new double[1][6][6];

    fttcon1(fio, ftt);

    for (int i = 0; i < i73; i++) {
      double arg = pi5 * i;
      cosw[i] = Math.cos(arg);
      sinw[i] = Math.sin(arg);
//      ifiw[i] = i * 5;
    }
    for (int igreek = 0; igreek < 6; igreek++) {
      for (int jgreek = igreek; jgreek < 6; jgreek++) {
        fak = 1.0;
        if (igreek > 2) fak *= w2;
        if (jgreek > 2) fak *= w2;
        e0s[igreek][jgreek] = e0[igreek][jgreek] * fak;
        e0s[jgreek][igreek] = e0s[igreek][jgreek];
        e0[jgreek][igreek] = e0[igreek][jgreek];
      }
    }
    print("    E0-matrix :", e0);
    print("    E0S-matrix :", e0s);
    if (eisrs1(e0s, eigw0, p0) == 1) // problem
	    return tensorHomo;
    print(" EIGENVALUES of E0S and P0-matrix  (P0-1)*E0S*P0 =", eigw0);
    print("  MATRIX P0", p0);
    for (int i = 0; i < 6; i++)
      eiglog[i] = Math.log(eigw0[i]);
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        hloge0[i][j] = 0.0;
        for (int k = 0; k < 6; k++)
          hloge0[i][j] += p0[i][k] * p0[j][k] * eiglog[k];
        hloge0[j][i] = hloge0[i][j];
      }
    }
    for (int kw = 0; kw < 21; kw++) {
      for (int kws = 0; kws < 36; kws++) {
        wwarim[kw][kws] = 0.0;
      }
    }
//        if (irandom != 0)
//          phon = 1.0001;
    double[][] wwarir = warir(0.0);
//    if (phon < 1.0) {
      for (int nga = 0; nga < i73; nga++) {
        sga = sinw[nga];
        cga = cosw[nga];
        for (int nb = 0; nb < i37; nb++) {
          double sb = sinw[nb];
          double g9 = cosw[nb];   // cb
          for (int na = 0; na < i73; na++) {
            fcon = ftt[na][nb][nga];
            if (fcon != 0.0) {

              double sa = sinw[na];
              double ca = cosw[na];
              double g1 = ca * g9 * cga - sa * sga;
              double g2 = sa * g9 * cga + ca * sga;
              double g3 = -sb * cga;
              double g4 = -ca * g9 * sga - sa * cga;
              double g5 = -sa * g9 * sga + ca * cga;
              double g6 = sb * sga;
              double g7 = ca * sb;
              double g8 = sa * sb;
//                  double g9 = cb;

              w[0][0] = g1 * g1;
              w[0][1] = g4 * g4;
              w[0][2] = g7 * g7;
              w[0][3] = w2 * g4 * g7;
              w[0][4] = w2 * g1 * g7;
              w[0][5] = w2 * g1 * g4;

              w[1][0] = g2 * g2;
              w[1][1] = g5 * g5;
              w[1][2] = g8 * g8;
              w[1][3] = w2 * g5 * g8;
              w[1][4] = w2 * g2 * g8;
              w[1][5] = w2 * g2 * g5;

              w[2][0] = g3 * g3;
              w[2][1] = g6 * g6;
              w[2][2] = g9 * g9;
              w[2][3] = w2 * g6 * g9;
              w[2][4] = w2 * g3 * g9;
              w[2][5] = w2 * g3 * g6;

              w[3][0] = w2 * g2 * g3;
              w[3][1] = w2 * g5 * g6;
              w[3][2] = w2 * g8 * g9;
              w[3][3] = g5 * g9 + g6 * g8;
              w[3][4] = g3 * g8 + g2 * g9;
              w[3][5] = g2 * g6 + g3 * g5;

              w[4][0] = w2 * g1 * g3;
              w[4][1] = w2 * g4 * g6;
              w[4][2] = w2 * g7 * g9;
              w[4][3] = g6 * g7 + g4 * g9;
              w[4][4] = g1 * g9 + g3 * g7;
              w[4][5] = g3 * g4 + g1 * g6;

              w[5][0] = w2 * g1 * g2;
              w[5][1] = w2 * g4 * g5;
              w[5][2] = w2 * g7 * g8;
              w[5][3] = g4 * g8 + g5 * g7;
              w[5][4] = g2 * g7 + g1 * g8;
              w[5][5] = g1 * g5 + g2 * g4;

              for (int kw = 0; kw < 21; kw++) {
                int i = mi[kw];
                int j = mj[kw];
                for (int kws = 0; kws < 36; kws++)
                  wwarim[kw][kws] += w[i][mi[kws]] * w[j][mj[kws]] * fcon;
              }
            }
          }
        }
      }
//    } else phon = 1.0;
/*    for (int kw = 0; kw < 21; kw++) {
//          int i = mi[kw];
//          int j = mj[kw];
      for (int kws = 0; kws < 36; kws++)
        wwarim[kw][kws] += wwarir[kw][kws] * phon;
    }*/

    for (int kw = 0; kw < 21; kw++) {
      int i = mi[kw];
      int j = mj[kw];
      hlogea[i][j] = 0.0;
      for (int kws = 0; kws < 36; kws++) {
        is = mi[kws];
        js = mj[kws];
        hlogea[i][j] += wwarim[kw][kws] * hloge0[is][js];
      }
      hlogea[j][i] = hlogea[i][j];
    }
    if (eisrs1(hlogea, eigwl, ps) == 1) // problem
	    return tensorHomo;
//      for (int i = 0; i < 6; i++) {
//      }
    for (int igreek = 0; igreek < 6; igreek++) {
      for (int jgreek = igreek; jgreek < 6; jgreek++) {
        egeoms[igreek][jgreek] = 0.0;
        for (int k = 0; k < 6; k++)
          egeoms[igreek][jgreek] += ps[igreek][k] * ps[jgreek][k] * Math.exp(eigwl[k]);
        egeoms[jgreek][igreek] = egeoms[igreek][jgreek];
      }
    }
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        fak = 1.0;
        if (i > 2) fak = w2;
        if (j > 2) fak *= w2;
        egeom[i][j] = egeoms[i][j] / fak;
        tensorHomo[0][i][j] = egeom[i][j];
      }
    }
    return tensorHomo;
  }

  public static double[][][] tensor4(double[][] e0, double[][][] fio, int model) {
/*
c
c!!! This version for BEARTEX is simplified. It always assumes that input
c!!! are stiffnesses. It calculates first stiffnesses, then compliances
c
C       PROGRAM GEO4ELA.FOR                                S.M. Nov.94
C
C    Origin -> Geot4efp.for S.Matthies, march 1993, Universite de Metz, LM2P
C
C    The program determines the geometric mean and other "means" basing on
C    the arithmetic mean algorithm :
C
C    arithmetic mean                           "Voigt"
C    arithmetic mean of the inverse tensor     "REUSS"
C    HILL approximation                        "HILL "
C    SUPERHILL mean                            "SUPERHILL"
C    Geometric mean                            "GEOM"
C
C    for the elasticity tensors (i.e. twice symmetric tensors of rank 4) for
C    a given orientation distribution.
C
C    The whole calculating cycle is repeated for the inverse INPUT data E0.
C                                                            *****
C    Used INPUT version of the ODF (IRANDOM=O) :  WIMV Standard FORMAT
C         *****
C    (i.e. 5 degree grid and special choice of the elementary region)
C    (ALPHA,BETA,GAMMA)-version of the EULERian angles
C    GAMMA SECTIONS ---see SUBROUTINE ODFINP---
C
C    For IRANDOM=1 random ODF f(g)=1 is assumed -  ODF-INPUT not necessary
C
C    For IRED=1 a reduced ODF of type f~(g) not necessary >0 can be used
C
C    INPUT of sample and crystal symmetry by IGA,IGB (for IRANDOM=0 only)
C    *****
C
C    Code numbers IGA IGB :
C
C    Sample  symmetry              D2 C2 C1
C    Crystal symmetry  O  T  D4 C4 D2 C2 C1 D6 C6 D3 C3
C    Code number       7  6   5  4  3  2  1 11 10  9  8
C
C    Input of the true values (! not renormalized by Voigt or Wooster schemes)
C    *****
C    E0(i1i2,j1j2) (i,j=1,2,3) of the elastic tensor components (stiffness or
C    compliance) using the Voigt Index Code :
C
C    I   1  2  3  4  5  6  enlarged code  7  8  9
C
C    i1  1  2  3  2  3  1                 3  1  2
C
C    i2  1  2  3  3  1  2                 2  3  1
C
C       If E (E0) corresponds to the stiffness C so EINV means compliance S
C       and reverse
C
C    REAL INPUT : Only the 21 numbers (upper triangle)
C         *****
C
C                 E0(I,J)  I=1,6 ; J=I,6
C
C       Below an example for an E0-DATA FILE is given. True, not modified
C     values !!!  22 strokes (Textstroke FORMAT A70)  :
C
C       TEXT stroke, e.g. Copper,stiffness
c       168.4           E0(1,1)
c       121.4           E0(1,2)
c       121.4           E0(1,3)
c       0.              E0(1,4)
c       0.              E0(1,5)
c       0.              E0(1,6)
c       168.4           E0(2,2)
c       121.4           E0(2,3)
c       0.              E0(2,4)
c       0.              E0(2,5)
c       0.              E0(2,6)
c       168.4           E0(3,3)
c       0.              E0(3,4)
c       0.              E0(3,5)
c       0.              E0(3,6)
c       75.5            E0(4,4)
c       0.              E0(4,5)
c       0.              E0(4,6)
c       75.5            E0(5,5)
c       0.              E0(5,6)
c       75.5            E0(6,6)
C
C    OUTPUT : FILE GEOT4ELA.LST.
C
C    It contains the means in form of (6X6 T-unreduced matrices), e.g.
C    EGEOM(6,6) a.s.o. Using these data the transformation to a (9x9)
C    schema is trivial.
C    (I,J=7,8,9 correspond to 4,5,6). The (6*6) data can also be used
C    to create Voigt or WOOSTER modified elastic matrix elements.
*/
    int[] mi = {0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 1, 2, 3,
        4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5};
    int[] mj = {0, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5, 0, 0, 0,
        0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4};
    int is, js;
    double cga, fak, sga, fcon;
    double[] cosw = new double[i73], sinw = new double[i73];
    double[][] e0s = new double[6][6];
    double[][] esa = new double[6][6];
    double[][] ea = new double[6][6];
    double[][] e0inv = new double[6][6];
    double[][] e0invs = new double[6][6];
    double[][] einvsa = new double[6][6];
    double[][] einvsain = new double[6][6];
    double[][] einva = new double[6][6];
    double[][] einvainv = new double[6][6];
    double[][] hloge0 = new double[6][6];
    double[][] hlogea = new double[6][6];
    double[][] ehill = new double[6][6];
    double[][] egeom = new double[6][6];
    double[][] egeoms = new double[6][6];
    double[] eigw0 = new double[6];
    double[] eigwl = new double[6];
    double[] eiglog = new double[6];
    double[] eigwin = new double[6];
    double[][] p0 = new double[6][6];
    double[][] ps = new double[6][6];
    double[][] wwarim = new double[36][36];
    double[][] w = new double[6][6];
    double[][][] tensorHomo = new double[1][6][6];

    fttcon1(fio, ftt);

    for (int i = 0; i < i73; i++) {
      double arg = pi5 * i;
      cosw[i] = Math.cos(arg);
      sinw[i] = Math.sin(arg);
//      ifiw[i] = i * 5;
    }
    for (int igreek = 0; igreek < 6; igreek++) {
      for (int jgreek = igreek; jgreek < 6; jgreek++) {
        fak = 1.0;
        if (igreek > 2) fak *= w2;
        if (jgreek > 2) fak *= w2;
        e0s[igreek][jgreek] = e0[igreek][jgreek] * fak;
        e0s[jgreek][igreek] = e0s[igreek][jgreek];
        e0[jgreek][igreek] = e0[igreek][jgreek];
      }
    }
    print("    E0-matrix :", e0);
    print("    E0S-matrix :", e0s);
    if (eisrs1(e0s, eigw0, p0) == 1) // problem
	    return tensorHomo;
	  print(" EIGENVALUES of E0S and P0-matrix  (P0-1)*E0S*P0 =", eigw0);
    print("  MATRIX P0", p0);
    for (int i = 0; i < 6; i++)
      eiglog[i] = Math.log(eigw0[i]);
    for (int i = 0; i < 6; i++)
      eigwin[i] = 1.0 / eigw0[i];
    for (int i = 0; i < 6; i++) {
      for (int j = i; j < 6; j++) {
        e0invs[i][j] = 0.0;
        for (int k = 0; k < 6; k++)
          e0invs[i][j] += p0[i][k] * p0[j][k] * eigwin[k];
        e0invs[j][i] = e0invs[i][j];
      }
    }
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        fak = 1.0;
        if (i > 2) fak = w2;
        if (j > 2) fak *= w2;
        e0inv[i][j] = e0invs[i][j] / fak;
      }
    }
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        hloge0[i][j] = 0.0;
        for (int k = 0; k < 6; k++)
          hloge0[i][j] += p0[i][k] * p0[j][k] * eiglog[k];
        hloge0[j][i] = hloge0[i][j];
      }
    }
    for (int kw = 0; kw < 21; kw++) {
      for (int kws = 0; kws < 36; kws++) {
        wwarim[kw][kws] = 0.0;
      }
    }
//        if (irandom != 0)
//          phon = 1.0001;
    double[][] wwarir = warir(0.0);
//    if (phon < 1.0) {
      for (int nga = 0; nga < i73; nga++) {
        sga = sinw[nga];
        cga = cosw[nga];
        for (int nb = 0; nb < i37; nb++) {
          double sb = sinw[nb];
          double g9 = cosw[nb];   // cb
          for (int na = 0; na < i73; na++) {
            fcon = ftt[na][nb][nga];
            if (fcon != 0.0) {

              double sa = sinw[na];
              double ca = cosw[na];
              double g1 = ca * g9 * cga - sa * sga;
              double g2 = sa * g9 * cga + ca * sga;
              double g3 = -sb * cga;
              double g4 = -ca * g9 * sga - sa * cga;
              double g5 = -sa * g9 * sga + ca * cga;
              double g6 = sb * sga;
              double g7 = ca * sb;
              double g8 = sa * sb;
//                  double g9 = cb;

              w[0][0] = g1 * g1;
              w[0][1] = g4 * g4;
              w[0][2] = g7 * g7;
              w[0][3] = w2 * g4 * g7;
              w[0][4] = w2 * g1 * g7;
              w[0][5] = w2 * g1 * g4;

              w[1][0] = g2 * g2;
              w[1][1] = g5 * g5;
              w[1][2] = g8 * g8;
              w[1][3] = w2 * g5 * g8;
              w[1][4] = w2 * g2 * g8;
              w[1][5] = w2 * g2 * g5;

              w[2][0] = g3 * g3;
              w[2][1] = g6 * g6;
              w[2][2] = g9 * g9;
              w[2][3] = w2 * g6 * g9;
              w[2][4] = w2 * g3 * g9;
              w[2][5] = w2 * g3 * g6;

              w[3][0] = w2 * g2 * g3;
              w[3][1] = w2 * g5 * g6;
              w[3][2] = w2 * g8 * g9;
              w[3][3] = g5 * g9 + g6 * g8;
              w[3][4] = g3 * g8 + g2 * g9;
              w[3][5] = g2 * g6 + g3 * g5;

              w[4][0] = w2 * g1 * g3;
              w[4][1] = w2 * g4 * g6;
              w[4][2] = w2 * g7 * g9;
              w[4][3] = g6 * g7 + g4 * g9;
              w[4][4] = g1 * g9 + g3 * g7;
              w[4][5] = g3 * g4 + g1 * g6;

              w[5][0] = w2 * g1 * g2;
              w[5][1] = w2 * g4 * g5;
              w[5][2] = w2 * g7 * g8;
              w[5][3] = g4 * g8 + g5 * g7;
              w[5][4] = g2 * g7 + g1 * g8;
              w[5][5] = g1 * g5 + g2 * g4;

              for (int kw = 0; kw < 21; kw++) {
                int i = mi[kw];
                int j = mj[kw];
                for (int kws = 0; kws < 36; kws++)
                  wwarim[kw][kws] += w[i][mi[kws]] * w[j][mj[kws]] * fcon;
              }
            }
          }
        }
      }
//    } else phon = 1.0;
/*    for (int kw = 0; kw < 21; kw++) {
//          int i = mi[kw];
//          int j = mj[kw];
      for (int kws = 0; kws < 36; kws++)
        wwarim[kw][kws] += wwarir[kw][kws] * phon;
    }*/

    if (model == VOIGT_STIFFNESS || model == HILL_STIFFNESS) {
      for (int kw = 0; kw < 21; kw++) {
        int i = mi[kw];
        int j = mj[kw];
        esa[i][j] = 0.0;
        for (int kws = 0; kws < 36; kws++) {
          is = mi[kws];
          js = mj[kws];
          esa[i][j] += wwarim[kw][kws] * e0s[is][js];
        }
        esa[j][i] = esa[i][j];
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          fak = 1.0;
          if (i > 2) fak = w2;
          if (j > 2) fak *= w2;
          ea[i][j] = esa[i][j] / fak;
          if (model == 0)
            tensorHomo[0][i][j] = ea[i][j];
        }
      }
      if (model == VOIGT_STIFFNESS)
        return tensorHomo;
    }
    if (model == REUSS_STIFFNESS || model == HILL_STIFFNESS) {
      for (int kw = 0; kw < 21; kw++) {
        int i = mi[kw];
        int j = mj[kw];
        einvsa[i][j] = 0.;
        for (int kws = 0; kws < 36; kws++) {
          is = mi[kws];
          js = mj[kws];
          einvsa[i][j] += wwarim[kw][kws] * e0invs[is][js];
        }
        einvsa[j][i] = einvsa[i][j];
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          fak = 1.0;
          if (i > 2) fak = w2;
          if (j > 2) fak *= w2;
          einva[i][j] = einvsa[i][j] / fak;
        }
      }
      if (eisrs1(einvsa, eigw0, p0) == 1) // problem
	      return tensorHomo;
//      for (int i = 0; i < 6; i++) {
//      }
      for (int i = 0; i < 6; i++)
        eigwin[i] = 1.0 / eigw0[i];
      for (int i = 0; i < 6; i++) {
        for (int j = i; j < 6; j++) {
          einvsain[i][j] = 0.0;
          for (int k = 0; k < 6; k++)
            einvsain[i][j] += p0[i][k] * p0[j][k] * eigwin[k];
          einvsain[j][i] = einvsain[i][j];
        }
      }
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          fak = 1.0;
          if (i > 2) fak = w2;
          if (j > 2) fak *= w2;
          einvainv[i][j] = einvsain[i][j] / fak;
          tensorHomo[0][i][j] = einvainv[i][j];
        }
      }
      if (model == REUSS_STIFFNESS)
        return tensorHomo;
    }
    if (model == HILL_STIFFNESS) {
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          ehill[i][j] = (ea[i][j] + einvainv[i][j]) / 2.0;
          tensorHomo[0][i][j] = ehill[i][j];
        }
      }
      return tensorHomo;
    }
    for (int kw = 0; kw < 21; kw++) {
      int i = mi[kw];
      int j = mj[kw];
      hlogea[i][j] = 0.0;
      for (int kws = 0; kws < 36; kws++) {
        is = mi[kws];
        js = mj[kws];
        hlogea[i][j] += wwarim[kw][kws] * hloge0[is][js];
      }
      hlogea[j][i] = hlogea[i][j];
    }
    if (eisrs1(hlogea, eigwl, ps) == 1)  // problem
	    return tensorHomo;
//      for (int i = 0; i < 6; i++) {
//      }
    for (int igreek = 0; igreek < 6; igreek++) {
      for (int jgreek = igreek; jgreek < 6; jgreek++) {
        egeoms[igreek][jgreek] = 0.0;
        for (int k = 0; k < 6; k++)
          egeoms[igreek][jgreek] += ps[igreek][k] * ps[jgreek][k] * Math.exp(eigwl[k]);
        egeoms[jgreek][igreek] = egeoms[igreek][jgreek];
      }
    }
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        fak = 1.0;
        if (i > 2) fak = w2;
        if (j > 2) fak *= w2;
        egeom[i][j] = egeoms[i][j] / fak;
        tensorHomo[0][i][j] = egeom[i][j];
      }
    }
    return tensorHomo;
  }

/*  private static double[][] multMatrix(double[][] a, double[][] b) {
    int dim1 = a.length;
    int dim2 = a[0].length;
    int dim2b = b.length;
    int dim3 = b[0].length;
    if (dim2 != dim2b)
      return null;
    double[][] result = new double[dim1][dim3];
    for (int i = 0; i < dim1; i++) {
      for (int j = 0; j < dim3; j++) {
        for (int ij = 0; ij < dim2; ij++) {
          result[i][j] += a[i][ij] * b[ij][j];
        }
      }
    }
    return result;
  }*/

  private static double fttcon1(double[][][] fio, double[][][] ftt) {
/*
  C       VARIANT WITH VG/(8PI*PI)
  C
  C       Attention FTT-structure (73,37,73)
  C
  C       FTT(ALPHA,BETA,GAMMA)=
  C       FIO(ALPHA,BETA,GAMMA)*VG(ALPHA,BETA,GAMMA)/(8PI*PI)
  C       COMPLETE G-SPACE CONSIDERED, 5 degree STEPS.
  C     FOR IRED=0 :
  C       BECAUSE FIO WAS NORMALIZED EARLIER FOR SPEED FIO VALUES LOWER 0.005
  C       CAN BE NEGLECTED DUE TO THEIR SMALL CONTRIBUTION TO THE MEAN VALUE.
*/

    double a, b, fa, hva, hvg, hvag;
    double[] hvbtt = new double[i37];

    for (int ib = 0; ib < i37; ib++) {
      if (ib <= 0) {
        a = 0.0;
        b = pi25;
      } else {
        a = pi5 * ib - pi25;
        b = a + pi5;
        if (ib == i37 - 1) {
          b = a + pi25;
        }
      }
      hvbtt[ib] = Math.cos(a) - Math.cos(b);
    }
    double sum = 0.0;
    for (int ia = 0; ia < i73; ia++) {
      if (ia <= 0 || ia >= i73 - 1)
        hva = pi25 / picon;
      else
        hva = pi5 / picon;
      for (int ig = 0; ig < i73; ig++) {
        if (ig <= 0 || ig >= i73 - 1)
          hvg = pi25;
        else
          hvg = pi5;
        hvag = hva * hvg;
        for (int ib = 0; ib < i37; ib++) {
          fa = fio[ia][ib][ig] * hvag * hvbtt[ib];
          sum += fa;
          ftt[ia][ib][ig] = (double) fa;
        }
      }
    }
//    System.out.println("Integral of odf: " + sum);
    if (sum > 1.00001 || sum < 0.999999)
      for (int ia = 0; ia < i73; ia++)
        for (int ig = 0; ig < i73; ig++)
          for (int ib = 0; ib < i37; ib++)
            ftt[ia][ib][ig] /= sum;
    return 1.0;
  }

  /*
  @param ar   Square matrix
  @param ar   eigenvalues
  @param ar   eigenvectors
  @return     error
  */
  private static int eisrs1(double[][] ar, double[] wr, double[][] zr) {
/*     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
    C     SYMMETRIC MATRIX
*/
//    double[] work = tred2(ar, wr, zr);
//    return tql2(wr, zr, work);
    int dim3 = wr.length;
	  int dim1 = ar.length;
	  int dim2 = ar[0].length;
	  boolean invalid = false;
	  for (int i = 0; i < dim1 && !invalid; i++)
		  for (int j = 0; j < dim2 && !invalid; j++)
			  if (Double.isNaN(ar[i][j]))
				  invalid = true;
	  if (invalid) {
		  print("Invalid matrix in the eigenvalue decomposition, not able to homogenize");
		  return 1;
	  }
	  print("Start eigenvalue decomposition:");
	  EigenvalueDecomposition eigenD = new EigenvalueDecomposition(new Matrix(ar));
	  print("Retrieve now eigenvalues...");
	  double[][] zr1 = eigenD.getEigenVector();
    double[] wr1 = eigenD.getRealEigenvalues();
    for (int i = 0; i < dim3; i++) {
      wr[i] = wr1[i];
      System.arraycopy(zr1[i], 0, zr[i], 0, dim3);
    }
//    print("z mat", zr);
    return 0;
  }

/*  private static double[] tred2(double[][] a, double[] d, double[][] z) {
    double f, g, h, hh, scale;

    print("a1 matrix", a);
    print("d1 matrix", d);
    int dim3 = d.length;
//    int z_dim1 = a.length;
    double[] e = new double[dim3];
    for (int i = 0; i < dim3; i++) {
//      d[i] = 0.0;
      for (int j = 0; j <= i; j++) {
        z[i][j] = a[i][j];
      }
    }
    if (dim3 != 1) {
//      i__2 = dim3;
      for (int ii = 1; ii < dim3; ii++) {
        int i = dim3 - ii;
        int l = i - 1;
        h = 0.0;
        scale = 0.0;
//        System.out.println("ii " + ii + " " + i + " " + l + " " + h + " " + scale);
        if (l >= 1) {
          for (int k = 0; k <= l; k++) {
            scale += Math.abs(z[i][k]);
          }
          if (scale != 0.0) {
            for (int k = 0; k <= l; k++) {
              z[i][k] /= scale;
              h += z[i][k] * z[i][k];
            }
            f = z[i][l];
            g = -MoreMath.sign(Math.sqrt(h), f);
            e[i] = scale * g;
//            System.out.println("e1 " + i + " " + e[i]);
            h = h - f * g;
            z[i][l] = f - g;
            f = 0.0;
//            System.out.println(l);
            for (int j = 0; j <= l; j++) {
              z[j][i] = z[i][j] / (scale * h);
              g = 0.0f;
              for (int k = 0; k <= j; k++)
                g += z[j][k] * z[i][k];
              int jp1 = j + 1;
              if (l >= jp1) {
                for (int k = jp1; k <= l; k++)
                  g += z[k][j] * z[i][k];
              }
              e[j] = g / h;
//              System.out.println("e2 " + j + " " + e[j]);
              f += e[j] * z[i][j];
            }
            hh = f / (h + h);
            for (int j = 0; j <= l; j++) {
              f = z[i][j];
              g = (e[j] - hh * f);
              e[j] = g;
//              System.out.println("e3 " + j + " " + e[j]);
              for (int k = 0; k <= j; k++)
                z[j][k] = z[j][k] - f * e[k] - g * z[i][k];
            }
            for (int k = 0; k <= l; k++)
              z[i][k] = scale * z[i][k];
          } else {
            e[i] = z[i][l];
//            System.out.println("e4 " + i + " " + e[i]);
          }
        } else {
            e[i] = z[i][l];
//            System.out.println("e4 " + i + " " + e[i]);
        }
        d[i] = h;
      }
    }
    d[0] = 0.0;
    e[0] = 0.0;
    for (int i = 0; i < dim3;  i++) {
      int l = i - 1;
      if (d[i] != 0.0) {
        for (int j = 0; j <= l;  j++) {
          g = 0.0f;
          for (int k = 0; k <= l; k++)
            g += z[i][k] * z[k][j];
          for (int k = 0; k <= l; k++)
            z[k][j] = z[k][j] - g * z[k][i];
        }
      }
      d[i] = z[i][i];
      z[i][i] = 1.0;
      if (l >= 1) {
        for (int j = 0; j <= l; j++) {
          z[i][j] = 0.0;
          z[j][i] = 0.0;
        }
      }
    }
    print("e matrix", e);
    return e;
  }

  private static int tql2(double[] d, double[][] z, double[] e) {
    double b, c, f, g, h, p, r, s;
    int m, mml;

//    int z_dim1 = z.length;
    print("z1 matrix", z);
    print("d1 matrix", d);
    int dim3 = d.length;
    if (dim3 != 1) {
      for (int i = 1; i < dim3; i++)
        e[i - 1] = e[i];
      f = 0;
      b = 0;
      e[dim3 - 1] = 0.0;
      for (int l = 0; l < dim3; l++) {
        int j = 0;
        h = machep * (Math.abs(d[l]) + Math.abs(e[l]));
        if (b < h)
          b = h;
        System.out.println(l + " " + dim3 + " " + j + " " + b);
        for (m = l; m < dim3; m++)
          if (Math.abs(e[m]) <= b) {
            break;
          }
        if (m == 6)
          m--;
        System.out.println(m + " " + l + " " + j);
        if (m != l) {
System.out.println("ml " + m + " " + l + " " + j);
          do {
System.out.println("do " + m + " " + l + " " + j);
            if (j == 30) // no convergence
              return 1;
            j++;
System.out.println("do1 " + m + " " + l + " " + j);
            p = (d[l + 1] - d[l]) / (e[l] * 2);
            r = Math.sqrt(p * p + 1);
            h = d[l] - e[l] / (p + MoreMath.sign(r, p));
            for (int i = l; i < dim3; i++)
              d[i] -= h;
        System.out.println(m + " " + l + " " + f + " " + h);
            f += h;
            p = d[m];
            c = 1;
            s = 0;
            mml = m - l;
            for (int ii = 0; ii < mml; ii++) {
//              if (Math.abs(p) < 1E-6) p = 0.0;
              int i = m - ii - 1;
              g = c * e[i];
              h = c * p;
//              System.out.println(p + " " + e[i]);
              if (Math.abs(p) < Math.abs(e[i])) {
//                System.out.println(e[i] + " / " + p);
                c = p / e[i];
//                System.out.println(c);
                r = Math.sqrt(c * c + 1);
//                System.out.println(s + " " + e[i] + " " + r);
                e[i + 1] = s * e[i] * r;
//                System.out.println(e[i+1] + " " + r);
                s = 1.0 / r;
//                System.out.println(s);
                c *= s;
                System.out.println("crs1 " + c + " " + r + " " + s);
              } else {
                System.out.println("crs2 start " + e[i] + " " + p);
                c = e[i] / p;
                r = Math.sqrt(c * c + 1);
                e[i + 1] = s * p * r;
                s = c / r;
                c = 1.0 / r;
                System.out.println("crs2 " + c + " " + r + " " + s);
              }
              System.out.println("p " + c + " * " + d[i] + " - " + s + " * " + g);
              p = c * d[i] - s * g;
              d[i + 1] = h + s * (c * g + s * d[i]);
              System.out.println("z " + p + " " + h + " " + s + " " + c);
              for (int k = 0; k < dim3; k++) {
                h = z[k][i + 1];
                z[k][i + 1] = s * z[k][i] + c * h;
                z[k][i] = c * z[k][i] - s * h;
              }
            }
            e[l] = s * p;
            d[l] = c * p;
            System.out.println("while "+e[l] + " > " + b);
          } while (Math.abs(e[l]) > b);
        }
System.out.println("F "+l + " " + f + " " + d[l]);
        d[l] += f;
      }
      print("e2 mat", e);
      print("d mat", d);
      print("z2 mat", z);
      for (int ii = 1; ii < dim3; ii++) {
        int i = ii;
        int k = i;
        p = d[i];
        for (int j = ii; j < dim3; j++) {
          if (d[j] < p) {
            k = j;
            p = d[j];
          }
        }
        if (k != i) {
          d[k] = d[i];
          d[i] = p;
          for (int j = 0; j < dim3; j++) {
            p = z[j][i];
            z[j][i] = z[j][k];
            z[j][k] = p;
          }
        }
      }
    }
    print("z mat", z);
    return 0;
  }
 */

  private static double fttcon2(double[][][] fio, double[][][] ftt) {
/*
C       VARIANT WITH VG/(8PI*PI)
C
C       Attention FTT-structure (73,37,73)
C
C       FTT(ALPHA,BETA,GAMMA)=
C       FIO(ALPHA,BETA,GAMMA)*VG(ALPHA,BETA,GAMMA)/(8PI*PI)
C       COMPLETE G-SPACE CONSIDERED, 5 degree STEPS.
C     FOR IRED=0 :
C       BECAUSE FIO WAS NORMALIZED EARLIER FOR SPEED FIO VALUES LOWER 0.005
C       CAN BE NEGLECTED DUE TO THEIR SMALL CONTRIBUTION TO THE MEAN VALUE.
*/
    double a, b, hva, hvg, hvag, fmin;
    double[] hvbtt = new double[i37];
    fmin = 6.66e6;
    for (int ia = 0; ia < i73; ia++) {
      for (int ib = 0; ib < i37; ib++) {
        for (int ig = 0; ig < i73; ig++) {
          if (fio[ia][ib][ig] < fmin)
            fmin = fio[ia][ib][ig];
        }
      }
    }
    double phon = fmin;
    for (int ib = 0; ib < i37; ib++) {
      if (ib <= 0) {
        a = 0.;
        b = pi25;
      } else {
        a = -pi25 + pi5 * ib;
        b = a + pi5;
        if (ib == i37 - 1)
          b = a + pi25;

      }
      hvbtt[ib] = Math.cos(a) - Math.cos(b);
    }
    for (int ia = 0; ia < i73; ia++) {
      if ((ia <= 0) || (ia >= i73 - 1))
        hva = pi25 / picon;
      else
        hva = pi5 / picon;
      for (int ig = 0; ig < i73; ig++) {
        if ((ig <= 0) || (ig >= i73 - 1))
          hvg = pi25;
        else
          hvg = pi5;
        hvag = hva * hvg;
        for (int ib = 0; ib < i37; ib++) {
          ftt[ia][ib][ig] = (double) ((fio[ia][ib][ig] - phon) * hvag * hvbtt[ib]);
        }
      }
    }
    return phon;
  }

  private static double[][] warir(double phon) {
    /*
    C       Analytical formulae for WWARIM  random case (ODF=1)
    */
    int[] mi = {0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 1, 2, 3,
        4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5};
    int[] mj = {0, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 2, 3, 4, 5, 3, 4, 5, 4, 5, 5, 0, 0, 0,
        0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4};
    double[][] wwarir = new double[36][36];

//    if (phon == 0.0)
//      return wwarir;
    for (int kw = 0; kw < 36; kw++) {
      int i = mi[kw];
      int j = mj[kw];
      for (int kws = 0; kws < 36; kws++) {
        int is = mi[kws];
        int js = mj[kws];
        wwarir[kw][kws] = del(i, is, j, js);
      }
    }
    return wwarir;
  }

  private static double del(int i, int is, int j, int js) {
    int[][] mi1 = {{1, 1, 1, 1, 1, 1}, {2, 2, 2, 2, 2, 2}, {3, 3, 3, 3, 3, 3},
        {2, 2, 2, 2, 2, 2}, {1, 1, 1, 1, 1, 1}, {1, 1, 1, 1, 1, 1}};
    int[][] mi2 = {{1, 2, 3, 2, 1, 1}, {1, 2, 3, 2, 1, 1}, {1, 2, 3, 2, 1, 1},
        {1, 2, 3, 2, 3, 1}, {1, 2, 3, 3, 1, 2}, {1, 2, 3, 2, 3, 1}};
    int[][] mi3 = {{1, 1, 1, 1, 1, 1}, {2, 2, 2, 2, 2, 2}, {3, 3, 3, 3, 3, 3},
        {3, 3, 3, 3, 3, 3}, {3, 3, 3, 3, 3, 3}, {2, 2, 2, 2, 2, 2}};
    int[][] mi4 = {{1, 2, 3, 3, 3, 2}, {1, 2, 3, 3, 3, 2}, {1, 2, 3, 3, 3, 2},
        {1, 2, 3, 3, 1, 2}, {1, 2, 3, 2, 3, 1}, {1, 2, 3, 3, 1, 2}};
    int[][] mi5 = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0},
        {0, 0, 0, 2, 2, 2}, {0, 0, 0, 1, 1, 1}, {0, 0, 0, 1, 1, 1}};
    int[][] mi6 = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0},
        {0, 0, 0, 3, 1, 2}, {0, 0, 0, 2, 3, 1}, {0, 0, 0, 3, 1, 2}};
    int[][] mi7 = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0},
        {0, 0, 0, 3, 3, 3}, {0, 0, 0, 3, 3, 3}, {0, 0, 0, 2, 2, 2}};
    int[][] mi8 = {{0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0},
        {0, 0, 0, 2, 3, 1}, {0, 0, 0, 3, 1, 2}, {0, 0, 0, 2, 3, 1}};
    int ii, ij, ik, il, it, iu, iv;
    double fak;
    int iis;
    double delh;
    double res; // = 0.0;
    fak = 1.0;
    if ((i < 3) && (is >= 3)) fak = w2;
    if ((is < 3) && (i >= 3)) fak = w2;
    if ((j < 3) && (js >= 3)) fak *= w2;
    if ((js < 3) && (j >= 3)) fak *= w2;
    ii = mi1[i][is];
    ij = mi3[i][is];
    ik = mi1[j][js];
    il = mi3[j][js];
    iis = mi2[i][is];
    it = mi4[i][is];
    iu = mi2[j][js];
    iv = mi4[j][js];
    double res1 = delf(ii, ij, ik, il, iis, it, iu, iv);
    delh = res1;
    if (mi5[i][is] != 0) {
      ii = mi5[i][is];
      ij = mi7[i][is];
      ik = mi1[j][js];
      il = mi3[j][js];
      iis = mi6[i][is];
      it = mi8[i][is];
      iu = mi2[j][js];
      iv = mi4[j][js];
      res1 = delf(ii, ij, ik, il, iis, it, iu, iv);
      delh += res1;
      if (mi5[j][js] == 0) {
        res = delh * fak;
        return res;
      }
      ii = mi5[i][is];
      ij = mi7[i][is];
      ik = mi5[j][js];
      il = mi7[j][js];
      iis = mi6[i][is];
      it = mi8[i][is];
      iu = mi6[j][js];
      iv = mi8[j][js];
      res1 = delf(ii, ij, ik, il, iis, it, iu, iv);
      delh += res1;
    }
    if (mi5[j][js] != 0) {
      ii = mi1[i][is];
      ij = mi3[i][is];
      ik = mi5[j][js];
      il = mi7[j][js];
      iis = mi2[i][is];
      it = mi4[i][is];
      iu = mi6[j][js];
      iv = mi8[j][js];
      res1 = delf(ii, ij, ik, il, iis, it, iu, iv);
      delh += res1;
    }
    res = delh * fak;
    return res;
  }

  private static double delf(int ii, int ij, int ik, int il, int is, int it, int iu, int iv) {
    return (delta(ii, ij) * delta(ik, il) * delta(is, it) * delta(iu, iv) + delta(ii, ik) * delta(ij, il) *
        delta(is, iu) * delta(it, iv) + delta(ii, il) * delta(ij, ik) * delta(is, iv) * delta(it, iu)) / 6. -
        (delta(ii, ij) * delta(ik, il) + delta(ii, ik) * delta(ij, il) + delta(ii, il) * delta(ij, ik)) *
            (delta(is, it) * delta(iu, iv) + delta(is, iu) * delta(it, iv) + delta(is, iv) * delta(it, iu)) / 30.;
  }

  private static double delta(int i, int j) {
    if (i != j)
      return 0.;
    return 1.0;
  }

  private static double[][][] suphill(double[][] ea, double[][] einva, int itend) {
    double[][] ca = new double[6][6];
    double[][] sa = new double[6][6];
    double[][] chill = new double[6][6];
    double[][] shill = new double[6][6];
    double[][][] rhill = new double[2][6][6];

    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        ca[i][j] = ea[i][j];
        sa[i][j] = einva[i][j];
      }
    }
    double[][] cainv = inv(ca);
    double[][] sainv = inv(sa);
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        shill[i][j] = (sa[i][j] + cainv[i][j]) / 2.0;
        chill[i][j] = (ca[i][j] + sainv[i][j]) / 2.0;
      }
    }
    double[][] sshill = inv(chill);
    double[][] cshill = inv(shill);
    for (int it = 0; it < itend; it++) {
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          shill[i][j] = (shill[i][j] + sshill[i][j]) / 2.0;
          chill[i][j] = (chill[i][j] + cshill[i][j]) / 2.0;
        }
      }
      sshill = inv(chill);
      cshill = inv(shill);
    }
    for (int i = 0; i < 6; i++) {
      for (int j = 0; j < 6; j++) {
        rhill[0][i][j] = chill[i][j];
        rhill[1][i][j] = cshill[i][j];
      }
    }
    return rhill;
  }

  private static double[][] inv(double[][] a) {
    double[][] ainv = new double[6][6];
    double[][] as = new double[6][6];
    double[] eigw = new double[6];
    double[] eigwin = new double[6];
    double[][] p = new double[6][6];
    double[][] ainvs = new double[6][6];

    for (int igreek = 0; igreek < 6; igreek++) {
      for (int jgreek = igreek; jgreek < 6; jgreek++) {
        double fak = 1.0;
        if (igreek > 2) fak *= w2;
        if (jgreek > 2) fak *= w2;
        double e = a[igreek][jgreek] * fak;
        as[igreek][jgreek] = e;
        as[jgreek][igreek] = e;
      }
    }
    if (eisrs1(as, eigw, p) == 1) // problem
	    return ainv;
    for (int i = 0; i < 6; i++)
      eigwin[i] = 1.0 / eigw[i];
    for (int i = 0; i < 6; i++) {
      for (int j = i; j < 6; j++) {
        double sum = 0.0;
        for (int k = 0; k < 6; k++)
          sum += p[i][k] * p[j][k] * eigwin[k];
        ainvs[i][j] = sum;
        ainvs[j][i] = sum;
      }
    }
    for (int i = 0; i < 6; i++) {
      for (int j = 1; j < 6; j++) {
        double fak = 1.0;
        if (i > 2) fak = w2;
        if (j > 2) fak *= w2;
        ainv[i][j] = ainvs[i][j] / fak;
      }
    }
    return ainv;
  }

  public static double[] convert(double a, double b, double c, int ieuler) {
//    at this point converts angles to Canova convention
    double[] phthoh = new double[3];
    switch (ieuler) {
      case 4:
        phthoh[0] = c;
        phthoh[1] = b;
        phthoh[2] = a;
        break;
      case 2:
        phthoh[0] = -c + 90.0;
        phthoh[1] = -b;
        phthoh[2] = -a - 90.0;
        break;
      case 3:
//      transforms Bunge's into Canova's
        phthoh[0] = -a;
        phthoh[1] = -b;
        phthoh[2] = -c;
        break;
      case 1:
//      transforms Kocks's into Canova's
        phthoh[0] = c + 90.0;
        phthoh[1] = b;
        phthoh[2] = -a + 90.0;
        break;
      default: {
      }
    }
    return phthoh;
  }

/*  private int odfstiff_() {
c   George Johnsons program to calculate stiffness from ideal orientations
        subroutine odfstiff
c
c       this program computes the stiffness
c       tensor for an ideal aggregate of trigonal crystals
c       from a single orientation and the additional
c      orientations necessary to have the aggregate orthotropic
    double[] c__  was [4][3][3][3][3]  = {0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    double[] cs   was [3][3][3][3]  = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    double[] f   was [3][4]  = {1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f, 1.f,
        1.f, 1.f, 1.f, 1.f};
    double atan;
    double cos;
    double sin;
    int i__;
    int j;
    int k;
    int l;
    double t;
    double p1;
    double p2;
    double ct;
    double ph;
    int ir;
    int nm;
    int nn;
    int np;
    int nq;
    double phi;
    double phi1;
    double phi2;
    cs[40] = cs[0];
    cs[20] = cs[70];
    cs[76] = cs[72];
    cs[54] = -cs[58];
    cs[16] = cs[58];
    cs[30] = cs[0] - cs[36] / 2.f;
    for (i__ = 1; i__ <= 3; ++ i__) {
      for (j = 1; j <= 3; ++ j) {
        for (k = 1; k <= 3; ++ k) {
          for (l = 1; l <= 3; ++ l) {
            if (cs[i__ + j + k + l * 3 * 3 * 3 - 40] != 0.0) {
              cs[j + i__ + k + l * 3 * 3 * 3 - 40] = cs[i__ + j + k + l * 3 * 3 * 3 - 40];
              cs[i__ + j + l + k * 3 * 3 * 3 - 40] = cs[i__ + j + k + l * 3 * 3 * 3 - 40];
              cs[j + i__ + l + k * 3 * 3 * 3 - 40] = cs[i__ + j + k + l * 3 * 3 * 3 - 40];
              cs[k + l + i__ + j * 3 * 3 * 3 - 40] = cs[i__ + j + k + l * 3 * 3 * 3 - 40];
              cs[l + k + i__ + j * 3 * 3 * 3 - 40] = cs[i__ + j + k + l * 3 * 3 * 3 - 40];
              cs[k + l + j + i__ * 3 * 3 * 3 - 40] = cs[i__ + j + k + l * 3 * 3 * 3 - 40];
              cs[l + k + j + i__ * 3 * 3 * 3 - 40] = cs[i__ + j + k + l * 3 * 3 * 3 - 40];
            }
          }
        }
      }
    }
    p1 = phi1 * pi / 180.f;
    ph = phi * pi / 180.f;
    p2 = phi2 * pi / 180.f;
    f[3] = -1.f;
    f[7] = -1.f;
    f[11] = -1.f;
    for (ir = 1; ir <= 4; ++ ir) {
      t[0] = Math.cos(p1) * Math.cos(p2) - Math.sin(p1) * Math.cos(ph) * Math.sin(p2) * f[ir * 3 - 3];
      t[3] = Math.sin(p1) * Math.cos(p2) + Math.cos(p1) * Math.cos(ph) * Math.sin(p2) * f[ir * 3 - 2];
      t[6] = Math.sin(ph) * Math.sin(p2) * f[ir * 3 - 1];
      t[1] = -Math.cos(p1) * Math.sin(p2) - Math.sin(p1) * Math.cos(ph) * Math.cos(p2) * f[ir * 3 - 3];
      t[4] = -Math.sin(p1) * Math.sin(p2) + Math.cos(p1) * Math.cos(ph) * Math.cos(p2) * f[ir * 3 - 2];
      t[7] = Math.sin(ph) * Math.cos(p2) * f[ir * 3 - 1];
      t[2] = Math.sin(p1) * Math.sin(ph) * f[ir * 3 - 3];
      t[5] = -Math.cos(p1) * Math.sin(ph) * f[ir * 3 - 2];
      t[8] = Math.cos(ph) * f[ir * 3 - 1];
      for (i__ = 1; i__ <= 3; ++ i__) {
        for (j = 1; j <= 3; ++ j) {
          for (k = 1; k <= 3; ++ k) {
            for (l = 1; l <= 3; ++ l) {
              for (nm = 1; nm <= 3; ++ nm) {
                for (nn = 1; nn <= 3; ++ nn) {
                  for (np = 1; np <= 3; ++ np) {
                    for (nq = 1; nq <= 3; ++ nq) {
                      c__[ir + i__ + j + k + l * 3 * 3 * 3
                      2 - 161 ]=
                      t[nm + i__ * 3 - 4] * t[nn + j * 3 - 4] * t[np + k * 3 - 4] * t[nq + l * 3 - 4] *
                               cs[nm + nn + np + nq * 3 * 3 * 3 - 40];
                    }
                  }
                }
              }
              ct[i__ + j + k + l * 3 * 3 * 3 - 40] = c__[ir + i__ + j + k + l * 3 * 3 * 3
              2 - 161 ]/ 4.f;
            }
          }
        }
      }
    }
    for (i__ = 1; i__ <= 3; ++ i__) {
      if (i__ != 3) {
        for (j = i__ + 1; j <= 3; ++ j) {
        }
      }
    }
    return 0;
  }

  private static int velocity_() {
c   George Johnsons program to calculate p velocity surface from tensor
        subroutine velocity
c   this would be a very useful program to have (and the same for second rank
c   tensors) but for all symmetries
c
c       this program computes the longitudinal wave speed for an
c       orthorhombic material at various angles of propagation.  Output
c      is normalized to 1000 maximum.
    double[] c__   was [3][3][3][3]  = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
    double[] t   was [3][3]  = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0};
    double atan;
    double sin;
    double cos;
    double sqrt;
    int i_nint;
    int i__;
    int j;
    int k;
    int l;
    double ch;
    int ic;
    int ii;
    int ij;
    int ik;
    int il;
    double ph;
    double pi;
    int ip;
    int iv;
    char qu;
    int ich;
    int iph;
    int one;
    double rho;
    double cst;
    double five;
    double velo;
    double temp;
    double vmax;
    int power_;
    double ninety;
    pi = 4.f * Math.atan(1.f);
    rho = 1.f;
    t[6] = 1.f;
    t[4] = 1.f;
    t[2] = -1.f;
    one = 1;
    five = 5.f;
    ninety = 90.f;
    for (i__ = 1; i__ <= 3; ++ i__) {
      for (j = 1; j <= 3; ++ j) {
        for (k = 1; k <= 3; ++ k) {
        }
      }
    }
    if ((unsigned
    char qu [
    0] == 'y' )|| (unsigned
    char qu [
    0] == 'Y' )) {
    for (i__ = 1; i__ <= 3; ++ i__) {
      for (j = 1; j <= 3; ++ j) {
        for (k = 1; k <= 3; ++ k) {
          for (l = 1; l <= 3; ++ l) {
            for (ii = 1; ii <= 3; ++ ii) {
              for (ij = 1; ij <= 3; ++ ij) {
                for (ik = 1; ik <= 3; ++ ik) {
                  for (il = 1; il <= 3; ++ il) {
                    cst[i__ + j + k + l * 3 * 3 * 3 - 40] = t[i__ + ii * 3 - 4] * t[j + ij * 3 - 4] *
                             t[k + ik * 3 - 4] * t[l + il * 3 - 4] * c__[ii + ij + ik + il * 3 * 3 * 3 - 40];
                  }
                }
              }
            }
          }
        }
      }
    }
    for (i__ = 1; i__ <= 3; ++ i__) {
      for (j = 1; j <= 3; ++ j) {
        for (k = 1; k <= 3; ++ k) {
          for (l = 1; l <= 3; ++ l) {
            c__[i__ + j + k + l * 3 * 3 * 3 - 40] = cst[i__ + j + k + l * 3 * 3 * 3 - 40];
          }
        }
      }
    }
  }
    vmax = 0.0;
    for (ich = 0; ich <= 90; ich = 5) {
      ic = ich / 5 + 1;
      ch =
      double ich * pi / 180.f;
      for (iph = 0; iph <= 355; iph = 5) {
        ip = iph / 5 + 1;
        ph =
        double iph * pi / 180.f;
        n[0] = Math.sin(ch) * Math.sin(ph);
        n[1] = Math.sin(ch) * Math.cos(ph);
        n[2] = Math.cos(ch);
        for (j = 1; j <= 3; ++ j) {
          for (k = 1; k <= 3; ++ k) {
            ck[j + k * 3 - 4] = 0.0;
            for (i__ = 1; i__ <= 3; ++ i__) {
              for (l = 1; l <= 3; ++ l) {
                ck[j + k * 3 - 4] = c__[i__ + j + k + l * 3 * 3 * 3 - 40] * n[i__ - 1] * n[l - 1];
              }
            }
          }
        }
        power_();
        velo[ic + ip * 19 - 20] = Math.sqrt(lam) / rho;
        if (velo[ic + ip * 19 - 20] > vmax) {
          vmax = velo[ic + ip * 19 - 20];
        }
      }
    }
    for (i__ = 1; i__ <= 19; ++ i__) {
      for (j = 1; j <= 72; ++ j) {
        temp = velo[i__ + j * 19 - 20] * 1e3f / vmax;
        iv[i__ + j * 19 - 20] = i_nint( & temp );
      }
    }
    return 0;
  }

  private static int power_() {
c     this subroutine calculates the largest eigenvalue of a 3 by 3
c     matrix by the power method.   The error criterion is currently
c     set at 0.001% difference between estimates of two steps.
    double r__1;
    int i__;
    int j;
    double v;
    double vm;
    double vn;
    double vp;
    int icount;
    icount = 0;
    r__1 = max(n[0], n[1]);
    vm = dmax(r__1, n[2]);
    if (vm <= 0.0) {
      r__1 = min(n[0], n[1]);
      vm = dmin(r__1, n[2]);
    }
    for (i__ = 1; i__ <= 3; ++ i__) {
      vp[i__ - 1] = n[i__ - 1];
    }
    {
      ++ icount;
      for (i__ = 1; i__ <= 3; ++ i__) {
        v[i__ - 1] = vp[i__ - 1] / vm;
      }
      for (i__ = 1; i__ <= 3; ++ i__) {
        vp[i__ - 1] = 0.0;
        for (j = 1; j <= 3; ++ j) {
          vp[i__ - 1] = ck[i__ + j * 3 - 4] * v[j - 1];
        }
      }
      r__1 = max(vp[0], vp[1]);
      vn = dmax(r__1, vp[2]);
      if (vn <= 0.0) {
        r__1 = min(vp[0], vp[1]);
        vn = dmin(r__1, vp[2]);
      }
      if (r__1 = vn - vm / vn
      dabs(r__1) < 1e-5f ) {
      lam = vn;
      return 0;
    }
      if (icount > 50) {
        icount = 0;
      } else vm = vn;
    }
    icount >= 0;
    return 0;
  }*/

  public static void print(String title, double[][] tens) {
    if (!debug) return;
    System.out.println(title);
    int dim1 = tens.length;
    int dim2 = tens[0].length;
    for (int i = 0; i < dim1; i++) {
      for (int j = 0; j < dim2; j++) {
        System.out.print(tens[i][j] + " ");
      }
      System.out.println();
    }
    System.out.println();
  }

  public static void print(String title, double[] tens) {
    if (!debug) return;
    System.out.println(title);
    int dim1 = tens.length;
    for (int i = 0; i < dim1; i++) {
      System.out.print((double) tens[i] + " ");
    }
    System.out.println();
    System.out.println();
  }

  public static void print(String title) {
    if (!debug) return;
    System.out.println(title);
  }

  public static void print() {
    if (!debug) return;
    System.out.println();
  }

  public static double[][] getTensorHomogenized(double[][] tensorToHomogenize, int model) {
    return getTensorHomogenized(tensorToHomogenize, null, model);
  }

  public static double[][] getTensorGEO(double[][] tensorToHomogenize) {
    return getTensorHomogenized(tensorToHomogenize, null, GEO_STIFFNESS);
  }

  public static double[][] getTensorGEO(double[][] tensorToHomogenize, String filename) {
    return getTensorHomogenized(tensorToHomogenize, filename, GEO_STIFFNESS);
  }

  public static double[][] getTensorHomogenized(double[][] tensorToHomogenize,
                                                String filename, int model) {
    double[][][] odf = null;
    if (filename != null && !filename.equalsIgnoreCase("")) {
      try {
        odf = Uwimvuo.ODFinputBeartex(filename);
      } catch (Exception e) {
        System.out.println("Error: file not found, use random odf instead");
        odf = null;
      }
    }
    TensorHomogenization work = new TensorHomogenization(tensorToHomogenize, odf);
    if (work.rank == 1)
      if (model > GEOMETRIC)
        model = GEOMETRIC;
    work.homogenizeModel(model);
    return work.getHomogenizedTensor(0);
  }

  int numberOfPoints = 0;
  double[] data = null;
  double[] theta = null;
  double[] phi = null;
  boolean[][] active = null;
  int activeModel = 0;

  public void solve(String filename, int model) {
    BufferedReader PFreader = Misc.getReader(filename);
    activeModel = model;

    if (PFreader != null) {
      try {

        String line = null;
        Vector pointV = new Vector(3, 3);

        line = PFreader.readLine(); // comments line
        line = PFreader.readLine();
        while (line != null) {
          StringTokenizer st = new StringTokenizer(line, " ,\t\r\n");
          double[] point = new double[3];
          for (int i = 0; i < 3; i++)
            if (st.hasMoreTokens())
              point[i] = (double) Double.parseDouble(st.nextToken());
          pointV.add(point);
          line = PFreader.readLine();
        }
        PFreader.close();
        int number = pointV.size();
        double[] dta = new double[number];
        double[] thetaD = new double[number];
        double[] phiD = new double[number];
        for (int i = 0; i < number; i++) {
          double[] point = (double[]) pointV.elementAt(i);
          dta[i] = point[2];
          thetaD[i] = point[0];
          phiD[i] = point[1];
        }
        double[][] tensorToFit = new double[tensor.length][tensor.length];
        for (int i = 0; i < tensor.length; i++)
          for (int j = 0; j < tensor.length; j++)
            tensorToFit[i][j] = tensor[i][j];

        solve(dta, thetaD, phiD, tensorToFit);

        result = new double[1][tensor.length][tensor.length];
        for (int i = 0; i < tensor.length; i++)
          for (int j = 0; j < tensor.length; j++)
            result[0][i][j] = tensorToFit[i][j];

      } catch (Exception io) {
        io.printStackTrace();
      }
    }
  }

  public void solve(double[] dta, double[] thetaD, double[] phiD, double[][] tensor) {
    numberOfPoints = dta.length;
    data = new double[numberOfPoints];
    System.arraycopy(dta, 0, data, 0, numberOfPoints);
    theta = new double[numberOfPoints];
    phi = new double[numberOfPoints];
    for (int i = 0; i < numberOfPoints; i++) {
      theta[i] = thetaD[i] * Math.PI / 180.0;
      phi[i] = phiD[i] * Math.PI / 180.0;
    }

    double[] wgt = new double[numberOfPoints];
    for (int i = 0; i < numberOfPoints; i++)
      wgt[i] = 1;
    int dim = tensor.length;
    int numberOfParm = 0;
    for (int i = 0; i < dim; i++)
      for (int j = i; j < dim; j++)
        if (tensor[i][j] != 0.0)
          numberOfParm++;
    double[] parm = new double[numberOfParm];
    active = new boolean[dim][dim];
    for (int i = 0; i < dim; i++)
      for (int j = 0; j < dim; j++)
        active[i][j] = false;
    int k = 0;
    for (int i = 0; i < dim; i++)
      for (int j = i; j < dim; j++)
        if (tensor[i][j] != 0.0) {
          active[i][j] = true;
          parm[k++] = (double) tensor[i][j];
        }

    int iterations = 20;
    MarqardLeastSquares solver = new MarqardLeastSquares(this, iterations);
    solver.outputEnabled = true;
    solver.setPrecision(0.000001);
    solver.setDerivateStep(0.001);
    double wssLimit = 0.0;
    solver.wssLimit = wssLimit;
    double[] fit = new double[numberOfPoints];
    double wss = solver.simpleSolve(dta, wgt, fit, parm, false, null);
    System.out.println("Final WSS = " + wss);
    refreshFit(fit, parm, null);
    System.out.println("Point #, theta, phi, value, fit");
    for (int i = 0; i < numberOfPoints; i++) {
      System.out.println(i + ", " + thetaD[i] + ", " + phiD[i] + ", " + dta[i] + ", " + fit[i]);
    }
    k = 0;
    for (int i = 0; i < 3; i++)
      for (int j = i; j < 3; j++)
        if (active[i][j])
          tensor[i][j] = tensor[j][i] = parm[k++];
    solver.releaseMemory();
    solver = null;
  }

  public void refreshFit(double[] fit, double[] parm, int[] controls) {
    if (tensor.length == 3) {
      double[][] tensor = new double[3][3];
      int k = 0;
      for (int i = 0; i < 3; i++)
        for (int j = i; j < 3; j++)
          if (active[i][j])
            tensor[i][j] = tensor[j][i] = parm[k++];
      for (int i = 0; i < 3; i++) {
        if (tensor[i][i] < 0.0) {
          fit[i] = (double) 1.0E10;
          return;
        }
      }
      setTensor(tensor);
      homogenizeModel(activeModel);
      tensor = getHomogenizedTensor(0);
      for (int i = 0; i < numberOfPoints; i++) {
        fit[i] = (double) MoreMath.getDirectionalProperty(tensor, MoreMath.directionCosines(theta[i], phi[i]));
      }
    } else {
      double[][] tensor = new double[6][6];
      int k = 0;
      for (int i = 0; i < 6; i++)
        for (int j = i; j < 6; j++)
          if (active[i][j])
            tensor[i][j] = tensor[j][i] = parm[k++];
      setTensor(tensor);
      homogenizeModel(tensor4IndexRead[activeModel]);
      tensor = getHomogenizedTensor(0);
      for (int i = 0; i < numberOfPoints; i++) {
        fit[i] = (double) MoreMath.getDirectionalProperty4Rank(tensor, MoreMath.directionCosines(theta[i], phi[i]));
      }
    }
  }

  public static void main(String[] args) {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    } catch (Exception exc1) {
      System.out.println("Error loading L&F: " + exc1);
    }
    JPopupMenu.setDefaultLightWeightPopupEnabled(false);

    TensorHomogenization work = new TensorHomogenization();
    TensorHomogenizationFrame frame = work.getFrame();
    frame.initmyFrame();
    frame.setVisible(true);
    while (frame.isVisible()) {
      try {
        Thread.sleep(1000);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
    System.exit(0);
  }

  public TensorHomogenizationFrame getFrame() {
    return new TensorHomogenizationFrame();
  }

  public class TensorHomogenizationFrame extends JFrame {

    TensorLabel[][] tensorL = new TensorLabel[6][6];
    JLabel[][] tensorR = new JLabel[6][6];
    JComboBox modelChoice = null;
    Tensor2Label[][] tensor2L = new Tensor2Label[3][3];
    JLabel[][] tensor2R = new JLabel[3][3];
    JComboBox modelChoice2 = null;
    public JLabel ODFlabel = new JLabel(
        "                                                                                           ");

    public void initmyFrame() {
      Container principal = getContentPane();
      principal.setLayout(new BorderLayout(9, 9));
      JPanel upperPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
      JButton odfButton = new JButton("Load ODF");
      upperPanel.add(odfButton);
      odfButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          browseAndLoadODF();
        }
      });
      upperPanel.add(ODFlabel);
      principal.add(upperPanel, BorderLayout.NORTH);

      JTabbedPane tabpanel = new JTabbedPane();
      principal.add(tabpanel, BorderLayout.CENTER);

      JPanel tensor4Panel = new JPanel(new BorderLayout(6, 6));

      JPanel centerPanel = new JPanel(new GridLayout(7, 7));
      tensor4Panel.add(centerPanel, BorderLayout.CENTER);
      for (int i = 0; i < 7; i++) {
        for (int j = 0; j < 7; j++) {
          if (i == 0 || j == 0) {
            if (i == 0 && j == 0)
              centerPanel.add(new Label("Stiffness"));
            else if (i == 0)
              centerPanel.add(new Label(Integer.toString(j)));
            else
              centerPanel.add(new Label(Integer.toString(i)));
          } else {
            tensorL[i - 1][j - 1] = new TensorLabel(defaultTensor[i - 1][j - 1], i - 1, j - 1);
            centerPanel.add(tensorL[i - 1][j - 1]);
          }
        }
      }
      JPanel lowerPanel = new JPanel(new BorderLayout(3, 3));
      tensor4Panel.add(lowerPanel, BorderLayout.SOUTH);
      JPanel lowerPanel1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
      lowerPanel.add(lowerPanel1, BorderLayout.NORTH);
      JButton computeButton = new JButton("Homogenize all (see console)");
      lowerPanel1.add(computeButton);
      computeButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          retrieveAndCompute();
        }
      });
      JButton computeGEOButton = new JButton("Homogenize");
      lowerPanel1.add(computeGEOButton);
      computeGEOButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          retrieveAndComputeModel();
        }
      });
      modelChoice = new JComboBox();
      for (int i = 0; i < tensor4IndexRead.length; i++) {
        modelChoice.addItem(tensor4Label[tensor4IndexRead[i]]);
      }
      lowerPanel1.add(modelChoice);
      modelChoice.setSelectedIndex(tensor4IndexRead.length - 1);

      JButton fitTensor = new JButton("Fit tensor");
      lowerPanel1.add(fitTensor);
      fitTensor.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          fitTensor();
        }
      });


      JPanel centerPanel2 = new JPanel(new GridLayout(7, 7));
      lowerPanel.add(centerPanel2, BorderLayout.CENTER);
      for (int i = 0; i < 7; i++) {
        for (int j = 0; j < 7; j++) {
          if (i == 0 || j == 0) {
            if (i == 0 && j == 0)
              centerPanel2.add(new JLabel("Homogenized"));
            else if (i == 0)
              centerPanel2.add(new JLabel(Integer.toString(j)));
            else
              centerPanel2.add(new JLabel(Integer.toString(i)));
          } else {
            tensorR[i - 1][j - 1] = new JLabel("0.0");
            centerPanel2.add(tensorR[i - 1][j - 1]);
          }
        }
      }

      JPanel tensor2Panel = new JPanel(new BorderLayout(6, 6));
      tabpanel.addTab("Tensor rank 2", tensor2Panel);
      tabpanel.addTab("Tensor rank 4", tensor4Panel);

      JPanel center2Panel = new JPanel(new GridLayout(4, 4));
      tensor2Panel.add(center2Panel, BorderLayout.CENTER);
      for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
          if (i == 0 || j == 0) {
            if (i == 0 && j == 0)
              center2Panel.add(new JLabel("Tensor"));
            else if (i == 0)
              center2Panel.add(new JLabel(Integer.toString(j)));
            else
              center2Panel.add(new JLabel(Integer.toString(i)));
          } else {
            tensor2L[i - 1][j - 1] = new Tensor2Label(defaultTensor2[i - 1][j - 1], i - 1, j - 1);
            center2Panel.add(tensor2L[i - 1][j - 1]);
          }
        }
      }
      JPanel lower2Panel = new JPanel(new BorderLayout(3, 3));
      tensor2Panel.add(lower2Panel, BorderLayout.SOUTH);
      JPanel lower2Panel1 = new JPanel(new FlowLayout(FlowLayout.CENTER, 3, 3));
      lower2Panel.add(lower2Panel1, BorderLayout.NORTH);
      Button computeButton2 = new Button("Homogenize all (see console)");
      lower2Panel1.add(computeButton2);
      computeButton2.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          retrieveAndCompute2();
        }
      });
      JButton computeGEOButton2 = new JButton("Homogenize");
      lower2Panel1.add(computeGEOButton2);
      computeGEOButton2.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          retrieveAndComputeModel2();
        }
      });
      modelChoice2 = new JComboBox();
      for (int i = 0; i < tensor2Label.length; i++) {
        modelChoice2.addItem(tensor2Label[i]);
      }
      lower2Panel1.add(modelChoice2);
      modelChoice2.setSelectedIndex(tensor2Label.length - 1);

      JButton fitTensor2 = new JButton("Fit tensor");
      lower2Panel1.add(fitTensor2);
      fitTensor2.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          fitTensor2();
        }
      });

      JPanel center2Panel2 = new JPanel(new GridLayout(4, 4));
      lower2Panel.add(center2Panel2, BorderLayout.CENTER);
      for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
          if (i == 0 || j == 0) {
            if (i == 0 && j == 0)
              center2Panel2.add(new JLabel("Homogenized"));
            else if (i == 0)
              center2Panel2.add(new JLabel(Integer.toString(j)));
            else
              center2Panel2.add(new JLabel(Integer.toString(i)));
          } else {
            tensor2R[i - 1][j - 1] = new JLabel("0.0");
            center2Panel2.add(tensor2R[i - 1][j - 1]);
          }
        }
      }

      setDefaultCloseOperation(DISPOSE_ON_CLOSE);
      pack();
    }

    public void browseAndLoadODF() {
      String filename = Utility.openFileDialog(this, "Load ODF in Beartex format", FileDialog.LOAD,
          null, null, null);
      if (filename != null) {
        double[][][] odf = Uwimvuo.ODFinputBeartex(filename);
        setODF(odf);
        ODFlabel.setText(filename);
        pack();
      }
    }

    public int retrieve() {
      double[][] tensor = new double[6][6];
      for (int i = 0; i < 6; i++) {
        for (int j = 0; j < 6; j++) {
          tensor[i][j] = Double.parseDouble(tensorL[i][j].getText());
        }
      }
      setTensor(tensor);
      return modelChoice.getSelectedIndex();
    }

    public void retrieveAndCompute() {
      retrieve();
      homogenize();
      showResults();
    }

    public void retrieveAndComputeModel() {
      int modelIndex = retrieve();
      homogenizeModel(tensor4IndexRead[modelIndex]);
      showModelResults(tensor4IndexRead[modelIndex]);
    }

    public void showResults() {
      String results = getResults();
      System.out.println(results);
      double[][] ten = getHomogenizedTensor(tensor4IndexRead[modelChoice.getSelectedIndex()]);
      showResultTensor(ten);
    }

	  public int retrieve2() {
      double[][] tensor = new double[3][3];
      for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
          tensor[i][j] = Double.parseDouble(tensor2L[i][j].getText());
        }
      }
      setTensor(tensor);
      return modelChoice2.getSelectedIndex();
    }

    public void retrieveAndCompute2() {
      retrieve2();
      homogenize();
      showResults2();
    }

    public void retrieveAndComputeModel2() {
      int modelIndex = retrieve2();
      homogenizeModel(modelIndex);
      showModelResults(modelIndex);
    }

    public void fitTensor() {
      int modelIndex = retrieve();
      loadDataAndSolve(modelIndex);
      showResultTensor(getHomogenizedTensor(0));
    }

    public void fitTensor2() {
      int modelIndex = retrieve2();
      loadDataAndSolve(modelIndex);
      showResultTensor(getHomogenizedTensor(0));
    }

    public void loadDataAndSolve(int modelIndex) {
      String filename = Utility.openFileDialog(this, "Load property data to fit", FileDialog.LOAD,
		      null, null, null);
      if (filename != null)
        solve(filename, modelIndex);
    }

    public void showResults2() {
      String results = getResults();
      System.out.println(results);
      double[][] ten = getHomogenizedTensor(modelChoice2.getSelectedIndex());
      showResultTensor(ten);
    }

    public void showGEOResults() {
      String results = getGEOResults();
      System.out.println(results);
      double[][] ten = getHomogenizedTensor(0);
      showResultTensor(ten);
    }

    public void showResultTensor(double[][] ten) {
      if (ten.length == 6) {
        double max = 0.0;
        for (int i = 0; i < 6; i++)
          for (int j = 0; j < 6; j++)
            if (ten[i][j] > max)
              max = ten[i][j];
        for (int i = 0; i < 6; i++) {
          for (int j = 0; j < 6; j++) {
            float real = (float) ten[i][j];
            if (real / max < 1e-9) real = 0.0f;
            tensorR[i][j].setText(Float.toString(real));
          }
        }
      } else {
        double max = 0.0;
        for (int i = 0; i < 3; i++)
          for (int j = 0; j < 3; j++)
            if (ten[i][j] > max)
              max = ten[i][j];
        for (int i = 0; i < 3; i++) {
          for (int j = 0; j < 3; j++) {
            float real = (float) ten[i][j];
            if (real / max < 1e-9) real = 0.0f;
            tensor2R[i][j].setText(Float.toString(real));
          }
        }
      }
    }

    public void showModelResults(int model) {
      String results = getModelResults(model);
      System.out.println(results);
      double[][] ten = getHomogenizedTensor(0);
      showResultTensor(ten);
    }

	  public void updateTensor(double value, int xIndex, int yIndex) {
//s      tensorL[yIndex][xIndex].setText(Float.toString((double) value));
      tensorL[xIndex][yIndex].setText(Float.toString((float) value));
    }

    public void updateTensor2(double value, int xIndex, int yIndex) {
//s      tensor2L[yIndex][xIndex].setText(Float.toString((double) value));
      tensor2L[xIndex][yIndex].setText(Float.toString((float) value));
    }

    public class TensorLabel extends TextField {

      int xIndex = 0;
      int yIndex = 0;

      public TensorLabel(double value, int i, int j) {
        super(12);
        setText(Float.toString((float) value));
        setCoord(i, j);
        addFocusListener(new FocusListener() {

          /**
           * Invoked when a component gains the keyboard focus.
           */
          public void focusGained(FocusEvent e) {
          }

          /**
           * Invoked when a component loses the keyboard focus.
           */
          public void focusLost(FocusEvent e) {
            //To change body of implemented methods use File | Settings | File Templates.
            checkValueAndUpdateSymmetry();
          }
        });
      }

      public void setCoord(int x, int y) {
        xIndex = x;
        yIndex = y;
      }

      public void checkValueAndUpdateSymmetry() {
        double value = Double.parseDouble(getText());
        updateTensor(value, xIndex, yIndex);
      }
    }

    public class Tensor2Label extends TextField {

      int xIndex = 0;
      int yIndex = 0;

      public Tensor2Label(double value, int i, int j) {
        super(12);
        setText(Float.toString((float) value));
        setCoord(i, j);
        addFocusListener(new FocusListener() {

          /**
           * Invoked when a component gains the keyboard focus.
           */
          public void focusGained(FocusEvent e) {
          }

          /**
           * Invoked when a component loses the keyboard focus.
           */
          public void focusLost(FocusEvent e) {
            //To change body of implemented methods use File | Settings | File Templates.
            checkValueAndUpdateSymmetry();
          }
        });
      }

      public void setCoord(int x, int y) {
        xIndex = x;
        yIndex = y;
      }

      public void checkValueAndUpdateSymmetry() {
        double value = Double.parseDouble(getText());
        updateTensor2(value, xIndex, yIndex);
      }
    }

  }

}

