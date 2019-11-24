/*
 * @(#)MoreMath.java created 19/10/1998 Pergine
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

package it.unitn.ing.rista.util;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.EigenDecomposition;

import java.lang.*;
import java.util.Vector;

/**
 * The MoreMath is a class providing static methods for
 * simple mathematics computation.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.9 $, $Date: 2006/12/04 14:30:15 $
 * @since JDK1.1
 */


public class MoreMath {

  public static final double InchesTomm = 25.4;
  public static final double[] i = {0.0, 1.0};
  public static double[] factorial = null;

  private MoreMath() {
  }

  public static final boolean odd(int value) {
/*    int i = value / 2;
    if (i * 2 == value)
      return false;
    else
      return true;*/
	  return (Math.abs(value) & 1) != 0;
  }

  public static final int is3Neven(int value) {
	  int residual = value % 3;
	  if (residual < 0) {
		  residual += 3;
	  } else if (residual == 0) {
		  return 0;
	  }
	  if (residual == 1) {
		  return 1;
	  }
	  return -1;
  }

  public static final boolean isMultipleOf(int value, int factor) {
    int i = value / factor;
    if (i * factor == value)
      return true;
    return false;
  }

  public static final double pow(int a, int b) {
    double result = 1.0;

    if (b > 0)
      for (int i = 0; i < b; i++)
        result *= a;
    else if (b < 0)
      for (int i = 0; i > b; i--)
        result /= a;

    return result;
  }

  public static final int powint(int a, int b) {
    int resulti = 1;

    if (b > 0)
      for (int i = 0; i < b; i++)
        resulti *= a;
    else if (b < 0)
      for (int i = 0; i > b; i--)
        resulti /= a;

    return resulti;
  }

  public static final int pow_ii(int b) {
    return powint(-1, b);
  }

  public static final double pow(double a, int b) {
    double result = 1.0;

    if (b > 0)
      for (int i = 0; i < b; i++)
        result *= a;
    else if (b < 0)
      for (int i = 0; i > b; i--)
        result /= a;

    return result;
  }

  public static final double log(double arg, double base) {
    return Math.log(arg) / Math.log(base);
  }

  public static final double log10(double arg) {
    return log(arg, 10.0);
  }

  public static final void initFactorial(int maxValue) {
    if (factorial == null || factorial.length < maxValue) {
      factorial = new double[maxValue];
      for (int i = 0; i < maxValue; i++)
        factorial[i] = fact(i);
    }
  }

  public static final double factorial(int a) {
    if (a < 0)
      return -factorial[-a];
    return factorial[a];
  }

  public static final double fact(int a) {
    double result = 1.0;
    for (int i = 2; i <= a; i++)
      result *= i;
    return result;
  }

  public static final double factorial(int a, int b) {
    return factorial(a) / factorial(b);
  }

/*	public static final double fact(int a, int b) {
		double result = 1.0;
		for (int i = b+1; i <= a; i++)
			result *= i;
		return result;
	}*/

  public static final double binomial(int n, int k) {
    return factorial(n, n - k) / factorial(k);
  }

  public static final double cosd(double angle) {
    return Math.cos(angle * Constants.DEGTOPI);
  }

  public static final double sind(double angle) {
    return Math.sin(angle * Constants.DEGTOPI);
  }

  public static final double tand(double angle) {
    return Math.tan(angle * Constants.DEGTOPI);
  }

//	public static final double cotd(double angle) {
//		return Math.cot(angle * Constants.DEGTOPI);
//	}

  public static final double atand(double tangent) {
    return Math.atan(tangent) * Constants.PITODEG;
  }

  public static final double acosd(double tangent) {
    return Math.acos(tangent) * Constants.PITODEG;
  }

  public static final double asind(double tangent) {
    return Math.asin(tangent) * Constants.PITODEG;
  }

  public static final int sign(int value) {
    if (value >= 0)
      return 1;
    else
      return -1;
  }

  public static final int sign(double value) {
    if (value >= 0.0)
      return 1;
    else
      return -1;
  }

  /* fortran function sign(x,y) */
  public static final double sign(double x, double y) {
    return Math.abs(x) * sign(y);
  }

  public static final double sqrt(double value) {
    return Math.sqrt(Math.abs(value));
  }

  public static final double sqrt_or_zero(double value) {
    if (value < 0)
      return 0.0;
    return Math.sqrt(value);
  }

	public static final double getGaussianNoise(double intensity) {
		double u, v;
		if (intensity < 0)
			intensity = -intensity;
		while ((u = Math.random()) == 0.0);
		v = Math.random();
		return sqrt(-2.0 * Math.log(u) * intensity) * Math.cos(2.0 * Constants.PI * v);
	}

	public static final double getPoissonNoise(double intensity) {
		double L, p;
		int k;
		if (intensity == 0)
			return 0;
		if (intensity < 0)
			intensity = -intensity;
		L = Math.exp(-Math.sqrt(intensity));
		k = 0;
		p = 1;
		do {
			k++;
			p *= Math.random();
		} while (p >= L);
		return -intensity + k - 1;
	}


	public static final double getIntensityRandomError(double intensity) {
    double error = Math.random();
    double hwhm = 3.0 * Math.sqrt(intensity);
    error = ec.util.RandomChoice.pickFromDistribution(distribution,
        error, 0) * hwhm / distribution.length;
    if (hwhm / distribution.length > 1.5)
      error += (Math.random() - 0.5) * hwhm / distribution.length;
    if (Math.random() > .5)
      error = -error;
    return error;
  }

  static double[] distribution = null;

  public static void prepareGaussianDistribution(int number) {
    int total = 3 * number;
    distribution = new double[total];
    for (int i = 0; i < total; i++) {
      distribution[i] = Gaussian.getY(number, i);
    }
    ec.util.RandomChoice.organizeDistribution(distribution);
  }

  public static void resetGaussianDistribution() {
    distribution = null;
  }

  public static double phase(double a, double b) {
    double phase = 0.0;
    if (Math.abs(a) < 1E-18) {
      a = 0.0;
      if (b > 1E-9)
        phase = Constants.PI * 0.5;
      else if (b < -1E-9)
        phase = Constants.PI * 1.5;
    } else {
      if (Math.abs(b) < 1E-18)
        b = 0.0;
      phase = Math.atan(b / a);
      if (a < 0) {
        if (b <= 0)
          phase += Constants.PI;
      } else {
        if (b < 0)
          phase += Constants.PI;
      }
    }
    return phase;
  }

  public static final int deltaDirac(int i1, int i2) {
    if (i1 == i2)
      return 1;
    else
      return 0;
  }

  public static final double[] complexSqrt(final double re, final double im) {
    double mod, arg;
    final double reAbs = Math.abs(re);
    final double imAbs = Math.abs(im);

    if (reAbs <= 0.0 && imAbs <= 0.0) {
      mod = 0.0;
    } else {
      if (reAbs < imAbs)
        mod = imAbs * Math.sqrt(1.0 + (re / im) * (re / im));
      else
        mod = reAbs * Math.sqrt(1.0 + (im / re) * (im / re));
    }
    if (re > 0.0) {
      arg = Math.atan(im / re);
    } else if (re < 0.0) {
      if (im >= -0.0)
        arg = Math.atan(im / re) + Math.PI;
      else
        arg = Math.atan(im / re) - Math.PI;
    } else {
      if (im > 0.0)
        arg = Math.PI / 2.0;
      else if (im < 0.0)
        arg = -Math.PI / 2.0;
      else
        arg = 0.0;
    }

    mod = Math.pow(mod, 0.5);
    arg *= 0.5;

    double[] complex = {mod * Math.cos(arg), mod * Math.sin(arg)};
    return complex;
  }

  public static final double[] complexSqrt(final double[] complex) {
    return complexSqrt(complex[0], complex[1]);
  }

  public static final double[] complexDivide(final double re, final double im, final double re1, final double im1) {
    double denominator, real, imag, a;
    if (Math.abs(re1) < Math.abs(im1)) {
      a = re1 / im1;
      denominator = re1 * a + im1;
      real = re * a + im;
      imag = im * a - re;
    } else {
      a = im1 / re1;
      denominator = re1 + im1 * a;
      real = re + im * a;
      imag = im - re * a;
    }
    double[] complex = {real / denominator, imag / denominator};
    return complex;
  }

  public static final double[] complexDivide(final double[] complex, final double[] complex1) {
    return complexDivide(complex[0], complex[1], complex1[0], complex1[1]);
  }

  public static final double[] complexMultiply(final double re, final double im, final double re1, final double im1) {
    double[] complex = {re * re1 - im * im1, im * re1 + re * im1};
    return complex;
  }

  public static final double[] complexMultiply(final double[] complex, final double[] complex1) {
    return complexMultiply(complex[0], complex[1], complex1[0], complex1[1]);
  }

  public static final double[] multiply(final double[] complex, final double re) {
    double[] complex1 = {complex[0] * re, complex[1] * re};
    return complex1;
  }

  public static final double[] complexExp(final double re, final double im) {
	  double mexpre = Math.exp(re);  // 0;
//	  if (re > -Double.MIN_EXPONENT/2 && re < Double.MAX_EXPONENT/2)
//		  mexpre = Math.exp(re);
//	  else {
//		  if (re > 0)
//			  mexpre = Double.MAX_VALUE/2;
//	  }
    double[] complex = {mexpre * Math.cos(im), mexpre * Math.sin(im)};
    return complex;
  }

  public static final double[] complexExp(final double[] z) {
    return complexExp(z[0], z[1]);
  }

  public static final double[] complexConjugate(final double re, final double im) {
    double[] complex = {re, -im};
    return complex;
  }

  public static final double[] complexConjugate(final double[] z) {
    return complexConjugate(z[0], z[1]);
  }

  public static final double[] complexAdd(final double re, final double im, final double re1, final double im1) {
    double[] complex = {re + re1, im + im1};
    return complex;
  }

  public static final double[] complexAdd(final double[] z, final double[] z1) {
    return complexAdd(z[0], z[1], z1[0], z1[1]);
  }

	public static final double complexAbs(final double re, final double im) {
		return Math.sqrt(re * re + im * im);
	}

	public static final double complexAbs(final double[] z) {
		return complexAbs(z[0], z[1]);
	}

	public static final boolean isInvalidNumber(double number) {
    // there is probably a bug in jdk 1.1.8
    if (Double.isNaN(number))
      return true;
    if (Double.isInfinite(number))
      return true;
    else
      return false;
  }

  public static double[] getPlaneBy3Points(double[] A, double[] B, double[] C) throws Exception {
    double[] n = getNormalVector(A, B, C);
    if (n[2] == 0.0) {
      n = getNormalVector(B, C, A);
      if (n[2] == 0.0) {
        n = getNormalVector(C, A, B);
        if (n[2] == 0.0)
          throw new Exception("Degenerated plane");
        return getPlaneByVectorAndPoint(C, n);
      }
      return getPlaneByVectorAndPoint(B, n);
    }
    return getPlaneByVectorAndPoint(A, n);
  }

  public static double[] getNormalVector(double[] A, double[] B, double[] C) {
    double v1 = B[0] - A[0];
    double v2 = B[1] - A[1];
    double v3 = B[2] - A[2];
    double w1 = C[0] - A[0];
    double w2 = C[1] - A[1];
    double w3 = C[2] - A[2];
    double[] nVector = new double[3];
    nVector[0] = v2 * w3 - v3 * w2;
    nVector[1] = v3 * w1 - v1 * w3;
    nVector[2] = v1 * w2 - v2 * w1;
    return nVector;
  }

  public static double[] getPlaneByVectorAndPoint(double[] x, double[] n) {
    double[] nPlane = new double[3];
    nPlane[0] = -n[0] / n[2];
    nPlane[1] = -n[1] / n[2];
    nPlane[2] = (n[0] * x[0] + n[1] * x[1] + n[2] * x[2]) / n[2];
    return nPlane;
  }

  public static double getDistanceBetweenPoints(double[] A, double[] B) {
    int dim = A.length;
    double distance = 0.0;
    for (int i = 0; i < dim; i++) {
      double diff = B[i] - A[i];
      distance += diff * diff;
    }
    return Math.sqrt(distance);
  }

  public static double getDistanceBetweenPoints(double Ax, double Ay, double Bx, double By) {
    double diff = Bx - Ax;
    double distance = diff * diff;
    diff = By - Ay;
    distance += diff * diff;

    return Math.sqrt(distance);
  }

  public static double getLinearInterpolation(double x, double x1, double x2,
                                              double y1, double y2) {
    double a = (y2 - y1) / (x2 - x1);
    if (Double.isNaN(a) || Double.isInfinite(a))
      return (y1 + y2) / 2;
    double b = y1 - a * x1;
    return a * x + b;
  }

  public static double getPolinomialValue(double x, double[] pars) {
    double value = 0.0;
    for (int i = 0; i < pars.length; i++)
      value += pars[i] * MoreMath.pow(x, i);
    return value;
  }

  public static double[] getPolinomialInterpolation(int polinomialDegree, double[] xdata,
                                                    double[] ydata) {
    int parNumber = polinomialDegree + 1;
    double[] pars = new double[parNumber];
    double[][] ds = new double[parNumber][parNumber];
    double[] ds_0 = new double[parNumber];
    int numberOfPoints = xdata.length;

    for (int i = 0; i < parNumber; i++) {
      for (int k = 0; k < parNumber; k++) {
        ds[i][k] = 0.0;
      }
      ds_0[i] = 0.0;
    }
    for (int n = 0; n < numberOfPoints; n++) {
      double x = xdata[n];
      double xn = 1.0;
      for (int j = 0; j < parNumber; j++) {
        double xnk = 1.0;
        for (int k = 0; k < parNumber; k++) {
          ds[j][k] += xn * xnk;
          xnk *= x;
        }
        ds_0[j] += xn * ydata[n];
        xn *= x;
      }
    }

    double GAUSS_DETERM = AX_Bgauss(ds, ds_0, pars);

    return pars;
  }

/*
            The following example is a Gauss elimination algorithm. The original
              set of routines was produced directly from "Advanced Engineering
              Mathematics" (Erwin Kreysig, 6th ed.). Optimization resulted
              in approximately a 1.5x improvement in speed on a Pentium II. Only
              the elimination and pivot routines are shown here as they are the
              key ones from a performance standpoint. You can download
              the entire unit for the complete example.
            General note on solving linear systems of equations: There are
              several methods to solve these problems and Gauss elimination with
              pivoting is only one of them. Another common approach is LU decomposition
              which, in theory, takes fewer arithmetic operations. However, it
              also jumps around the matrix a lot more. Consequently, on the problems
              that I have tested, Gauss elimination beats LU decomposition, because
              of its reduced cache and memory access requirements.
*/

//            Original pivot and elimination routines:

  public static void GaussPivot(double[][] Mx, double[] S, int j, int N) {

    int pivot = j;
    double big = Math.abs(Mx[j][j] * S[j]);
    for (int i = j + 1; i < N; i++)
      if (Math.abs(Mx[i][j] * S[i]) > big) {
        big = Math.abs(Mx[i][j] * S[i]);
        pivot = i;
      }
    for (int i = 0; i < N - 1; i++) {
      double dummy = Mx[j][i];
      Mx[j][i] = Mx[pivot][i];
      Mx[pivot][i] = dummy;
    }
  }


  public static void Eliminate(double[][] Mx, double[] S, int N) {
    N--;
    for (int k = 0; k < N - 1; k++) {
      GaussPivot(Mx, S, k, N);
      for (int j = k + 1; j < N; j++) {
        double scale = Mx[j][k] / Mx[k][k];
        for (int i = k + 1; i < N + 1; i++)    // Iterate
          Mx[j][i] = Mx[j][i] - scale * Mx[k][i];
      }
    }
  }

//            Optimized routines:

/*
function FindMax(X,S:PFloatArray; N:integer):PFloatArray;
// return pivot Row
var
  i:Integer;
  big: single;
begin
  big := abs(X[0]*S[0]);
  result := X;
  i:=1;
  while i<N do  // converted for to while
  begin
    inc(X);  // step one row
    if (abs(X[0]*S[i]) > big) then
    begin
      big := abs(X[0]*S[i]);
      result := X;
    end;
    inc(i);
  end;
end;

procedure GaussPivot1(X:PFloatArray; S:PFloatArray; j,N:integer);
// Restructured Param types
var
  i: integer;
  big,dummy: single;
  Pivot:PFloatArray;
begin
  X:=@X[j];  // shift to First element to change
  Pivot:=FindMax(X,@S[j],N-j);  // pulled out loop
  i:=0;
  N:=N-j;
  // note that physically copying rows is faster than indexing
  while i<=N do // converted for to while
  begin
    dummy:=X[i];
    X[i]:=Pivot[i];
    Pivot[i]:=dummy;
    inc(i);
  end;
end;   { procedure Pivot }

procedure Eliminate1(var Mx:TFloatMatrix; const S:TFloatArray; N:integer);
var
  i,j,k:Integer;
  scale,divisor:Double;
  Mxj,Mxk:PFloatArray;
begin
  // generally, all matrix references have been reduced to array refs.
  // by making a Pointer to the rows.
  k:=0;
  while k<N-1 do
  begin
    GaussPivot1(@Mx[k],@S,k,N);
    Mxk:=@Mx[k];  // Get row k
    j:=k+1;
    Mxj:=@Mx[j];  // Get row j
    Divisor:=1/Mxk[k];     // move division out of loop
    while j<N do  // converted for to while
    begin
      Scale:=Mxj[k]*Divisor;
      i:=k+1;
      while i<=N do  // converted for to while
      begin
        Mxj[i]:=Mxj[i]-Scale*Mxk[i];
        inc(i);
      end;
      inc(j);
      inc(Mxj);  // step forward one row
    end;
    inc(k);
  end;
end;
*/

  public static double AX_Bgauss(double[][] ds, double[] ds_0_, double[] ds_1_) {
    double GAUSS_DETERM = 0.0;
    double d = 1.0;
    int i = ds_0_.length;
    double[][] ds_2_ = new double[i][1];
    for (int i_3_ = 0; i_3_ < i; i_3_++)
      ds_2_[i_3_][0] = ds_0_[i_3_];
    int nswaps = trianmat(i, ds, ds_2_, 1);
    if (nswaps >= 0) {
      double d_4_ = d;
      for (int i_5_ = 0; i_5_ < i; i_5_++) {
        ds_1_[i_5_] = ds_2_[i_5_][0];
        d_4_ *= ds[i_5_][i_5_];
      }
      if (nswaps % 2 != 0)
        d_4_ = -d_4_;
      GAUSS_DETERM = d_4_;
    }
    return GAUSS_DETERM;
  }

  public static int Ilog10(double d) {
    double d_6_ = Math.abs(d);
    return (int) log10(d_6_);
  }

  public static void Matprod1(double[][] ds, double[][] ds_7_, int i) {
    double[] V1 = new double[20];
    double d = 0.0;
    for (int i_8_ = 0; i_8_ < i; i_8_++) {
      for (int i_9_ = 0; i_9_ < i; i_9_++) {
        V1[i_9_] = d;
        for (int i_10_ = 0; i_10_ < i; i_10_++)
          V1[i_9_] += ds[i_8_][i_10_] * ds_7_[i_10_][i_9_];
      }
      for (int i_11_ = 0; i_11_ < i; i_11_++)
        ds[i_8_][i_11_] = V1[i_11_];
    }
  }

  public static double invgauss(int i, double[][] ds, double[][] ds_12_) {
    double GAUSS_DETERM = 0.0;
    double d = 0.0;
    double d_13_ = 1.0;
    for (int i_14_ = 0; i_14_ < i; i_14_++) {
      for (int i_15_ = 0; i_15_ <= i_14_; i_15_++) {
        ds_12_[i_14_][i_15_] = d;
        ds_12_[i_15_][i_14_] = ds_12_[i_14_][i_15_];
        if (i_15_ == i_14_)
          ds_12_[i_15_][i_14_] = d_13_;
      }
    }
    int nswaps = trianmat(i, ds, ds_12_, i);
    if (nswaps >= 0) {
      double d_16_ = d_13_;
      for (int i_17_ = 0; i_17_ < i; i_17_++)
        d_16_ *= ds[i_17_][i_17_];
      if (nswaps % 2 != 0)
        d_16_ = -d_16_;
      GAUSS_DETERM = d_16_;
    }
    return GAUSS_DETERM;
  }

  public static double mod(double d, double d_18_) {
    return Math.IEEEremainder(d, d_18_);
  }

  public static void sortAscending(int i, double[] ds) {
    for (int i_21_ = 1; i_21_ < i; i_21_++) {
      for (int i_22_ = 0; i_22_ < i - 1; i_22_++) {
        if (ds[i_22_] > ds[i_21_]) {
          double d = ds[i_21_];
          ds[i_21_] = ds[i_22_];
          ds[i_22_] = d;
        }
      }
    }
  }

  public static void sortDescending(int i, double[] ds) {
    for (int i_23_ = 1; i_23_ < i; i_23_++) {
      for (int i_24_ = 0; i_24_ < i - 1; i_24_++) {
        if (ds[i_24_] < ds[i_23_]) {
          double d = ds[i_23_];
          ds[i_23_] = ds[i_24_];
          ds[i_24_] = d;
        }
      }
    }
  }

  public static void swaplines(double[][] ds, int i, int i_25_, int i_26_) {
    for (int i_27_ = 0; i_27_ < i_26_; i_27_++) {
      double d = ds[i][i_27_];
      ds[i][i_27_] = ds[i_25_][i_27_];
      ds[i_25_][i_27_] = d;
    }
  }

  public static void transpose(double[][] ds, int i) {
    for (int i_28_ = 0; i_28_ < i; i_28_++) {
      for (int i_29_ = 0; i_29_ <= i_28_; i_29_++) {
        double d = ds[i_28_][i_29_];
        ds[i_28_][i_29_] = ds[i_29_][i_28_];
        ds[i_29_][i_28_] = d;
      }
    }
  }

  public static int trianmat(int i, double[][] ds, double[][] ds_30_,
                             int i_31_) {
    double d = 1.0E-20;
    double[] ds_32_ = new double[i];
    int nswaps = 0;
    for (int i_33_ = 0; i_33_ < i; i_33_++) {
      double d_34_ = Math.abs(ds[i_33_][i_33_]);
      int i_35_ = i_33_;
      for (int i_36_ = i_33_ + 1; i_36_ < i; i_36_++) {
        if (d_34_ < Math.abs(ds[i_36_][i_33_])) {
          d_34_ = Math.abs(ds[i_36_][i_33_]);
          i_35_ = i_36_;
        }
      }
      if (d_34_ < d)
        return -nswaps - 1;
      if (i_35_ > i_33_) {
        nswaps++;
        swaplines(ds, i_35_, i_33_, i);
        swaplines(ds_30_, i_35_, i_33_, i_31_);
      }
      for (int i_37_ = i_33_ + 1; i_37_ < i; i_37_++) {
        double d_38_ = ds[i_37_][i_33_] / ds[i_33_][i_33_];
        for (int i_39_ = i_33_; i_39_ < i; i_39_++)
          ds[i_37_][i_39_] -= ds[i_33_][i_39_] * d_38_;
        for (int i_40_ = 0; i_40_ < i_31_; i_40_++)
          ds_30_[i_37_][i_40_] -= ds_30_[i_33_][i_40_] * d_38_;
      }
    }
    for (int i_41_ = 0; i_41_ < i_31_; i_41_++) {
      for (int i_42_ = i - 1; i_42_ >= 0; i_42_--) {
        ds_32_[i_42_] = ds_30_[i_42_][i_41_];
        for (int i_43_ = i_42_ + 1; i_43_ < i; i_43_++)
          ds_32_[i_42_] -= ds[i_42_][i_43_] * ds_32_[i_43_];
        ds_32_[i_42_] /= ds[i_42_][i_42_];
      }
      for (int i_44_ = 0; i_44_ < i; i_44_++)
        ds_30_[i_44_][i_41_] = ds_32_[i_44_];
    }
    return nswaps;
  }

  public static void zero_mat(double[][] ds, int i, int i_45_) {
    double d = 0.0;
    for (int i_46_ = 0; i_46_ < i; i_46_++) {
      for (int i_47_ = 0; i_47_ < i_45_; i_47_++)
        ds[i_46_][i_47_] = d;
    }
  }

  public static int getNextPowerof2(int number) {
    int result = 2;
    while (result < number)
      result *= 2;
    return result;
  }

  public static double[][] MatProduct(double[][] a, double[][] b, int row, int col, int rowcol) {
    double[][] res = new double[row][col];
    for (int i = 0; i < row; i++)
      for (int j = 0; j < col; j++)
        for (int ij = 0; ij < rowcol; ij++)
          res[i][j] += a[i][ij] * b[ij][j];
    return res;
  }

  public static double[][] MatProduct(double[][] a, double[][] b) {
    return MatProduct(a, b, a.length, b[0].length, a[0].length);
  }

  public static double r_sign(double a, double b) {
    double x;
    x = (a >= 0 ? a : -a);
    return (b >= 0 ? x : -x);
  }

  /**
   * **********************************************************************
   * Computes the prime factorization of N using brute force.
   * <p/>
   * <p/>
   * Remarks
   * -------
   * - Tests i <= N / i instead of i <= N for efficiency.
   * <p/>
   * - Tests i <= N / i instead of i * i <= N  to stave off overflow
   * and enable us to factor 9201111169755555649; otherwise program
   * goes into infinite loop. However, i * i <= N is almost twice
   * as fast on some systems.
   * <p/>
   * ***********************************************************************
   */

  public static long[] getPrimeFactors(long N) {
    Vector result = new Vector(10, 10);
    for (long i = 2; i <= N / i; i++) {
      while (N % i == 0) {
        result.add(new Long(i));
        N = N / i;
      }
    }

    // if biggest factor occurs only once, N > 1
    if (N > 1)
      result.add(new Long(N));
    long[] res = new long[result.size()];
    for (int i = 0; i < result.size(); i++)
      res[i] = ((Long) result.elementAt(i)).longValue();
    return res;
  }

  // integer version (quicker for most cases)
  public static final int[] getPrimeFactors(int N) {
    Vector result = new Vector(10, 10);
    for (int i = 2; i * i <= N; i++) {
      while (N % i == 0) {
        result.add(new Integer(i));
        N = N / i;
      }
    }
    if (N > 1)
      result.add(new Integer(N));
    int[] res = new int[result.size()];
    for (int i = 0; i < result.size(); i++)
      res[i] = ((Integer) result.elementAt(i)).intValue();
    return res;
  }

  // integer version (quicker for most cases)
  public static final int[] getDividers(int N) {
    Vector result = new Vector(10, 10);
    for (int i = 1; i <= N; i++) {
      if (N % i == 0) {
        result.add(new Integer(i));
      }
    }
    int[] res = new int[result.size()];
    for (int i = 0; i < result.size(); i++)
      res[i] = ((Integer) result.elementAt(i)).intValue();
    return res;
  }

  public static final int[][] get3factors(int N) {
    int[][] res = null;

    int[] primes = getDividers(N);

    Vector result = new Vector(10, 10);
    int[] pres = new int[3];
    for (int i = 0; i < primes.length; i++) {
      int Nprime = N / primes[i];
      int[] seconds = getDividers(Nprime);
      for (int j = 0; j < seconds.length; j++) {
        pres[0] = primes[i];
        pres[1] = seconds[j];
        pres[2] = Nprime / seconds[j];
        order(pres);
        boolean alreadyPresent = false;
        for (int h = 0; h < result.size(); h++) {
          int[] tmp = (int[]) result.elementAt(h);
          if (tmp[0] == pres[0] && tmp[1] == pres[1] && tmp[2] == pres[2]) {
            alreadyPresent = true;
            break;
          }
        }
        if (!alreadyPresent)
          result.add(pres.clone());
      }
    }
    int total = result.size();
    res = new int[3][total];
    for (int i = 0; i < total; i++) {
      int[] tmp = (int[]) result.elementAt(i);
      for (int j = 0; j < 3; j++)
        res[j][i] = tmp[j];
    }
    result = null;
    return res;
  }

  public static final void order(int[] seq) {
    if (seq[0] <= seq[1] && seq[1] <= seq[2])
      return;
    if (seq[0] > seq[1]) {
      int tmp = seq[0];
      seq[0] = seq[1];
      seq[1] = tmp;
    }
    if (seq[1] > seq[2]) {
      int tmp = seq[1];
      seq[1] = seq[2];
      seq[2] = tmp;
      if (seq[0] > seq[1]) {
        tmp = seq[0];
        seq[0] = seq[1];
        seq[1] = tmp;
      }
    }
  }

  public static final int[][] get2factors(int N) {
    int[][] res = null;

    int[] primes = getPrimeFactors(N);
    switch (primes.length) {
      case 0:
        // error
        break;
      case 1:
        res = new int[2][1];
        res[0][0] = 1;
        res[1][0] = primes[0];
        break;
      case 2:
        res = new int[2][2];
        res[0][0] = 1;
        res[1][0] = N;
        res[0][1] = primes[0];
        res[1][1] = primes[1];
        break;
      default: {
        int np = primes.length;
        Vector result = new Vector(10, 10);
        int[] pres = new int[2];
        pres[0] = 1;
        pres[1] = N;
        result.add(pres.clone());

        int half = np / 2;
        int total = 0;
        for (int i = 0; i < half; i++)
          for (int j = 1; j <= np - i; j++)
            total += j;
        int[] results = new int[total];
        int tres = 1;
        int index = 0;
        int recIndex = 0;
        for (int i = 0; i < np; i++) {
          addResult2(i, primes, tres, results, index, recIndex);
        }
        for (int i = 0; i < total; i++) {
          pres[0] = results[i];
          pres[1] = N / pres[0];
          order2(pres);
          boolean alreadyPresent = false;
          for (int j = 0; j < result.size(); j++) {
            int[] tmp = (int[]) result.elementAt(j);
            if (tmp[0] == pres[0] && tmp[1] == pres[1]) {
              alreadyPresent = true;
              break;
            }
          }
          if (!alreadyPresent)
            result.add(pres.clone());
        }
        total = result.size();
        res = new int[2][total];
        for (int i = 0; i < total; i++) {
          int[] tmp = (int[]) result.elementAt(i);
          for (int j = 0; j < 2; j++)
            res[j][i] = tmp[j];
        }
      }
    }
    return res;
  }

  public static final void addResult2(int n, int[] vect, int res, int[] results, int index,
                                      int recIndex) {
    if (++recIndex > vect.length / 2)
      return;
    res *= vect[n];
    results[index++] = res;
    for (int i = n + 1; i < vect.length; i++) {
      addResult2(i, vect, res, results, index, recIndex);
    }
  }

  public static final void order2(int[] seq) {
    if (seq[0] <= seq[1])
      return;
    else {
      int tmp = seq[0];
      seq[0] = seq[1];
      seq[1] = tmp;
    }
  }

  public static double[] directionCosines(double theta, double phi) {
    // in radiant
    double[] cosine = new double[3];
    double sintheta = Math.sin(theta);
    cosine[0] = sintheta * Math.cos(phi);
    cosine[1] = sintheta * Math.sin(phi);
    cosine[2] = Math.cos(theta);
    return cosine;
  }

  public static double getDirectionalProperty(double[][] tensor, double[] directionCosines) {
    double result = 0.0;
    int length = tensor.length;
    for (int i = 0; i < length; i++)
      for (int j = 0; j < length; j++)
        result += tensor[i][j] * directionCosines[i] * directionCosines[j];
    return result;
  }

  public static double getDirectionalProperty(double[][][] tensor, double[] directionCosines) {
    double result = 0.0;
    int length = tensor.length;
    for (int i = 0; i < length; i++)
      for (int j = 0; j < length; j++)
        for (int k = 0; k < length; k++)
          result += tensor[i][j][k] * directionCosines[i] * directionCosines[j] * directionCosines[k];
    return result;
  }

  public static double getDirectionalProperty(double[][][][] tensor, double[] directionCosines) {
    double result = 0.0;
    int length = tensor.length;
    for (int i = 0; i < length; i++)
      for (int j = 0; j < length; j++)
        for (int k = 0; k < length; k++)
          for (int l = 0; l < length; l++)
            result += tensor[i][j][k][l] * directionCosines[i] * directionCosines[j] * directionCosines[k] *
                directionCosines[l];
    return result;
  }

  public static double[][][][] getFull4RankTensor(double[][] tensor6x6) {
    int length = 3;
    double[][][][] tensor4Rank = new double[length][length][length][length];
    int[][] i1 = new int[length][length];
    i1[0][0] = 0;
    i1[1][1] = 1;
    i1[2][2] = 2;
    i1[1][2] = i1[2][1] = 3;
    i1[2][0] = i1[0][2] = 4;
    i1[0][1] = i1[1][0] = 5;
    for (int i = 0; i < length; i++)
      for (int j = 0; j < length; j++)
        for (int k = 0; k < length; k++)
          for (int l = 0; l < length; l++)
            tensor4Rank[i][j][k][l] = tensor6x6[i1[i][j]][i1[k][l]];
    return tensor4Rank;
  }

  public static double getDirectionalProperty4Rank(double[][] tensor6x6, double[] directionCosines) {
    return getDirectionalProperty(getFull4RankTensor(tensor6x6), directionCosines);
  }

  public static final double euler_mascheroni = 0.57721566490153286060651209;

  /**
   * Return the exponential integral function.
   */
  public static double Ei(double rPart) {
    double clnx, temx, subx, ratx, addx, modulus, sumx, mod_r, mod2;
    double ans;
    int iter;
    modulus = Math.abs(rPart);
    if (modulus < 4 || rPart < 0) {
      iter = 10 + 4 * (int) Math.round(modulus);
      clnx = 0.5 * Math.log(rPart * rPart);
      int iter1 = iter + 1;
      temx = (double) iter / (iter1 * iter1);
      temx = rPart * temx;
      sumx = temx;
      for (int j = iter; iter >= 2; j--) {
        double t = 1. / j;
        subx = 1. - sumx;
        t = (t + 1) * (1 - t * t);
        temx *= t;
        sumx = temx * subx;
      }
      temx = 1.0 - sumx;
      sumx = temx * rPart;
      sumx = -euler_mascheroni - clnx + sumx;
      mod_r = 0.0;
      if (rPart > -75)
        mod_r = Math.exp(rPart);
      ans = sumx * mod_r;
    } else {
      if (rPart < 0)
        modulus = Math.sqrt((rPart + 29) * (rPart + 29) / 9);
      iter = 4 + (int) Math.round(128.0 / modulus);
      temx = iter;
      addx = temx;
      for (int i = 1; i < iter; i++) {
        temx = temx - 1.;
        sumx = rPart + addx;
        mod2 = sumx * sumx;
        ratx = temx * sumx / mod2;
        ratx = ratx + 1;
        addx = temx * ratx / mod2;
      }
      rPart = rPart + addx;
      mod2 = rPart * rPart;
      ans = rPart / mod2;
    }
    return ans;
  }

	/**
	 * Return the real and imaginary part of the function expEi(q)
	 * where expEi(q) is the exponential integral function. From Wolfram.
	 */
	public static double[] iExpEI(double rPart, double iPart) {
		double clnx, clny, temx, temy, subx, suby, ratx, raty, addx, addy,
				modulus, sumx, sumy, mod_r, mod_i, mod2;
		double[] ans = new double[2];
		int iter;
		double iPartCh = -1;
		if (rPart < 0)
			iPartCh = 0.125 * rPart + 4.;
		modulus = Math.sqrt(rPart * rPart + iPart * iPart);
		if (modulus < 4 || Math.abs(iPart) < iPartCh) {
			iter = 10 + 4 * (int) Math.round(modulus);
			clnx = 0.5 * Math.log(rPart * rPart + iPart * iPart);
			clny = Math.atan2(iPart, rPart);
			int iter1 = iter + 1;
			temx = (double) iter / (iter1 * iter1);
			temy = iPart * temx;
			temx = rPart * temx;
			sumx = temx;
			sumy = temy;
			for (int j = iter; iter >= 2; j--) {
				double t = 1. / j;
				subx = 1. - sumx;
				suby = -sumy;
				t = (t + 1) * (1 - t * t);
				temx *= t;
				temy *= t;
				sumx = temx * subx - temy * suby;
				sumy = temx * suby + temy * subx;
			}
			temx = 1.0 - sumx;
			temy = -sumy;
			sumx = temx * rPart - temy * iPart;
			sumy = temx * iPart + temy * rPart;
			sumx = -euler_mascheroni - clnx + sumx;
			sumy = -clny + sumy;
			mod_r = 0.0;
			if (rPart > -75)
				mod_r = Math.exp(rPart);
			mod_i = mod_r * Math.sin(iPart);
			mod_r = mod_r * Math.cos(iPart);
			ans[0] = sumx * mod_r - sumy * mod_i;
			ans[1] = sumx * mod_i + sumy * mod_r;
		} else {
			if (rPart < 0)
				modulus = Math.sqrt((rPart + 29) * (rPart + 29) / 9 + iPart * iPart);
			iter = 4 + (int) Math.round(128.0 / modulus);
			temx = iter;
			addx = temx;
			addy = 0.0;
			for (int i = 1; i < iter; i++) {
				temx = temx - 1.;
				sumx = rPart + addx;
				sumy = iPart + addy;
				mod2 = sumx * sumx + sumy * sumy;
				ratx = temx * sumx / mod2;
				raty = -temx * sumy / mod2;
				ratx = ratx + 1;
				mod2 = ratx * ratx + raty * raty;
				addx = temx * ratx / mod2;
				addy = -temx * raty / mod2;
			}
			rPart = rPart + addx;
			iPart = iPart + addy;
			mod2 = rPart * rPart + iPart * rPart;
			ans[0] = rPart / mod2;
			ans[1] = -iPart / mod2;
		}

		return ans;
	}

	public static double splint(double[] xa, double[] ya, double[] y2a, double x) {
      int klo,khi,k;
      double h,b,a;
      klo=0;
      khi=xa.length-1;
      while(khi > klo + 1){
          k=(khi+klo)/2;
          if(xa[k]>x)
            khi=k;
          else
            klo=k;
      }
      h=xa[khi]-xa[klo];
      a=(xa[khi]-x)/h;
      b=(x-xa[klo])/h;
      return a*ya[klo]+b*ya[khi]+(Math.pow(a,3)-a)*y2a[klo]+(Math.pow(b,3)-b)*y2a[khi]*h*h/6;
  }

  public static double positive_or_zero(float v) {
    if (v > 0)
      return v;
    return 0;
  }

	public static double getEbelLogarithmicInterpolation(double[] coefficients, double energyInKeV) {
		double result = 0.0;
		for (int i = 0; i < coefficients.length; i++) {
			result += coefficients[i] * MoreMath.pow(Math.log(energyInKeV), i);
//			System.out.print(coefficients[i] + " " + result + " ");
		}
//		System.out.println(Math.exp(result));
		return Math.exp(result);
	}


	public static double getAngleBetweenPoints(double[] A, double[] B, double[] C) {

		double[] v1 = {A[0] - B[0], A[1] - B[1], A[2] - B[2]};
		double[] v2 = {C[0] - B[0], C[1] - B[1], C[2] - B[2]};

		double v1mag = Math.sqrt(v1[0] * v1[0] + v1[1] * v1[1] + v1[2] * v1[2]);
		double[] v1norm = {v1[0] / v1mag, v1[1] / v1mag, v1[2] / v1mag};

		double v2mag = Math.sqrt(v2[0] * v2[0] + v2[1] * v2[1] + v2[2] * v2[2]);
		double[] v2norm = {v2[0] / v2mag, v2[1] / v2mag, v2[2] / v2mag};

		double res = v1norm[0] * v2norm[0] + v1norm[1] * v2norm[1] + v1norm[2] * v2norm[2];

		return Math.acos(res);
	}

	public static double getAngleBetweenPoints(double[] A, double[] C) {

		double v1mag = Math.sqrt(A[0] * A[0] + A[1] * A[1] + A[2] * A[2]);
		double[] v1norm = {A[0] / v1mag, A[1] / v1mag, A[2] / v1mag};

		double v2mag = Math.sqrt(C[0] * C[0] + C[1] * C[1] + C[2] * C[2]);
		double[] v2norm = {C[0] / v2mag, C[1] / v2mag, C[2] / v2mag};

		double res = v1norm[0] * v2norm[0] + v1norm[1] * v2norm[1] + v1norm[2] * v2norm[2];

		return Math.acos(res);
	}

	public static double[] abs(double[] value) {
		double[] absValue = new double[value.length];
		for (int i = 0; i < value.length; i++)
			absValue[i] = Math.abs(value[i]);
		return absValue;
	}

	public static double[][] diagonalMatrix(double[][] p) {
		EigenDecomposition ev = new EigenDecomposition(new Array2DRowRealMatrix(p, false));
		return ev.getD().getData();
	}


}
