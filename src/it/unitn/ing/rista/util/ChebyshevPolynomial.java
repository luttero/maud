/*
 * @(#)ChebyshevPolynomial.java 5/09/1999 Pergine
 *
 * Copyright (c) 1999 Luca Lutterotti All Rights Reserved.
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

/**
 *  The ChebyshevPolynomial is a class providing methods for
 *  Chebyshev polynomial computation.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class ChebyshevPolynomial {

	/*

	First kind

	T0(x) = 1
	T1(x) = x
	Tn+1(x) = 2*x*Tn(x) - Tn-1(x)

	Second kind
	U0(x) = 1
	U1(x) = 2*x
	Un+1(x) = 2*x*Un(x) - Un-1(x)

	*/

	// from cctbx::scitbx

	double low_limit_ = -1;
	double high_limit_ = 1;
	double[] cheb_coefs_ = null;
	double addition = 0.0;
	double difference = 1.0;

  public ChebyshevPolynomial(double lowLimit, double highLimit, double[] chebCoeff) {
	  if (high_limit_ - low_limit_ != 0) {
		  cheb_coefs_ = chebCoeff;
		  low_limit_ = lowLimit;
		  high_limit_ = highLimit;
	  } else
		  cheb_coefs_ = new double[0];
	  addition = (low_limit_ + high_limit_) * 0.5;
	  difference = (high_limit_ - low_limit_) * 0.5;
  }

	public double getTfunction(double x_in) {

		double x = (x_in - addition) / difference;  // transform to get into -1, 1

		double x2 = 2.0 * x;
		double d = 0, dd = 0, sv;

		for (int ii = cheb_coefs_.length - 1; ii >= 1; ii--){
			sv = d;
			d = x2 * d - dd + cheb_coefs_[ii];
			dd = sv;
		}
		return x * d - dd + 0.5 * cheb_coefs_[0];
	}

	// from GSAS, only up to 12, requires already normalized
	// updated to 15

	static final int Cm[][] = {
			{1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			{0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			{-1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			{0, -3, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			{1, 0, -8, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			{0, 5, 0, -20, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			{-1, 0, 18, 0, -48, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0},
			{0, -7, 0, 56, 0, -112, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0},
			{1, 0, -32, 0, 160, 0, -256, 0, 128, 0, 0, 0, 0, 0, 0, 0},
			{0, 9, 0, -120, 0, 432, 0, -576, 0, 256, 0, 0, 0, 0, 0, 0},
			{-1, 0, 50, 0, -400, 0, 1120, 0, -1280, 0, 512, 0, 0, 0, 0, 0},
			{0, -11, 0, 220, 0, -1232, 0, 2816, 0, -2816, 0, 1024, 0, 0, 0, 0},
			{1, 0, -72, 0, 840, 0, -3584, 0, 6912, 0, -6144, 0, 2048, 0, 0, 0},
			{0, 13, 0, -364, 0, 2912, 0, -9984, 0, 16640, 0, -13312, 0, 4096, 0, 0},
			{-1, 0, 98, 0, -1568, 0, 9408, 0, -26880, 0, 39424, 0, -2672, 0, 8192, 0},
			{0, 15, 0, 560, 0, -6048, 0, 28800, 0, -70400, 0, 92160, 0, -61440, 0, 16384}
	};


	public static final double getT(int n, double x) {

		if (n > 15)
			return 0;

		double t = 0.0;

		for (int i = n; i >= 0; i -= 2) {
			t += Cm[n][i] * MoreMath.pow(x, i);
		}

		return t;
	}

}

/*

  template <typename FloatType>
  FloatType
  chebyshev_base<FloatType>
  ::transform(FloatType const& x_in)
  {
    typedef FloatType f_t;
    f_t epsilon;
    epsilon = 1.0E-12;
    f_t result=0;
    if(high_limit_ - low_limit_ != 0) {
      result = (x_in - (low_limit_ + high_limit_)*0.5)
        / (0.5*(high_limit_ - low_limit_));
    }
    SCITBX_ASSERT (result<=1+epsilon);
    SCITBX_ASSERT (result>=-1.0-epsilon);
    return(result);
  }

  template <typename FloatType>
  FloatType
  chebyshev_base<FloatType>
  ::cheb_base_f(FloatType const& x_in)
  {
    typedef FloatType f_t;
    f_t x;
    x = transform(x_in);
    f_t x2 = 2*x;
    f_t result = 0.0;
    f_t d=0, dd=0, sv=0;

    for (int ii=cheb_coefs_.size()-1;ii>=1;ii--){
      sv = d;
      d = x2*d-dd+cheb_coefs_[ii];
      dd = sv;
    }
    result = x*d -dd+0.5*cheb_coefs_[0];
    return (result);
  }


*/
