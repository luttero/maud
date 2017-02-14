package it.unitn.ing.rista.util;

import java.io.BufferedReader;
import java.util.StringTokenizer;
import java.util.Vector;

public class IntegratedBesselJ0 {

	private static double maxValue = 500;
	public static int numberOfData = 10000;
	static double[] R = null;
	static double[] averageR = null;
	static double stepR = 0;
	private static int numberIntegrationThetaPoints = 100;
	private static int numberIntegrationBesselPoints = 20;
	public static final double PI_4 = Constants.PI / 4;
	public static double[] cosTheta = null;
	static double over3 = 1.0 / 3.0;
	static double over6 = 1.0 / 6.0;

	private IntegratedBesselJ0() {
	}

	public static void init() {
		if (R == null || R.length != numberOfData) {
			R = new double[numberOfData];
			stepR = maxValue / numberOfData;
			calculateR();
		}
		if (averageR == null || averageR.length != numberOfData) {
			averageR = new double[numberOfData];
			stepR = maxValue / numberOfData;
			calculateAverageR();
		}
	}

	private static void calculateCosTheta() {
		cosTheta = new double[numberIntegrationThetaPoints + 1];
		double stepTheta = PI_4 / numberIntegrationThetaPoints;
		for (int i = 0; i <= numberIntegrationThetaPoints; i++)
			cosTheta[i] = 1.0 / Math.cos(stepTheta * i);
	}

	public static void init(double newMaxValue, int newNumberOfData) {
		maxValue = newMaxValue;
		numberOfData = newNumberOfData;
		averageR = null;
		R = null;
		init();
	}

	public static void calculateR() {
		R[0] = 0;
		double lastR = R[0], lastX = 0, newX;
		double doubleStepR = stepR;
		for (int i = 1; i < numberOfData; i++) {
			newX = doubleStepR * i;
			lastR = R[i] = lastR + integrate_J02x(numberIntegrationBesselPoints, lastX, newX);
			lastX = newX;
		}
	}

	public static void calculateAverageR() {
		calculateCosTheta();
		averageR[0] = 1;
		for (int i = 1; i < numberOfData; i++) {
			averageR[i] = averageR(stepR * i);
		}
		cosTheta = null; // we don't need anymore
		R = null;
	}

	public static double averageR(double a) {

		// integration for theta2 from -pi/4 to pi/4
		// as it is costheta2, we need only to do 0 to pi/4

		double h = 1.0 / numberIntegrationThetaPoints;
		double s1 = 0.0;
		double s2 = 0.0;

		for (int i = 1; i < numberIntegrationThetaPoints; i += 2) {
			double s = a * cosTheta[i];
			s1 += rByInterpolation(s) / s;
		}
		for (int i = 2; i < numberIntegrationThetaPoints - 1; i += 2) {
			double s = a * cosTheta[i];
			s2 += rByInterpolation(s) / s;
		}

		double s = a * cosTheta[numberIntegrationThetaPoints];
		return over3 * h * (s1 * 4 + s2 * 2 + rByInterpolation(a) / a +
				rByInterpolation(s) / s);
	}

	public static double integrate_J02x(int n, double a, double b) {
		// Simpson rule, we input 2x, so the output need to be divided for the interval

		a *= 2;
		b *= 2;
		double h = (b - a) / n;
		double s1 = 0.0;
		double s2 = 0.0;

		for (int i = 1; i < n; i += 2) s1 += j0(a + h * i);
		for (int i = 2; i < n - 1; i += 2) s2 += j0(a + h * i);

		return over6 * h * (s1 * 4 + s2 * 2 + j0(a) + j0(b));
	}

	public static double averageIntegralBesselUpTo(double factor) {
		if (averageR == null)
			init();
		return averageRbyInterpolation(factor);
	}

	public static double integralBesselUpTo(double factor) {
		if (R == null)
			init();
		return rByInterpolation(factor);
	}

	private static double averageRbyInterpolation(double factor) {
		double r = 1;
		if (factor > 0) {
			if (factor < maxValue) {
				int index = (int) (factor / stepR);
				double rest = factor - stepR * index;
				if (index == numberOfData - 1) {
					index--;
					rest = stepR - rest;
				}
				r = averageR[index] + rest * (averageR[index + 1] - averageR[index]) / stepR;
			} else
				r = 0.5 / factor;
		}
		return r;
	}

	private static double rByInterpolation(double factor) {
		double r = 0;
		if (factor > 0) {
			if (factor < maxValue) {
				int index = (int) (factor / stepR);
				double rest = factor - stepR * index;
				if (index == numberOfData - 1) {
					index--;
					rest = stepR - rest;
				}
				r = R[index] + rest * (R[index + 1] - R[index]) / stepR;
			} else
				r = 0.5;
		}
		return r;
	}

	/**
	 * Returns the Bessel function of the first kind of order 0 of the argument.
	 * @param x the value to compute the bessel function of.
	 */
	public static double j0(double x) {
		double ax;

		if( (ax=Math.abs(x)) < 8.0 ) {
			double y=x*x;
			double ans1=57568490574.0+y*(-13362590354.0+y*(651619640.7
					+y*(-11214424.18+y*(77392.33017+y*(-184.9052456)))));
			double ans2=57568490411.0+y*(1029532985.0+y*(9494680.718
					+y*(59272.64853+y*(267.8532712+y*1.0))));

			return ans1/ans2;

		}
		else {
			double z=8.0/ax;
			double y=z*z;
			double xx=ax-0.785398164;
			double ans1=1.0+y*(-0.1098628627e-2+y*(0.2734510407e-4
					+y*(-0.2073370639e-5+y*0.2093887211e-6)));
			double ans2 = -0.1562499995e-1+y*(0.1430488765e-3
					+y*(-0.6911147651e-5+y*(0.7621095161e-6
					-y*0.934935152e-7)));

			return Math.sqrt(0.636619772/ax)*
					(Math.cos(xx)*ans1-z*Math.sin(xx)*ans2);
		}
	}

}