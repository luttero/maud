package it.unitn.ing.rista.util;

import java.util.Vector;

/**
 * Created by luca on 15/03/2017.
 */
public class SavitzkyGolay {

	public SavitzkyGolay() {
	}

	static int genFact(int a, int b) {
		// Calculates the generalised factorial(a)(a-1)...(a-b+1)

		int gf = 1;
		for (int j = a - b + 1; j <= a; j++)
			gf = gf * j;
		return gf;
	}

	static double gramPoly(int i, int m, int k, int s) {
		// Calculates the Gram Polynomial (s=0), or its s'th
		// derivative evaluated at i, order k, over 2m+1 points
		double gramPol;
		if (k > 0) {
			gramPol = ((double) (4*k-2)) / ((double) (k*(2*m-k+1))) *
					(((double) (i)) * gramPoly(i, m, k-1, s) +
							((double) (s)) * gramPoly(i, m, k-1, s-1)) -
					((double) ((k-1)*(2*m+k))) / ((double) (k*(2*m-k+1))) *
					gramPoly(i, m, k-2, s);
		} else {
			if ((k == 0) && (s == 0)) {
				gramPol = 1.0;
			} else {
				gramPol = 0.0;
			}
		}
		return gramPol;
	}

	static double weight(int i, int t, int m, int n, int s) {
		// Calculates the weight of the i'th data point for the t'th Least-Square
		// point of the s'th derivative, over 2m+1 points, order n.
		// i, t from -m to m, t != 0 when evaluating initial or final points
		double w = 0;
		for (int k = 0; k <= n; k++) {
			w += ((double) ((2*k+1) * genFact(2*m, k))) / ((double) genFact(2*m+k+1, k+1)) *
			gramPoly(i, m, k, 0) * gramPoly(t, m, k, s);
		}
		return w;
	}

	static double weight0(int i, int m, int n, int s) {
		// Calculates the weight of the i'th data point
		// of the s'th derivative, over 2m+1 points, order n.
		// i, t from -m to m, t != 0 when evaluating initial or final points
		double w = 0;
		for (int k = 0; k <= n; k++) {
			w += gramPoly(i, m, k, 0) * gramPoly(0, m, k, s) * (2*k+1) *
					genFact(2*m, k) / genFact(2*m+k+1, k+1);
		}
		return w;
	}

	public static int maxSGIndexM = MaudPreferences.getInteger("Savitzky-Golay.maximumM", 15);
	public static int orderSG = MaudPreferences.getInteger("Savitzky-Golay.order", 2);
	public static int initialM = MaudPreferences.getInteger("Savitzky-Golay.initialM", 3) - 1;

	static Vector<double[][]> smoothingCoeff = new Vector<>();
	static Vector<double[][]> firstDerivCoeff = new Vector<>();
	static Vector<double[][]> secondDerivCoeff = new Vector<>();

	static void initSavitzkyGolay(int order, int maxM) {
		if (!smoothingCoeff.isEmpty() && !firstDerivCoeff.isEmpty() && !secondDerivCoeff.isEmpty())
			return;                      // already initialized

		lastMax = maxM;
		lastOrder = order;
		smoothingCoeff.clear();
		firstDerivCoeff.clear();
		secondDerivCoeff.clear();

		for (int i = 0; i < maxM; i++) {
			int mm = i + 1;
			int mMax = mm * 2 + 1;
			double[][] smoothingCoeffSG = new double[mMax][mMax];
			double[][] firstDerivCoeffSG = new double[mMax][mMax];
			double[][] secondDerivCoeffSG = new double[mMax][mMax];
			for (int j = 0; j < mMax; j++) {
				for (int m = 0; m < mMax; m++) {
					smoothingCoeffSG[j][m] = weight(m - mm, j - mm, mm, order, 0); // / mMax;
					firstDerivCoeffSG[j][m] = weight(m - mm, j - mm, mm, order, 1); // / mMax;
					secondDerivCoeffSG[j][m] = weight(m - mm, j - mm, mm, order, 2); // / mMax;
				}
			}
			smoothingCoeff.add(smoothingCoeffSG);
			firstDerivCoeff.add(firstDerivCoeffSG);
			secondDerivCoeff.add(secondDerivCoeffSG);
		}

	}

// smoothing methods

	static int lastOrder = 0;
	static int lastMax = 0;

	private static double[] smoothPattern(double[] dta, int[] indexSG) {
		maxSGIndexM = MaudPreferences.getInteger("Savitzky-Golay.maximumM", maxSGIndexM);
		orderSG = MaudPreferences.getInteger("Savitzky-Golay.order", orderSG);
		if (smoothingCoeff.isEmpty() || lastOrder != orderSG || lastMax != maxSGIndexM) {
			smoothingCoeff.clear();
			initSavitzkyGolay(orderSG, maxSGIndexM);
		}

//		System.out.println("Smoothing Savitzky-Golay, m = " + );

		int dataNumber = dta.length;
		double[] smoothP = new double[dataNumber];
		for (int i = 0; i < dataNumber; i++) {
			smoothP[i] = 0;                                              // example:
			int m0 = indexSG[i];                                         // index = 2, m0 = 2
			int mm = m0 + 1;                                             // mm = 3
			int mMax = mm * 2 + 1;                                       // mMax = 7 points
			// indexSG[i] == 2 corresponds to -3 <= m <= 3               // -3 < m < 3

			double[][] sg = smoothingCoeff.elementAt(m0);

			if (i < mm) {                                        //  mm = 3, i = 2
				for (int m = 0; m < mMax; m++) {                   //
					smoothP[i] += sg[i][m] * dta[m];                 // sg[2][m] * dta[m]
				}
			} else if (i >= dataNumber - mm) {                   // i - (dataNumber - mm - 1) = i - dataNumber + mm + 1
				for (int m = 0; m < mMax; m++) {
					smoothP[i] += sg[2 * mm + i - dataNumber + 1][m] * dta[m - 2 * mm - 1 + dataNumber];
				}
			} else {
				for (int m = 0; m < mMax; m++) {
					smoothP[i] += sg[mm][m] * dta[i + m - mm];
				}
			}
		}
		return smoothP;
	}

	private static double[] firstDerivativePattern(double[] dta, int[] indexSG) {
		maxSGIndexM = MaudPreferences.getInteger("Savitzky-Golay.maximumM", maxSGIndexM);
		orderSG = MaudPreferences.getInteger("Savitzky-Golay.order", orderSG);
		if (smoothingCoeff.isEmpty() || lastOrder != orderSG || lastMax != maxSGIndexM) {
			smoothingCoeff.clear();
			initSavitzkyGolay(orderSG, maxSGIndexM);
		}

//		System.out.println("Smoothing Savitzky-Golay, m = " + );

		int dataNumber = dta.length;
		double[] smoothP = new double[dataNumber];
		for (int i = 0; i < dataNumber; i++) {
			smoothP[i] = 0;                                              // example:
			int m0 = indexSG[i];                                         // index = 2, m0 = 2
			int mm = m0 + 1;                                             // mm = 3
			int mMax = mm * 2 + 1;                                       // mMax = 7 points
			// indexSG[i] == 2 corresponds to -3 <= m <= 3               // -3 < m < 3

			double[][] sg = firstDerivCoeff.elementAt(m0);

			if (i < mm) {                                        //  mm = 3, i = 2
				for (int m = 0; m < mMax; m++) {                   //
					smoothP[i] += sg[i][m] * dta[m];                 // sg[2][m] * dta[m]
				}
			} else if (i >= dataNumber - mm) {                   // i - (dataNumber - mm - 1) = i - dataNumber + mm + 1
				for (int m = 0; m < mMax; m++) {
					smoothP[i] += sg[mm + i - dataNumber + mm + 1][m] * dta[i + m - mm - i - mm - 1 + dataNumber];
				}
			} else {
				for (int m = 0; m < mMax; m++) {
					smoothP[i] += sg[mm][m] * dta[i + m - mm];
				}
			}
		}
		return smoothP;
	}

	private static double[] secondDerivativePattern(double[] dta, int[] indexSG) {
		maxSGIndexM = MaudPreferences.getInteger("Savitzky-Golay.maximumM", maxSGIndexM);
		orderSG = MaudPreferences.getInteger("Savitzky-Golay.order", orderSG);
		if (smoothingCoeff.isEmpty() || lastOrder != orderSG || lastMax != maxSGIndexM) {
			smoothingCoeff.clear();
			initSavitzkyGolay(orderSG, maxSGIndexM);
		}

//		System.out.println("Smoothing Savitzky-Golay, m = " + );

		int dataNumber = dta.length;
		double[] smoothP = new double[dataNumber];
		for (int i = 0; i < dataNumber; i++) {
			smoothP[i] = 0;                                              // example:
			int m0 = indexSG[i];                                         // index = 2, m0 = 2
			int mm = m0 + 1;                                             // mm = 3
			int mMax = mm * 2 + 1;                                       // mMax = 7 points
			// indexSG[i] == 2 corresponds to -3 <= m <= 3               // -3 < m < 3

			double[][] sg = secondDerivCoeff.elementAt(m0);

			if (i < mm) {                                        //  mm = 3, i = 2
				for (int m = 0; m < mMax; m++) {                   //
					smoothP[i] += sg[i][m] * dta[m];                 // sg[2][m] * dta[m]
				}
			} else if (i >= dataNumber - mm) {                   // i - (dataNumber - mm - 1) = i - dataNumber + mm + 1
				for (int m = 0; m < mMax; m++) {
					smoothP[i] += sg[mm + i - dataNumber + mm + 1][m] * dta[i + m - mm - i - mm - 1 + dataNumber];
				}
			} else {
				for (int m = 0; m < mMax; m++) {
					smoothP[i] += sg[mm][m] * dta[i + m - mm];
				}
			}
		}
		return smoothP;
	}

	private static double[] calcPattern(double[] dta, int startI, int mm) {
		if (smoothingCoeff.isEmpty()) {
			initSavitzkyGolay(orderSG, maxSGIndexM);
		}

		double[][] sg = smoothingCoeff.elementAt(mm - 1);
		int mMax = mm * 2 + 1;                                       // mMax = 7 points
		int dataNumber = dta.length;
		double[] smoothP = new double[mMax];

		for (int i = 0; i < mMax; i++) {
			smoothP[i] = 0;
			int j = startI + i;
			if (j < mm) {                                        //  mm = 3, i = 2
				for (int m = 0; m < mMax; m++) {                   //
					smoothP[i] += sg[i][m] * dta[m];                 // sg[2][m] * dta[m]
				}
			} else if (j >= dataNumber - mm) {                   // i - (dataNumber - mm - 1) = i - dataNumber + mm + 1
				for (int m = 0; m < mMax; m++) {
					smoothP[i] += sg[mm + j - dataNumber + mm + 1][m] * dta[m - 2 * mm - 1 + dataNumber];
				}
			} else {
				for (int m = 0; m < mMax; m++) {
					smoothP[i] += sg[mm][m] * dta[j + m - mm];
				}
			}
		}
		return smoothP;
	}

	public static int[] optimizeM(double[] dta, double[] wgt, int[] indexSG) {
		int dataNumber = dta.length;
		int[] index = new int[dataNumber];
		int[] checkIndex = new int[dataNumber];
		for (int i = 0; i < dataNumber; i++) {
			index[i] = indexSG[i];
			checkIndex[i] = 0;
		}

		boolean useDurbinWatson = MaudPreferences.getBoolean("Savitzky-Golay.useDurbinWatsonStatistic", true);
		double wddm = MaudPreferences.getDouble("Savitzky-Golay.countingStatisticsFactor", 7.0);
		int useWeights = MaudPreferences.getInteger("Savitzky-Golay.weightsType", 0);
		if (useDurbinWatson)
			wddm = MaudPreferences.getDouble("Savitzky-Golay.DurbinWatsonFactor", -1.4);
		double wddm2 = wddm * wddm;

		for (int i = 0; i < dataNumber; i++) {
			boolean finished = false;
			while (!finished) {
				int m0 = index[i];                                         // index = 2, m0 = 2
				int mm = m0 + 1;                                             // mm = 3
				int mMax = mm * 2 + 1;                                       // mMax = 7 points
				int startI = 0;
				if (i < mm) {                                        //  mm = 3, i = 2
				} else if (i >= dataNumber - mm) {                   // i - (dataNumber - mm - 1) = i - dataNumber + mm + 1
					startI = dataNumber - 2 * mm - 1;
				} else {
					startI = i - mm;
				}

				double[] pSG = calcPattern(dta, startI, mm);

				boolean respected = true;

				if (useDurbinWatson) {
					if (i == dataNumber - 1) {
						index[i] = index[i - 1];
						finished = true;
					} else {
						double sumn = 0;
						double sumd = dta[startI + mMax - 1] - pSG[mMax - 1];
						sumd *= sumd;
						for (int m = 0; m < mMax - 1; m++) {
							double diff = dta[startI + m] - dta[startI + m + 1] + pSG[m + 1] - pSG[m];
							diff *= diff;
							sumn += diff;
							diff = dta[startI + m] - pSG[m];
							diff *= diff;
							sumd += diff;
						}
						sumn /= sumd;
						respected = sumn > 4.0 + 2.0 * wddm + 2.0 / (mm - 1) - 1.0 / mMax;
					}
				} else {
					double sumn = 0;
					for (int m = 0; m < mMax - 1; m++) {
						double diff = dta[startI + m] - pSG[m];
						switch (useWeights) {
							case 0: // no weights
								diff *= diff;
								sumn += diff;
								break;
							case 1: // 1 / SQRT(I)
								diff *= diff;
								sumn += diff * wgt[startI + m];
								break;
							default: { // ddm: 1 / I
								diff *= wgt[startI + m];
								diff *= diff;
								sumn += diff;
							}
						}
					}
					sumn /= 2 * mm + 1;
					respected = sumn < wddm2;
				}
				if (!finished) {
					if (respected) {
						if (checkIndex[i] < 0 || index[i] == maxSGIndexM - 1) {
							finished = true;
						} else {
							checkIndex[i] = 1;
							index[i] += 1;
						}
					} else {
						if (checkIndex[i] > 0) {
							finished = true;
							index[i] -= 1;
						} else {
							if (index[i] > 1) {
								checkIndex[i] = -1;
								index[i] -= 1;
							} else
								finished = true;
						}
					}
				}

			} // end of while loop "finished"
			System.out.println(i + " " + index[i]);
		}
		return index;
	}

	public static double[] smoothPattern(double[] dta, double[] wgt, int[] indexSG, boolean optimizeM) {
		if (optimizeM) {
			int[] index = optimizeM(dta, wgt, indexSG);
			return smoothPattern(dta, index);
		}
		return smoothPattern(dta, indexSG);
	}

	public static double[] firstDerivativePattern(double[] dta, double[] wgt, int[] indexSG, boolean optimizeM) {
		if (optimizeM) {
			int[] index = optimizeM(dta, wgt, indexSG);
			return firstDerivativePattern(dta, index);
		}
		return firstDerivativePattern(dta, indexSG);
	}

	public static double[] secondDerivativePattern(double[] dta, double[] wgt, int[] indexSG, boolean optimizeM) {
		if (optimizeM) {
			int[] index = optimizeM(dta, wgt, indexSG);
			return secondDerivativePattern(dta, index);
		}
		return secondDerivativePattern(dta, indexSG);
	}

}
