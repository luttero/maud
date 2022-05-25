package it.unitn.ing.wizard.LCLS2Wizard;

import ij.ImagePlus;
import ij.process.FloatProcessor;
import ij.process.ShortProcessor;

public class SensorImage {
	public String name = "";

	public boolean enabled = true;
	public boolean available = false;

	public int width = 0;
	public int height = 0;

	public double pixelWidth = 0.10992;
	public double pixelHeight = 0.10992;
	public int originX = 0;
	public int originY = 0;
	public double theta2;
	public double distance;
	public double etaDA;
	public double omegaDN;
	public double phiDA;
	public double centerX;
	public double centerY;

	public double[][] photons = null;

	public SensorImage(int swidth, int sheight) {
		width = swidth;
		height = sheight;
		photons = new double[height][width];
	}

	public SensorImage(double[][] phot) {
		setPhotons(phot);
	}

	public void setPhotons(double[][] phot) {
		if (phot != null && phot.length > 0) {
			width = phot[0].length;
			height = phot.length;
			photons = new double[height][width];
			for (int i = 0 ; i < height; i++)
				for (int j = 0; j < width; j++)
					photons[i][j] = phot[i][j];
			available = true;
		}
	}

	public void removeDarkCurrentWithGap(SensorImage image, int gapx, int gapy) {
		removeDarkCurrentWithGap(image.photons, gapx, gapy);
	}

	public void removeDarkCurrent(SensorImage image, int shiftx, int shifty) {
		removeDarkCurrent(image.photons, shiftx, shifty);
	}

	public void removeDarkCurrentWithGap(double[][] darkCurrent, int gapx, int gapy) {
		if (darkCurrent != null && photons != null) {
			if ((width > 0 && height > 0) && (darkCurrent.length >= height - gapy && darkCurrent[0].length >= width - gapx)) {
				int halfx = width / 2 - gapx / 2;
				int halfy = height / 2 - gapy / 2;
				double sumNegative = 0;
				int negativeCount = 0;
				double minPositive = 1.0E10;
				int addy = 0;
				for (int i = 0; i < height - gapy; i++) {
					if (i == halfy)
						addy = gapy;
					int addx = 0;
					for (int j = 0; j < width - gapx; j++) {
						if (j == halfx)
							addx = gapx;
						if (photons[i + addy][j + addx] >= 0) {   // only if these are valid pixels from the beginning
							double value = photons[i + addy][j + addx] - darkCurrent[i][j];
							if (value < 0) {
								sumNegative += -value;
								negativeCount++;
								minPositive = 0;
							} else if (minPositive > value)
								minPositive = value;
						}
					}
				}
				double meanNegative = sumNegative / negativeCount;
				addy = 0;
				for (int i = 0; i < height - gapy; i++) {
					if (i == halfy)
						addy = gapy;
					int addx = 0;
					for (int j = 0; j < width - gapx; j++) {
						if (j == halfx)
							addx = gapx;
						if (photons[i + addy][j + addx] >= 0) {   // only if these are valid pixels from the beginning
							photons[i + addy][j + addx] -= darkCurrent[i][j] - meanNegative + minPositive;
							if (photons[i + addy][j + addx] < 0) {
								photons[i + addy][j + addx] = 0;
							}
						}
					}
				}
				// we remove the gap in between the two sensor + 2 additional lines of pixels (the larger pixels)
				if (gapy > 0) {
					gapy += 2;
					halfy = height / 2 - gapy / 2;
					for (int i = halfy; i < halfy + gapy; i++) {
						for (int j = 0; j < width; j++) {
							photons[i][j] = -999;
						}
					}
				}
				if (gapx > 0) {
					gapx += 2;
					halfx = width / 2 - gapx / 2;
					for (int i = 0; i < height; i++) {
						for (int j = halfx; j < halfx + gapx; j++) {
							photons[i][j] = -999;
						}
					}
				}
			} else {
				System.out.println("WARNING: no dark current correction as dimesnions don't match:");
				System.out.println("Sensor: " + width + "x" + height + ", Dark current: " +
						darkCurrent[0].length + "x" + darkCurrent.length + ", Gaps: " + gapx + " & " + gapy);
			}
		}
	}

	public void removeDarkCurrent(double[][] darkCurrent, int shiftx, int shifty) {
		if (darkCurrent != null && photons != null) {
			if ((width > 0 && height > 0) && (darkCurrent.length >= height && darkCurrent[0].length >= width)) {
				for (int i = 0; i < height; i++)
					for (int j = 0; j < width; j++) {
						if (photons[i][j] >= 0) {   // only if these are valid pixels from the beginning
							photons[i][j] -= darkCurrent[i + shiftx][j + shifty];
							if (photons[i][j] < 0)
								photons[i][j] = 0;
						}
					}
			} else {
				System.out.println("WARNING: no dark current correction as dimesnions don't match:");
				System.out.println("Sensor: " + width + "x" + height + ", Dark current: " +
						darkCurrent[0].length + "x" + darkCurrent.length + ", Shifts: " + shiftx + " & " + shifty);
			}
		}
	}

	public void setPixelsStatus(int[][] status) {
		if (status != null && photons != null) {
			if ((width > 0 && height > 0) && (status.length == height && status[0].length == width)) {
				for (int i = 0; i < height; i++)
					for (int j = 0; j < width; j++) {
						if (status[i][j] != 0)
							photons[i][j] = -1; // then it will not be integrated or used
					}
			}
		}
	}

	public void rotateCW(LCLSDetectorType type) {
		double[][] newPhotons = new double[width][height];
		int maxRow = height - 1;
		for (int i = 0; i < height; i++)
			for (int j = 0; j < width; j++)
				newPhotons[j][maxRow - i] = photons[i][j];

		photons = newPhotons;
		height = width;
		width = maxRow + 1;

		double temp = pixelHeight;
		pixelHeight = pixelWidth;
		pixelWidth = temp;

		int temp1 = originY;
		originY = -originX;
		originX = width + temp1 - type.height;
	}

	public void rotateCCW(LCLSDetectorType type) {
		double[][] newPhotons = new double[width][height];
		int maxCol = width - 1;
		for (int i = 0; i < height; i++)
			for (int j = 0; j < width; j++)
				newPhotons[maxCol - j][i] = photons[i][j];

		photons = newPhotons;
		width = height;
		height = maxCol + 1;

		double temp = pixelHeight;
		pixelHeight = pixelWidth;
		pixelWidth = temp;

		int temp1 = originY;
		originY = height + originX - type.width;
		originX = -temp1;
	}

	public void rotate180(LCLSDetectorType type) {
		double[][] newPhotons = new double[width][height];
		int maxRow = height - 1;
		for (int i = 0; i < height; i++)
			for (int j = 0; j < width; j++)
				newPhotons[j][maxRow - i] = photons[i][j];
		maxRow = width - 1;
		for (int i = 0; i < width; i++)
			for (int j = 0; j < height; j++)
				photons[j][maxRow - i] = newPhotons[i][j];

		originX = width + originX - type.width;
		originY = height + originY - type.height;
	}

	public ImagePlus getImagePlus() {
		FloatProcessor fp = new FloatProcessor(width, height);
		for (int i = 0; i < height; i++)
			for (int j = 0; j < width; j++)
				fp.putPixelValue(j, i, photons[i][j]);
		ImagePlus imp = new ImagePlus(name, fp);
		ij.measure.Calibration cal = imp.getCalibration();
		cal.setUnit("mm");
		cal.pixelWidth = pixelWidth;
		cal.pixelHeight = pixelHeight;
		return imp;
	}
}
