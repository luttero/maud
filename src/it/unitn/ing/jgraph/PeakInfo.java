package it.unitn.ing.jgraph;

public class PeakInfo {
	public String info;
	public double coordinate;
	public double diff;

	public double calculateDiff(double center) {
		diff = Math.abs(center - coordinate);
		return diff;
	}
}
